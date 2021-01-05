

##########################################
# SCORING ON APPLICATION FOR CREDIRECT   #
#  ESTIMATION OF PROBABILITY TO FRAUD    # 
#         LOGISTIC REGRESSION            #
##########################################


#############
# Libraries #
#############
library(smbinning)
library(caTools)
library(ROCR)
library(Hmisc)
library(dummies)
library(plyr)
library(binr)
library(rsq)
library(Information)
library(RMySQL)
library(party)
library(openxlsx)


############
# Set f(x) #
############

# Function to count number of missing values
count_empty <- function(var){
  return(sum(is.na(var)))
}
# Function for calculating the G/B Indexes for each bin 
gb_index_fct <- function(data,var,gb_flag){
  gb_ratio_all <- count(data, gb_flag)[1,2]/count(data, gb_flag)[2,2]
  index <- vector(length = length(unique(var)))
  for (i in 1:length(unique(var))) {
    local <- count(subset(data,var==unique(var)[i]), gb_flag)[1,2]/
      count(subset(data,var==unique(var)[i]), gb_flag)[2,2]
    all <- gb_ratio_all
    index[i] <- ifelse(local>all,paste(round(local/all*100,1),"G",sep=""),
                       paste(round(all/local*100,1),"B",sep=""))
  }
  index <- as.data.frame(t(index))
  for (i in 1:length(unique(var))) {
    names(index)[i] <- paste(unique(var)[i])
  }
  return (index[ , order(names(index))])
}
# Function for taking only complete rows 
complete_fct <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
# Function for calculating Kolmogorov-Smirnoff coefficient
ks <- function(def, def_test){
  df_train <- def
  df_test <- def_test
  df_ks <- data.frame(t(table(df_train$default_flag,df_train$Score))[,1])
  names(df_ks)[1] <- "True_Goods_Train"
  df_ks$True_Bads_Train <-t(table(df_train$default_flag,df_train$Score))[,2]
  if(!is.na(df_test)){
  df_ks$True_Goods_Test <- t(table(df_test$default_flag,df_test$Score))[,1]
  df_ks$True_Bads_Test <- t(table(df_test$default_flag,df_test$Score))[,2]}
  
  df_ks$Cumulative_Goods_Train[1] <- df_ks$True_Goods_Train[1]
  df_ks$Cumulative_Bads_Train[1] <- df_ks$True_Bads_Train[1]
  if(!is.na(df_test)){
  df_ks$Cumulative_Goods_Test[1] <- df_ks$True_Goods_Test[1]
  df_ks$Cumulative_Bads_Test[1] <- df_ks$True_Bads_Test[1]}
  for (i in (2:nrow(df_ks))) {
    df_ks$Cumulative_Goods_Train[i]=df_ks$Cumulative_Goods_Train[i-1]+
      df_ks$True_Goods_Train[i]
    df_ks$Cumulative_Bads_Train[i]=df_ks$Cumulative_Bads_Train[i-1]+
      df_ks$True_Bads_Train[i]
    if(!is.na(df_test)){
    df_ks$Cumulative_Goods_Test[i]=df_ks$Cumulative_Goods_Test[i-1]+
      df_ks$True_Goods_Test[i]
    df_ks$Cumulative_Bads_Test[i]=df_ks$Cumulative_Bads_Test[i-1]+
      df_ks$True_Bads_Test[i]}
  }
  for (i in (1:nrow(df_ks))) {
    df_ks$Cumulative_Goods_Train_Pct[i]=df_ks$Cumulative_Goods_Train[i]/
      df_ks$Cumulative_Goods_Train[nrow(df_ks)]
    df_ks$Cumulative_Bads_Train_Pct[i]=df_ks$Cumulative_Bads_Train[i]/
      df_ks$Cumulative_Bads_Train[nrow(df_ks)]
    if(!is.na(df_test)){
    df_ks$KS_Test[i] <- df_ks$Cumulative_Goods_Train_Pct[i]-
      df_ks$Cumulative_Goods_Train_Pct[i]
    df_ks$Cumulative_Goods_Test_Pct[i]=df_ks$Cumulative_Goods_Test[i]/
      df_ks$Cumulative_Goods_Test[nrow(df_ks)]
    df_ks$Cumulative_Bads_Test_Pct[i]=df_ks$Cumulative_Bads_Test[i]/
      df_ks$Cumulative_Bads_Test[nrow(df_ks)]
    df_ks$KS_Test[i] <- df_ks$Cumulative_Goods_Test_Pct[i]-
      df_ks$Cumulative_Goods_Test_Pct[i]}
  }
  df_ks$KS_Test_Train <- df_ks$Cumulative_Goods_Train_Pct-
    df_ks$Cumulative_Bads_Train_Pct
  if(!is.na(df_test)){
  df_ks$KS_Test_Test <- df_ks$Cumulative_Goods_Test_Pct-
    df_ks$Cumulative_Bads_Test_Pct}
  return(df_ks)
}



##############
# Load Data  #
##############

## This is to make sure we generate always the same random numbers ##
set.seed(1)

# Load data #
main_dir <- "C:\\Projects\\Fraud_Profiling\\input\\"
setwd(main_dir)
df <- read.csv("input.csv", sep=",")
df$date <- as.Date(df$date)
df_raw <- df

# Read results for emails
email_result <- read.csv(paste(main_dir,
"SEON_emails.csv",sep=""),
sep=";")

# Read emails to link with credit number
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

get_emails <- 
  read.csv(paste(main_dir,"client_emails.csv",sep=""),sep=";")
df <- df[ , -which(names(df) %in% c("email"))]
df <- merge(df, get_emails, by.x = "egn", by.y = "egn", all.x = TRUE)
df <- df[!duplicated(df$egn),]
df <- merge(df, email_result, by.x = "email", by.y = "email",
            all.x = TRUE)


#######################
## Get phone numbers ##
#######################
phone_result <- read.csv(paste(main_dir,
  "SEON_phones.csv",sep=""),
   sep=",")
phone_result <- phone_result[ , -which(names(phone_result) %in% 
    c("applied_rules"))]
phone_result$whatsapp.photo <- 
  ifelse(phone_result$whatsapp.photo=="",0,1)
phone_result$viber.photo <- 
  ifelse(phone_result$viber.photo=="",0,1)
phone_result$whatsapp.last_seen <- as.Date(
  as.POSIXct(phone_result$whatsapp.last_seen, origin="1970-01-01"))
phone_result$viber.last_seen <- as.Date(
  as.POSIXct(phone_result$whatsapp.last_seen, origin="1970-01-01"))
phone_result <- phone_result[ , -which(names(phone_result) %in% 
   c("country","success","type","valid"))]
phone_result$digits <- nchar(phone_result$phone_number)
phone_result$phone_number_corr <- paste("0",
  substring(phone_result$phone_number,4,12),sep="")

phone_numbers <- read.xlsx(paste(main_dir,
   "all_phone_numbers.xlsx",sep=""))
df <- merge(df,phone_numbers[,c("credit_number","pers_number_1")],
            by.x = "credit_number", by.y = "credit_number",
            all.x = TRUE)
df <- merge(df,phone_result,
            by.x = "pers_number_1", by.y = "phone_number_corr",
            all.x = TRUE)




#####################
# Define Fraud Flag #
#####################

df$default_flag <- ifelse(df$amount_paid==0 & df$default_flag==1, 
   1, 0)



#########################
# Select relevant data  #
#########################

all_cols <- c("egn","credit_number","date","default_flag", 
  "age","maturity","gender","ratio_installment_income",
  "ownership","education","household_children","on_address",
  "experience_employer","marital_status", "purpose", "status_work",
  "status_active_total","status_finished_total","monthly_installment_total",
  "source_entity_count_total","amount_drawn_total","cred_count_total",
  "outs_overdue_ratio_total",names(df)[c(82:118)])
df <- df[,c(all_cols)]



#################
# Analyze data  #
#################

# Get information value
cols_fac <- c("education","gender","marital_status",
  "ownership","purpose","status_work",
   "status_active_total","status_finished_total",
  
  "email_exists","facebook_exists",
  "google_exists","microsoft_exists","instagram_exists",
  "spotify_exists","haveibeenpwned_exists",
  
  "carrier","facebook_registered","instagram_registered","yahoo_registered",              
  "whatsapp.photo","viber.photo", "google_registered",            
  "whatsapp_registered","viber_registered")
df[cols_fac] <- lapply(df[cols_fac], factor)
create_infotables(data=df, y="default_flag", bins=5,parallel=FALSE)$Tables



#######################################
# Make correlation report for emails  #
#######################################
list_var_model_cor <-  c("email_exists",
  "facebook_exists","google_exists","microsoft_exists",
  "instagram_exists","spotify_exists","haveibeenpwned_exists"
)
corr_df <- dummy.data.frame(df[,list_var_model_cor], sep = "_")
#write.xlsx(abs(cor(corr_df)),paste(main_dir,"correlation_email.xlsx",sep=""))



######################################
# Make correlation report for phones #
######################################
df$score <- ifelse(is.na(df$score),-999,df$score)
list_var_model_cor <-  c("carrier","score","facebook_registered",
   "instagram_registered","yahoo_registered","whatsapp.photo",
   "viber.photo","google_registered","whatsapp_registered",
    "viber_registered")
corr_df <- dummy.data.frame(df[,list_var_model_cor], sep = "_")
#write.xlsx(abs(cor(corr_df)),paste(main_dir,"correlation_phones.xlsx",sep=""))



#########################
# Cut and bin variables #
#########################

# Create an Information Value table
IV <- create_infotables(data=df, y="default_flag", bins=10, parallel=FALSE) 

# Age : OK
plot_infotables(IV, "age")
df$age_cut <- ifelse(df$age<=28,"28_less","29_more")
df$age <- as.factor(df$age_cut)
df <- within(df, age <- relevel(age, ref = "29_more"))
gb_index_fct(df, df$age, "default_flag")

# Gender : OK
plot_infotables(IV, "gender")
df$gender <- as.factor(df$gender)
df <- within(df, gender <- relevel(gender, ref = "1"))
gb_index_fct(df, df$gender, "default_flag")

# Ownership : OK
plot_infotables(IV, "ownership")
df$ownership_cut <- ifelse(df$ownership %in% c(1), "1", 
                ifelse(is.na(df$ownership), "not_1", "not_1"))
df$ownership <- as.factor(df$ownership_cut)
df <- within(df, ownership <- relevel(ownership, ref = "not_1"))
gb_index_fct(df, df$ownership, "default_flag")

# Education : OK
plot_infotables(IV, "education")
df$education <- ifelse(is.na(df$education), "2_3", 
                ifelse(df$education %in% c(2,3), "2_3", df$education))
df$education <- as.factor(df$education)
df <- within(df, education <- relevel(education, ref = "2_3"))
gb_index_fct(df, df$education, "default_flag")

# Marital Status : OK
plot_infotables(IV, "marital_status")
df$marital_status_cut <- ifelse(is.na(df$marital_status), "2_5",
  ifelse(df$marital_status %in% c(1,3,4), "1_3_4", "2_5"))
df$marital_status <- as.factor(df$marital_status_cut)
df <- within(df, marital_status <- relevel(marital_status, ref =  "2_5"))
gb_index_fct(df, df$marital_status, "default_flag")

# Household children : OK
plot_infotables(IV, "household_children")
df$household_children_cut <- ifelse(is.na(df$household_children), "0",
 ifelse(df$household_children==0,"0", "more_0"))
df$household_children <- as.factor(df$household_children_cut)
df <- within(df, household_children <- relevel(household_children, 
                                               ref = "0"))
gb_index_fct(df, df$household_children, "default_flag")

# Experience employer : OK
plot_infotables(IV, "experience_employer")
df$experience_employer_cut <- ifelse(is.na(df$experience_employer), "more_12",
  ifelse(df$experience_employer<=1,"0_1",
  ifelse(df$experience_employer<=120,"2_120","more_120")))
df$experience_employer <- as.factor(df$experience_employer_cut)
df <- within(df, experience_employer <- relevel(experience_employer, 
    ref = "more_12"))
gb_index_fct(df, df$experience_employer, "default_flag")
                      
# Purpose : OK 
plot_infotables(IV, "purpose")
df$purpose_cut <- ifelse(is.na(df$purpose), "1_6",
  ifelse(df$purpose %in% c(1,6),"1_6",
  ifelse(df$purpose==0, "1_6", "not_1_6")))
df$purpose <- as.factor(df$purpose_cut)
df <- within(df, purpose <- relevel(purpose, ref = "1_6"))
gb_index_fct(df, df$purpose, "default_flag")

# Status work : OK
plot_infotables(IV, "status_work")
df$status_work_cut <- ifelse(is.na(df$status_work), "other",
    ifelse(df$status_work==5, "5",
    ifelse(df$status_work %in% c(4,6,9), "4_6_9", "other")))
df$status_work <- as.factor(df$status_work_cut)
df <- within(df, status_work <- relevel(status_work, ref = "other"))
gb_index_fct(df, df$status_work, "default_flag")

# Status finished total : OK
plot_infotables(IV, "status_finished_total")
df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),"0_71_72",
  ifelse(df$status_finished_total %in% c(0,71,72),"0_71_72",
  ifelse(df$status_finished_total %in% c(73,74),"73_74", "75")))
df$status_finished_total <- as.factor(df$status_finished_total_cut)
df <- within(df, status_finished_total <- relevel(status_finished_total,
                                                  ref = "0_71_72"))
gb_index_fct(df, df$status_finished_total, "default_flag")

# Status active total : OK
plot_infotables(IV, "status_active_total")
df$status_active_total_cut <- ifelse(is.na(df$status_active_total), "0",
  ifelse(df$status_active_total==0,"0",
  ifelse(df$status_active_total %in% c(71,72,73), "71_72_73",
  ifelse(df$status_active_total==-1,"-1_74_75","-1_74_75"))))
df$status_active_total <- as.factor(df$status_active_total_cut)
df <- within(df, status_active_total <- relevel(status_active_total, 
          ref = "0"))
gb_index_fct(df, df$status_active_total, "default_flag")

# Source entity count : OK
plot_infotables(IV, "source_entity_count_total")
df$source_entity_count_total_cut <- ifelse(is.na(df$source_entity_count_total), 
      "1_2_5_more",
  ifelse(df$source_entity_count_total %in% c(0), "0",
  ifelse(df$source_entity_count_total %in% c(1,2), "1_2_5_more",
  ifelse(df$source_entity_count_total %in% c(3,4), "3_4","1_2_5_more"))))
df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
df <- within(df, source_entity_count_total <- relevel(source_entity_count_total, 
       ref = "1_2_5_more"))
gb_index_fct(df, df$source_entity_count_total, "default_flag")                                     
             
# Cred count total : OK
plot_infotables(IV, "cred_count_total")
df$cred_count_total_cut <- ifelse(is.na(df$cred_count_total), "2_more_8",
  ifelse(df$cred_count_total==0,"0",
  ifelse(df$cred_count_total==1,"1",
  ifelse(df$cred_count_total %in% c(2), "2_more_8",
  ifelse(df$cred_count_total %in% c(3:5), "3_5","2_more_8")))))
df$cred_count_total <- as.factor(df$cred_count_total_cut)
df <- within(df, cred_count_total <- relevel(cred_count_total, 
    ref = "2_more_8"))
gb_index_fct(df, df$cred_count_total, "default_flag")                                     

# Ratio : OK
plot_infotables(IV, "outs_overdue_ratio_total")
df$outs_overdue_ratio_total_cut <- ifelse(
     is.na(df$outs_overdue_ratio_total), "more_0.08_missing",
  ifelse(df$outs_overdue_ratio_total==-999,"more_0.08_missing",
  ifelse(df$outs_overdue_ratio_total<=0.03,"0_0.03",
  ifelse(df$outs_overdue_ratio_total<=0.08,"0.03_0.08","more_0.08_missing"))))
df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
df <- within(df, outs_overdue_ratio_total <- relevel(outs_overdue_ratio_total, 
     ref = "0_0.03"))
gb_index_fct(df, df$outs_overdue_ratio_total, "default_flag")

# Email score : OK
plot_infotables(IV, "email_score")
df$email_score_cut <- ifelse(is.na(df$email_score), "more_8",
  ifelse(df$email_score==0,"0",
  ifelse(df$email_score<=8,"1_8","more_8")))
df$email_score <- as.factor(df$email_score_cut)
df <- within(df, email_score <- relevel(email_score, ref = "1_8"))
gb_index_fct(df, df$email_score, "default_flag")

# Email exists : OK 
gb_index_fct(df, df$email_exists, "default_flag")
df$email_exists_cut <- ifelse(is.na(df$email_exists), "other",
  ifelse(df$email_exists=="FALSE", "FALSE", 
  ifelse(df$email_exists=="TRUE", "other" , "other")))
df$email_exists <- as.factor(df$email_exists_cut)
df <- within(df, email_exists <- relevel(email_exists, ref = "other"))
gb_index_fct(df, df$email_exists, "default_flag")

# Facebook : OK
gb_index_fct(df, df$facebook_exists, "default_flag")
df$facebook_exists_cut <- ifelse(is.na(df$facebook_exists), "other",
   ifelse(df$facebook_exists=="FALSE", "FALSE",
   ifelse(df$facebook_exists=="TRUE", "other", "other")))
df$facebook_exists <- as.factor(df$facebook_exists_cut)
df <- within(df, facebook_exists <- relevel(facebook_exists, ref = "other"))
gb_index_fct(df, df$facebook_exists, "default_flag")

# Google : weak
gb_index_fct(df, df$google_exists, "default_flag")
df$google_exists_cut <- ifelse(is.na(df$google_exists), "other",
  ifelse(df$google_exists=="FALSE", "FALSE",
  ifelse(df$google_exists=="TRUE", "other", "other")))
df$google_exists <- as.factor(df$google_exists_cut)
df <- within(df, google_exists <- relevel(google_exists, ref = "other"))
gb_index_fct(df, df$google_exists, "default_flag")

# Microsoft : NO
gb_index_fct(df, df$microsoft_exists, "default_flag")
df$microsoft_exists_cut <- ifelse(is.na(df$microsoft_exists), "other",
  ifelse(df$microsoft_exists=="FALSE", "other",
  ifelse(df$microsoft_exists=="TRUE", "TRUE", "other")))
df$microsoft_exists <- as.factor(df$microsoft_exists_cut)
df <- within(df, microsoft_exists <- relevel(microsoft_exists, ref = "other"))
gb_index_fct(df, df$microsoft_exists, "default_flag")

# Instagram : NO
gb_index_fct(df, df$instagram_exists, "default_flag")
df$instagram_exists_cut <- ifelse(is.na(df$instagram_exists), "other",
 ifelse(df$instagram_exists=="FALSE", "other",
 ifelse(df$instagram_exists=="TRUE", "TRUE", "other")))
df$instagram_exists <- as.factor(df$instagram_exists_cut)
df <- within(df, instagram_exists <- relevel(instagram_exists, ref = "other"))
gb_index_fct(df, df$instagram_exists, "default_flag")

# Haveibeenpwned : YES
gb_index_fct(df, df$haveibeenpwned_exists, "default_flag")
df$haveibeenpwned_exists_cut <- ifelse(is.na(df$haveibeenpwned_exists), "other",
  ifelse(df$haveibeenpwned_exists=="FALSE", "other",
  ifelse(df$haveibeenpwned_exists=="TRUE", "TRUE", "other")))
df$haveibeenpwned_exists <- as.factor(df$haveibeenpwned_exists_cut)
df <- within(df, haveibeenpwned_exists <- relevel(haveibeenpwned_exists, 
     ref = "other"))
gb_index_fct(df, df$haveibeenpwned_exists, "default_flag")

# Number Carrier : NO
gb_index_fct(df, df$carrier, "default_flag")
df$carrier_cut <- ifelse(is.na(df$carrier), "other",
  ifelse(df$carrier=="FALSE", "other",
  ifelse(df$carrier=="TRUE", "TRUE", "other")))
df$carrier <- as.factor(df$carrier_cut)
df <- within(df, carrier <- relevel(carrier, 
  ref = "other"))
gb_index_fct(df, df$carrier, "default_flag")

# PHONE SCORE : OK
gb_index_fct(df, df$score, "default_flag")
df$score <- as.factor(df$score)
df <- within(df, score <- relevel(score, 
     ref = "0"))
gb_index_fct(df, df$score, "default_flag")

# PHONE + FACEBOOK : OK
gb_index_fct(df, df$facebook_registered, "default_flag")
df$facebook_registered_cut <- ifelse(is.na(df$facebook_registered), "other",
 ifelse(df$facebook_registered=="False", "FALSE",
 ifelse(df$facebook_registered=="True", "other", "other")))
df$facebook_registered <- as.factor(df$facebook_registered_cut)
df <- within(df, facebook_registered <- relevel(facebook_registered, 
      ref = "other"))
gb_index_fct(df, df$facebook_registered, "default_flag")

# PHONE + INSTAGRAM : weak
gb_index_fct(df, df$instagram_registered, "default_flag")
df$instagram_registered_cut <- ifelse(is.na(df$instagram_registered), "other",
  ifelse(df$instagram_registered=="False", "other",
  ifelse(df$instagram_registered=="True", "True", "other")))
df$instagram_registered <- as.factor(df$instagram_registered_cut)
df <- within(df, instagram_registered <- relevel(instagram_registered, 
    ref = "other"))
gb_index_fct(df, df$instagram_registered, "default_flag")

# WHATSAPP PHOTO : OK  
gb_index_fct(df, df$whatsapp.photo, "default_flag")
df$whatsapp.photo_cut <- ifelse(is.na(df$whatsapp.photo),0,
 ifelse(df$whatsapp.photo==1, 1, 0))
df$whatsapp.photo <- as.factor(df$whatsapp.photo_cut)
df <- within(df, whatsapp.photo <- relevel(whatsapp.photo, ref = "0"))
gb_index_fct(df, df$whatsapp.photo, "default_flag")

# viber PHOTO : OK  
gb_index_fct(df, df$viber.photo, "default_flag")
df$viber.photo_cut <- ifelse(is.na(df$viber.photo),0,
   ifelse(df$viber.photo==1, 1, 0))
df$viber.photo <- as.factor(df$viber.photo_cut)
df <- within(df, viber.photo <- relevel(viber.photo, ref = "0"))
gb_index_fct(df, df$viber.photo, "default_flag")

# Google registered : NO  
gb_index_fct(df, df$google_registered, "default_flag")
df$google_registered_cut <- ifelse(is.na(df$google_registered),0,
   ifelse(df$google_registered==1, 1, 0))
df$google_registered <- as.factor(df$google_registered_cut)
df <- within(df, google_registered <- relevel(google_registered, ref = "0"))
gb_index_fct(df, df$google_registered, "default_flag")

# Twitter registered : NO  
gb_index_fct(df, df$twitter_registered, "default_flag")
df$twitter_registered_cut <- ifelse(is.na(df$twitter_registered),0,
    ifelse(df$twitter_registered==1, 1, 0))
df$twitter_registered <- as.factor(df$twitter_registered_cut)
df <- within(df, twitter_registered <- relevel(twitter_registered, ref = "0"))
gb_index_fct(df, df$twitter_registered, "default_flag")

# Whatsapp registered : NO  
gb_index_fct(df, df$whatsapp_registered, "default_flag")
df$whatsapp_registered_cut <- ifelse(is.na(df$whatsapp_registered), "other",
  ifelse(df$whatsapp_registered=="False", "other",
  ifelse(df$whatsapp_registered=="True", "True", "other")))
df$whatsapp_registered <- as.factor(df$whatsapp_registered_cut)
df <- within(df, whatsapp_registered <- relevel(whatsapp_registered, 
    ref = "other"))
gb_index_fct(df, df$whatsapp_registered, "default_flag")

# Viber registered : NO  
gb_index_fct(df, df$viber_registered, "default_flag")
df$viber_registered_cut <- ifelse(is.na(df$viber_registered), "other",
  ifelse(df$viber_registered=="False", "False",
  ifelse(df$viber_registered=="True", "other", "other")))
df$viber_registered <- as.factor(df$viber_registered_cut)
df <- within(df, viber_registered <- relevel(viber_registered, 
    ref = "other"))
gb_index_fct(df, df$viber_registered, "default_flag")



#########################################################
# Choose variable to be enter logistic regression model #
#########################################################

# Change column names
colnames(df)[which(names(df) == "score")] <- "phone_score"


list_var_model_cor <-  c("default_flag","age","gender",
   "ownership","education","purpose", "household_children",
   "experience_employer","marital_status",
   "status_work","status_finished_total","status_active_total",
   "source_entity_count_total",
   "cred_count_total","outs_overdue_ratio_total",
   "email_exists","facebook_exists",
   "google_exists","haveibeenpwned_exists",
   "phone_score","facebook_registered","instagram_registered",
   "whatsapp.photo","viber.photo","whatsapp_registered","viber_registered")
list_var_model <-  c("default_flag",
   "age","education","purpose","marital_status", "ownership",
   "status_finished_total", "outs_overdue_ratio_total",
   #"email_exists",
   #"facebook_exists",
   #google_exists",
   #"whatsapp.photo","viber.photo"
   "whatsapp_registered",
   "viber_registered"
   )
df_select <- df[,list_var_model]
corr_df <- dummy.data.frame(df[,list_var_model_cor], sep = "_")
write.xlsx(abs(cor(corr_df)),
           paste(main_dir,"all_dummies_correlation.xlsx",sep=""))


#########################################################
# Separate into 80% Training and 20% Testing            #
#########################################################

split <- sample.split(df_select$default_flag, SplitRatio = 0.8)
df80 <- subset(df_select, split == TRUE)
df20 <- subset (df_select, split == FALSE)



#####################################
# Carry-out stepwise regression     #
#####################################

df_Log_Credirect_Fraud <- glm(default_flag ~ ., family=binomial, 
   data=df80)
step(df_Log_Credirect_Fraud,direction = c("backward"))



#####################################
# Model with lositic regression     #
#####################################

# Summary of coefficients of Logistic Regression
summary(df_Log_Credirect_Fraud)

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
df80$score <- predict(df_Log_Credirect_Fraud, newdata=df80, 
                      type="response")
forecast <- predict(df_Log_Credirect_Fraud, newdata=df80, 
                    type="response")
ROCRpred <- ROCR::prediction (forecast, df80$default_flag)
ROCRperf <- performance (ROCRpred, "tpr", "fpr")
plot(ROCRperf, col='blue', lwd=5)
par(new=TRUE)
plot(y, col='green',axes=F,xlab='',ylab='',lwd=1)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
giniLog <- 2*(auc - 0.5)
gini_train <- giniLog*100
gini_train

# Apply on testing dataset
forecast2 <- predict(df_Log_Credirect_Fraud, newdata=df20, 
                     type="response")
ROCRpred <- ROCR::prediction (forecast2, df20$default_flag)
ROCRperf <- performance (ROCRpred, "tpr", "fpr")
plot(ROCRperf,col='blue', lwd=5)
par(new=TRUE)
plot(y, col='green',axes=F,xlab='',ylab='',lwd=1)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
giniLog <- 2*(auc - 0.5)
gini_test <- giniLog*100
gini_test

# Apply on whole dataset
df$score <- predict(df_Log_Credirect_Fraud, newdata=df, 
                    type="response")



##########################################
# User chooses bins for rest of analysis #
##########################################
bins <- c(0,0.0125,0.025,0.0375,0.05,0.0625,0.075,0.875,
          0.1,0.125,0.15,0.175,0.2,0.225,0.25,0.275,0.3,
          0.325,0.35,0.375,0.4,0.5,0.6)



##############################
# Kolmogorov-Smirnoff Test   #
##############################

score_train <- forecast
score_test <- forecast2
df_train <- cbind(df80,score_train)
df_test <- cbind(df20,score_test)
df_train$Score <- cut(df_train$score_train,bins)
df_test$Score <- cut(df_test$score_test,bins)
ks(df_train,df_test)



######################
# Confusion Matrix   #
######################
# Get the confusion matrix and choose the cut-off point for bads
cut_off <- 0.25
predict <- ifelse(score_test>cut_off,1,0)
table(predict)[2]/sum(table(predict))*100
accuracy_table <- table(df20$default_flag,predict)
accuracy_table


##############################
# Output file for analysis   #
##############################
setwd("C:\\Projects\\Fraud_Profiling\\results\\")
score_all <- predict(df_Log_Credirect_Fraud, newdata=df, type="response")
df_anal <- cbind(df,score_all)
names(df_anal)[length(df_anal)] <- 'Score'
df_anal <- merge(df_anal, df_raw[,c("credit_number","product_cat","amount_paid","amount")], 
                by.x = "credit_number", by.y = "credit_number", all.x = TRUE)
df_anal$default_flag_predict <- ifelse(df_anal$Score>0.2,1,0)

# Output results
write.xlsx(df_anal[,c("credit_number","amount","amount_paid","default_flag",
                     "default_flag_predict")],"Predicted_Default_Flag.xlsx")
write.csv(df_anal,"Results_Logit_for_Analysis.csv")



################
# Save RData   #
################
save(df_Log_Credirect_Fraud, 
     file = "PD_Logistic_Application_Credirect_Fraud.rdata")



#############################################
# Choose characteristics on entire dataset  #
#############################################

# Compute confusion matrix on whole dataset
df_new <- df
score_all <- predict(df_Log_Credirect_Fraud, newdata=df_new , type="response")
cut_off <- 0.25
predict <- ifelse(score_all>cut_off,1,0)
table(predict)[2]/sum(table(predict))*100
accuracy_table <- table(df$default_flag,predict)
accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)*100
sensitivity <-  accuracy_table[2,2]/(accuracy_table[2,2]+accuracy_table[2,1])
specificity <-  accuracy_table[1,1]/(accuracy_table[1,1]+accuracy_table[1,2])
tot_acc <- sensitivity + specificity
accuracy_table
accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)
accuracy

# Get KS test on all data set
score_all <- predict(df_Log_Credirect_Fraud, newdata=df_new , type="response")
df_new <- df
df_new <- cbind(df,score_all)
df_new$Score <- cut(df_new$score_all,bins)
df_new <- merge(df_new, df_raw[,c("credit_number","product_cat")], 
                by.x = "credit_number", by.y = "credit_number", all.x = TRUE)
ks(df_new,NA)



######### END #############

