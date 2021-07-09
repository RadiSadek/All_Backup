
##########################################
# SCORING ON APPLICATION FOR CREDIRECT   #
#  ESTIMATION OF PROBABILITY TO DEFAULT  # 
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
set.seed(103)

# Load data #
main_dir <- "C:\\Projects\\Application_Scoring\\Credirect_v4\\"
setwd(main_dir)
df <- read.csv("data\\input_current_v3.csv", sep=",")
df <- subset(df,df$product_cat=="Credirect_User")
df$date <- as.Date(df$date)


# Remove those who are with too many NAs
#View(round(sapply(df, function(x) sum(is.na(x)))/nrow(df),2))
df <- df[ , -which(names(df) %in% c("ratio_installment_income",
                                    "household_children"))]
df_raw <- df



##################
# Load API Data  #
##################

# Read data
api_data <- read.xlsx("data\\Api_Applications.xlsx")
api_data$API_amount <- as.numeric(api_data$API_amount)
api_data$API_period <- as.numeric(api_data$API_period)
api_data$API_installments <- as.numeric(api_data$API_installments)


# Merge to main dataframe
df <- merge(df,api_data,by.x = "credit_number",by.y = "credit_number",
            all.x = TRUE)

# Check time consistency of API data
df$month <- substring(df$date,1,7)
round(apply(table(df$API_referral_source,df$month), 2, 
            function(i) i/sum(i)),4)*100



####################
# Load SEON Phone  #
####################

# Read updated 
seon_phone <- read.csv(
 "C:\\Projects\\Application_Scoring\\Credirect_v4\\data\\seon_phone_v2.csv")

# Drop outdated seon phone variables (too much missings)
df <- df[ , -which(names(df) %in% c("facebook.registered.phone",
    "instagram.registered.phone","twitter.registered.phone",
    "google.registered.phone","viber_registered","whatsapp_registered",
    "viber_registered_bin","X.4","X.3","X.2","X.1","X"))]

# Join phone variables
seon_phone <- seon_phone[,c("credit_number","facebook.registered",
     "twitter.registered","whatsapp.registered","viber.registered",
     "skype.registered","instagram.registered")]
names(seon_phone) <- c("credit_number","facebook_phone","twitter_phone",
      "whatsapp_phone","viber_phone","skype_phone","instagram_phone")
df <- merge(df,seon_phone,by.x = "credit_number",by.y = "credit_number",
            all.x = TRUE)




##########################
# Work on SEON Email variables #
##########################

table(df$E113)
table(df$E114)
table(df$E115)
table(df$E117)

table(df$E121)
table(df$E122)
table(df$E111)
table(df$E124)
table(df$E125)

gb_index_fct(df,df$E113,"default_flag")
gb_index_fct(df,df$E114,"default_flag")
gb_index_fct(df,df$E115,"default_flag")
gb_index_fct(df,df$E117,"default_flag")

gb_index_fct(df,df$E121,"default_flag")
gb_index_fct(df,df$E122,"default_flag")
gb_index_fct(df,df$E111,"default_flag")
gb_index_fct(df,df$E124,"default_flag")
gb_index_fct(df,df$E125,"default_flag")

# check colinarity of seon variables
seon_df <- complete_fct(df,c("E113","E114","E121","E122","E124","E125","E111",
                             "E115","E117"))
seon_df <- seon_df[,c("E113","E114","E121","E122","E124","E125","E111",
                      "E115","E117")]
seon_df$E113 <- as.factor(seon_df$E113)
seon_df$E114 <- as.factor(seon_df$E114)
seon_df$E121 <- as.factor(seon_df$E121)
seon_df$E122 <- as.factor(seon_df$E122)
seon_df$E124 <- as.factor(seon_df$E124)
seon_df$E125 <- as.factor(seon_df$E125)
seon_df$E111 <- as.factor(seon_df$E111)
seon_df$E115 <- as.factor(seon_df$E115)
seon_df$E117 <- as.factor(seon_df$E117)
corr_df <- suppressWarnings(dummy.data.frame(seon_df, sep = "_"))
cor(corr_df)

# Check combinations
seon_df$domain <- paste(seon_df$E113,seon_df$E114,seon_df$E115,
                        seon_df$E117,seon_df$E111,seon_df$E121,
                        seon_df$E122,seon_df$E124,seon_df$E125,sep="_")

# Make final variable with seon 
df$seon_var2 <- 1 + ifelse(df$E113==1,-1,0) + 
  df$E114 + df$E115 + df$E117 + df$E111 + df$E121 + df$E122 + df$E124 + df$E125

df$seon_var <- ifelse(df$E114==1,10,0) + ifelse(df$E115==1,1,0) + 
  ifelse(df$E117==1,4,0) + ifelse(df$E111==1,8,0) + ifelse(df$E121==1,4,0) + 
  ifelse(df$E122==1,1,0) + ifelse(df$E124==1,1,0) + ifelse(df$E125==1,1,0)


#########################
# Cut and bin variables #
#########################

# Create an Information Value table
IV <- create_infotables(data=df, y="default_flag", bins=10, parallel=FALSE)

# Flag Risky Location : NO 
gb_index_fct(df, df$flag_location_curr, "default_flag")
gb_index_fct(df, df$flag_location_offi, "default_flag")
plot_infotables(IV, "distance_curr_offi")

# Same City : NO 
gb_index_fct(df, df$same_city, "default_flag")

# Maturity : OK 
plot_infotables(IV, "maturity")
df$maturity_cut <- ifelse(df$maturity<=4,"3_4",
   ifelse(df$maturity<=8,"5_8","more_9"))
df$maturity <- as.factor(df$maturity_cut)
df <- within(df, maturity <- relevel(maturity, ref = "5_8"))
gb_index_fct(df, df$maturity, "default_flag")

# API_period : OK 
plot_infotables(IV, "API_period")
df$API_period_cut <- 
  ifelse(is.na(df$API_period),"5_6",
  ifelse(df$API_period<=4,"less_4",
  ifelse(df$API_period<=6,"5_6","more_7")))
df$API_period <- as.factor(df$API_period_cut)
df <- within(df, API_period <- relevel(API_period, ref = "5_6"))
gb_index_fct(df, df$API_period, "default_flag")

# API_amount : OK 
plot_infotables(IV, "API_amount")
df$API_amount_cut <- 
  ifelse(is.na(df$API_amount),"more_650",
  ifelse(df$API_amount<=250,"less_250",
  ifelse(df$API_amount<=450,"300_650","more_650")))
df$API_amount <- as.factor(df$API_amount_cut)
df <- within(df, API_amount <- relevel(API_amount, ref = "more_650"))
gb_index_fct(df, df$API_amount, "default_flag")

# Age : OK
plot_infotables(IV, "age")
df$age_cut <- ifelse(df$age<=20,"20_less","more_20")
df$age <- as.factor(df$age_cut)
df <- within(df, age <- relevel(age, ref = "more_20"))
gb_index_fct(df, df$age, "default_flag")

# Gender : No
gb_index_fct(df, df$gender, "default_flag")
df$gender <- as.factor(df$gender)
df <- within(df, gender <- relevel(gender, ref = "1"))
gb_index_fct(df, df$gender, "default_flag")

# Ownership : OK 
gb_index_fct(df, df$ownership, "default_flag")
df$ownership_cut <- ifelse(df$ownership %in% c(1), "1", 
   ifelse(is.na(df$ownership), "not_1", "not_1"))
df$ownership <- as.factor(df$ownership_cut)
df <- within(df, ownership <- relevel(ownership, ref = "not_1"))
gb_index_fct(df, df$ownership, "default_flag")

# Education : OK
gb_index_fct(df, df$education, "default_flag")
df$education_cut <- ifelse(is.na(df$education),"2",
   ifelse(df$education==1,"1",
   ifelse(df$education %in% c(3,4), "3_4", "2")))
df$education <- as.factor(df$education_cut)
df <- within(df, education <- relevel(education, ref = "2"))
gb_index_fct(df, df$education, "default_flag")

# Marital Status : OK
gb_index_fct(df, df$marital_status, "default_flag")
plot_infotables(IV, "marital_status")
df$marital_status_cut <- ifelse(is.na(df$marital_status), "2_4_5",
    ifelse(df$marital_status %in% c(1,3), "1_3", "2_4_5"))
df$marital_status <- as.factor(df$marital_status_cut)
df <- within(df, marital_status <- relevel(marital_status, 
       ref =  "2_4_5"))
gb_index_fct(df, df$marital_status, "default_flag")

# Household total : NO
plot_infotables(IV, "household_total")
df$household_total_cut <- ifelse(is.na(df$household_total), "not_2",
 ifelse(df$household_total==2,"2","not_2"))
df$household_total <- as.factor(df$household_total_cut)
df <- within(df, household_total <- relevel(household_total, 
 ref = "not_2"))
gb_index_fct(df, df$household_total, "default_flag")

# Experience employer : YES
plot_infotables(IV, "experience_employer")
df$experience_employer_cut <- 
  ifelse(is.na(df$experience_employer), "less_24",
  ifelse(df$experience_employer<=24,"less_24","more_24"))
df$experience_employer <- as.factor(df$experience_employer_cut)
df <- within(df, experience_employer <- relevel(experience_employer, 
  ref = "less_24"))
gb_index_fct(df, df$experience_employer, "default_flag")

# ON ADDRESS  : NO
plot_infotables(IV, "on_address")
df$on_address_cut <- ifelse(is.na(df$on_address), "less_24",
  ifelse(df$on_address<=24,"less_24","more_24"))
df$on_address <- as.factor(df$on_address_cut)
df <- within(df, on_address <- relevel(on_address, 
    ref = "less_24"))
gb_index_fct(df, df$on_address, "default_flag")
                      
# Purpose : YES
gb_index_fct(df, df$purpose, "default_flag")
df$purpose_cut <- ifelse(is.na(df$purpose), "not_6",
  ifelse(df$purpose==6,"6","not_6"))
df$purpose <- as.factor(df$purpose_cut)
df <- within(df, purpose <- relevel(purpose, ref = "not_6"))
gb_index_fct(df, df$purpose, "default_flag")

# Status work : YES 
gb_index_fct(df, df$status_work, "default_flag")
df$status_work_cut <- ifelse(is.na(df$status_work), "other",
  ifelse(df$status_work %in% c(4,5,9,10,12), "4_5_9_10_12","other"))
df$status_work <- as.factor(df$status_work_cut)
df <- within(df, status_work <- relevel(status_work, ref = "other"))
gb_index_fct(df, df$status_work, "default_flag")

# Status finished total : NO
gb_index_fct(df, df$status_finished_total, "default_flag")
df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
   "0_71_72_73",ifelse(df$status_finished_total %in% c(0,71,72),"0_71_72_73",
   "74_75"))
df$status_finished_total <- as.factor(df$status_finished_total_cut)
df <- within(df, status_finished_total <- relevel(status_finished_total,
  ref = "0_71_72_73"))
gb_index_fct(df, df$status_finished_total, "default_flag")

# Status active total : YES 
gb_index_fct(df, df$status_active_total, "default_flag")
df$status_active_total_cut <- ifelse(is.na(df$status_active_total),"other",
  ifelse(df$status_active_total %in% c(0), "no_delay",
  ifelse(df$status_active_total %in% c(-1,71,72),"other",
  ifelse(df$status_active_total %in% c(73),"73",
  ifelse(df$status_active_total %in% c(74,75),"74_75","other")))))
df$status_active_total <- as.factor(df$status_active_total_cut)
df <- within(df, status_active_total <- relevel(status_active_total, 
   ref = "other"))
gb_index_fct(df, df$status_active_total, "default_flag")

# Source entity count : YES
plot_infotables(IV, "source_entity_count_total")
df$source_entity_count_total_cut <- 
  ifelse(is.na(df$source_entity_count_total),"0_4",
  ifelse(df$source_entity_count_total<=4, "0_4","more_5"))
df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
df <- within(df, source_entity_count_total <- relevel(source_entity_count_total, 
   ref = "0_4"))
gb_index_fct(df, df$source_entity_count_total, "default_flag")                                     
             
# Cred count total : YES
plot_infotables(IV, "cred_count_total")
df$cred_count_total_cut <- ifelse(is.na(df$cred_count_total), "1_3",
  ifelse(df$cred_count_total<=3,"1_3","more_4"))
df$cred_count_total <- as.factor(df$cred_count_total_cut)
df <- within(df, cred_count_total <- relevel(cred_count_total, 
  ref = "1_3"))
gb_index_fct(df, df$cred_count_total, "default_flag")                                

# Ratio : YES 
plot_infotables(IV, "outs_overdue_ratio_total")
df$outs_overdue_ratio_total_cut <- ifelse(
  is.na(df$outs_overdue_ratio_total), "0_0.01",
  ifelse(df$outs_overdue_ratio_total==-999,"0_0.01",
  ifelse(df$outs_overdue_ratio_total<=0.01,"0_0.01",
  ifelse(df$outs_overdue_ratio_total<=0.21,"0.01_0.21","more_0.21"))))
df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
df <- within(df, outs_overdue_ratio_total <- relevel(outs_overdue_ratio_total, 
   ref = "0_0.01"))
gb_index_fct(df, df$outs_overdue_ratio_total, "default_flag")

# Whatsapp registered : YES 
gb_index_fct(df, df$whatsapp, "default_flag")
df$whatsapp_cut <- ifelse(is.na(df$whatsapp), "0",
    ifelse(df$whatsapp==0, "0",
    ifelse(df$whatsapp==1, "1", "0")))
df$whatsapp <- as.factor(df$whatsapp_cut)
df <- within(df, whatsapp <- relevel(whatsapp, 
     ref = "0"))
gb_index_fct(df, df$whatsapp, "default_flag")

# Viber registered : YES
gb_index_fct(df, df$viber, "default_flag")
df$viber_cut <- ifelse(is.na(df$viber), "1",
    ifelse(df$viber==0, "0",
    ifelse(df$viber==1, "1", "1")))
df$viber <- as.factor(df$viber_cut)
df <- within(df, viber <- relevel(viber, 
    ref = "1"))
gb_index_fct(df, df$viber, "default_flag")

# SKYPE registered : YES
gb_index_fct(df, df$skype.registered, "default_flag")
df$skype.registered_cut <- ifelse(is.na(df$skype.registered), "other",
  ifelse(df$skype.registered=="FALSE", "other",
  ifelse(df$skype.registered=="TRUE", "TRUE", "other")))
df$skype.registered <- as.factor(df$skype.registered_cut)
df <- within(df, skype.registered <- relevel(skype.registered, 
  ref = "other"))
gb_index_fct(df, df$skype.registered, "default_flag")

# linkedin registered : YES
gb_index_fct(df, df$linkedin.registered, "default_flag")
df$linkedin.registered_cut <- ifelse(is.na(df$linkedin.registered), "other",
  ifelse(df$linkedin.registered=="FALSE", "other",
  ifelse(df$linkedin.registered=="TRUE", "TRUE", "other")))
df$linkedin.registered <- as.factor(df$linkedin.registered_cut)
df <- within(df, linkedin.registered <- relevel(linkedin.registered, 
  ref = "other"))
gb_index_fct(df, df$linkedin.registered, "default_flag")

# facebook registered : YES
gb_index_fct(df, df$facebook.registered, "default_flag")
df$facebook.registered_cut <- ifelse(is.na(df$facebook.registered), "other",
  ifelse(df$facebook.registered=="FALSE", "FALSE",
  ifelse(df$facebook.registered=="TRUE", "other", "other")))
df$facebook.registered <- as.factor(df$facebook.registered_cut)
df <- within(df, facebook.registered <- relevel(facebook.registered, 
  ref = "other"))
gb_index_fct(df, df$facebook.registered, "default_flag")

# haveibeenpwned : YES
gb_index_fct(df, df$haveibeenpwned_listed, "default_flag")
df$haveibeenpwned_listed_cut <- ifelse(is.na(df$haveibeenpwned_listed), "other",
   ifelse(df$haveibeenpwned_listed=="FALSE", "other",
   ifelse(df$haveibeenpwned_listed=="TRUE", "TRUE", "other")))
df$haveibeenpwned_listed <- as.factor(df$haveibeenpwned_listed_cut)
df <- within(df, haveibeenpwned_listed <- relevel(haveibeenpwned_listed, 
   ref = "other"))
gb_index_fct(df, df$haveibeenpwned_listed, "default_flag")

# Fraud score manual 2 : YES
gb_index_fct(df, df$seon_var, "default_flag")
plot_infotables(IV, "seon_var")
df$seon_var_cut <- ifelse(is.na(df$seon_var), "1_2",
  ifelse(df$seon_var %in% c(0),"0",
  ifelse(df$seon_var %in% c(1:3) ,"1_2","more_4")))
df$seon_var <- as.factor(df$seon_var_cut)
df <- within(df, seon_var <- relevel(seon_var,ref = "1_2"))
gb_index_fct(df, df$seon_var, "default_flag")

# Fraud score manual  : YES
gb_index_fct(df, df$seon_var2, "default_flag")
df$seon_var2_cut <- ifelse(is.na(df$seon_var2), "more_1",
   ifelse(df$seon_var2 %in% c(0),"0","more_1"))
df$seon_var2 <- as.factor(df$seon_var2_cut)
df <- within(df, seon_var2 <- relevel(seon_var2,ref = "more_1"))
gb_index_fct(df, df$seon_var2, "default_flag")

# API payment method : YES
gb_index_fct(df, df$API_payment_method, "default_flag")
df$API_payment_method_cut <- ifelse(is.na(df$API_payment_method), "other",
  ifelse(df$API_payment_method==2, "2",
  ifelse(df$API_payment_method==3, "other", "other")))
df$API_payment_method <- as.factor(df$API_payment_method_cut)
df <- within(df,API_payment_method <- relevel(API_payment_method,ref = "other"))
gb_index_fct(df, df$API_payment_method, "default_flag")

# Location is in bad neighborhood : YES 
gb_index_fct(df, df$flag_location_offi, "default_flag")
df$flag_location_offi_cut <- ifelse(
  is.na(df$flag_location_offi), "other",
  ifelse(df$flag_location_offi==1,"1","other"))
df$flag_location_offi <- as.factor(df$flag_location_offi_cut)
df <- within(df, flag_location_offi <- relevel(
  flag_location_offi, ref = "other"))
gb_index_fct(df, df$flag_location_offi, "default_flag")

# Email domain : NO 
df$API_email_domain_cut <- 
  ifelse(is.na(df$API_email_domain), "NA",
  ifelse(df$API_email_domain=="abv.bg", "abv",
  ifelse(df$API_email_domain=="gmail.com", "gmail",
  ifelse(df$API_email_domain=="mail.bg", "mailbg","other"))))
gb_index_fct(df, df$API_email_domain_cut, "default_flag") 

# Facebook domain : NO 
gb_index_fct(df, df$facebook_registered, "default_flag") 
df$facebook_registered_cut <- 
  ifelse(is.na(df$facebook_registered), "other",
   ifelse(df$facebook_registered=="True", "True","other"))
df$facebook_registered <- as.factor(df$facebook_registered_cut)
df <- within(df, facebook_registered <- relevel(
  facebook_registered, ref = "other"))
gb_index_fct(df, df$facebook_registered, "default_flag") 

# Viber phone
gb_index_fct(df, df$viber_phone, "default_flag") 
df$viber_phone_cut <- 
  ifelse(is.na(df$viber_phone), "other",
  ifelse(df$viber_phone=="False", "False","other"))
df$viber_phone <- as.factor(df$viber_phone_cut)
df <- within(df, viber_phone <- relevel(
  viber_phone, ref = "other"))
gb_index_fct(df, df$viber_phone, "default_flag") 

# Whatsapp phone
gb_index_fct(df, df$whatsapp_phone, "default_flag") 
df$whatsapp_phone_cut <- 
  ifelse(is.na(df$whatsapp_phone), "other",
         ifelse(df$whatsapp_phone=="True", "True","other"))
df$whatsapp_phone <- as.factor(df$whatsapp_phone_cut)
df <- within(df, whatsapp_phone <- relevel(
  whatsapp_phone, ref = "other"))
gb_index_fct(df, df$whatsapp_phone, "default_flag") 

# Facebook phone
gb_index_fct(df, df$facebook_phone, "default_flag") 
df$facebook_phone_cut <- 
  ifelse(is.na(df$facebook_phone), "other",
         ifelse(df$facebook_phone=="True", "True","other"))
df$facebook_phone <- as.factor(df$facebook_phone_cut)
df <- within(df, facebook_phone <- relevel(
  facebook_phone, ref = "other"))
gb_index_fct(df, df$facebook_phone, "default_flag") 

# Instagram phone
gb_index_fct(df, df$instagram_phone, "default_flag") 
df$instagram_phone_cut <- 
  ifelse(is.na(df$instagram_phone), "other",
         ifelse(df$instagram_phone=="True", "True","other"))
df$instagram_phone <- as.factor(df$instagram_phone_cut)
df <- within(df, instagram_phone <- relevel(
  instagram_phone, ref = "other"))
gb_index_fct(df, df$instagram_phone, "default_flag")

# Skype phone
gb_index_fct(df, df$skype_phone, "default_flag") 
df$skype_phone_cut <- 
  ifelse(is.na(df$skype_phone), "other",
         ifelse(df$skype_phone=="True", "True","other"))
df$skype_phone <- as.factor(df$skype_phone_cut)
df <- within(df, skype_phone <- relevel(
  skype_phone, ref = "other"))
gb_index_fct(df, df$skype_phone, "default_flag")






#########################################################
# Choose variable to be enter logistic regression model #
#########################################################

list_var_model <-  c("default_flag",
   "age",
   "education",
   "marital_status",
   "status_work",
   "experience_employer",
    
   "status_finished_total",
   "outs_overdue_ratio_total",
   
   "viber_phone",
   
   "API_payment_method",
   "API_period"

)
df_select <- df[,list_var_model]

corr_df <- dummy.data.frame(df_select, sep = "_")
cor(corr_df)
write.xlsx((abs(cor(corr_df))),"data\\correlation_report.xlsx")

mat_corr <- as.matrix(abs(cor(corr_df)))
mat_corr <- mat_corr[2:nrow(mat_corr),2:ncol(mat_corr)]
corr_report <- numeric(1000)
k <- 0
for(i in 1:nrow(mat_corr)){
  for(j in 1:ncol(mat_corr)){
    if(mat_corr[i,j]>0.4 & i<j){
      corr_report[k] <- paste(rownames(mat_corr)[i],
                              colnames(mat_corr)[j],sep=",")
      k <- k + 1}}
}
corr_report  <- corr_report[seq_len(k-1)]
View(corr_report)
write.xlsx(corr_report,"data\\correlation_report_SHORT.xlsx")



#########################################################
# Separate into 80% Training and 20% Testing            #
#########################################################

split <- sample.split(df_select$default_flag, SplitRatio = 0.8)
df80 <- subset(df_select, split == TRUE)
df20 <- subset (df_select, split == FALSE)



#####################################
# Carry-out stepwise regression     #
#####################################

df_Log_Credirect_App_installments <- glm(default_flag ~ ., family=binomial, 
  data=df80)
step(df_Log_Credirect_App_installments,direction = c("backward"))



#####################################
# Model with lositic regression     #
#####################################

# Summary of coefficients of Logistic Regression
summary(df_Log_Credirect_App_installments)

# Output coefficients
coefficients <- as.data.frame(
  cbind(names(df_Log_Credirect_App_installments$coefficients),
  df_Log_Credirect_App_installments$coefficients))
write.xlsx(coefficients,"results\\Coefficients.xlsx")

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
df80$score <- predict(df_Log_Credirect_App_installments, newdata=df80, 
                      type="response")
forecast <- predict(df_Log_Credirect_App_installments, newdata=df80, 
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
forecast2 <- predict(df_Log_Credirect_App_installments, newdata=df20, 
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
df$score <- predict(df_Log_Credirect_App_installments, newdata=df, 
                    type="response")



##########################################
# User chooses bins for rest of analysis #
##########################################
bins <- c(0.05,0.2,0.25,0.275,0.3,
          0.325,0.35,0.375,0.4,0.425,0.45,0.5,0.55,0.6,0.65,0.7,1)



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
View(ks(df_train,df_test))



######################
# Confusion Matrix   #
######################
# Get the confusion matrix and choose the cut-off point for bads
cut_off <- 0.6
predict <- ifelse(score_test>cut_off,1,0)
table(predict)[2]/sum(table(predict))*100
accuracy_table <- table(df20$default_flag,predict)
accuracy_table



##############################
# Output file for analysis   #
##############################
score_all <- predict(df_Log_Credirect_App_installments, newdata=df, 
                     type="response")
names(df)[length(df)] <- 'Score'
df$default_flag_predict <- ifelse(df$Score>0.5,1,0)
table(df$default_flag[df$date>="2019-06-22"],
      df$default_flag_predict[df$date>="2019-06-22"])
write.csv(df, "data\\Results_Logit_for_Analysis_Installments.csv")



################
# Save RData   #
################
save(df_Log_Credirect_App_installments, 
  file = "rdata\\Credirect_new_installments.rdata")



#############################################
# Choose characteristics on entire dataset  #
#############################################

# Compute confusion matrix on whole dataset
df_new <- df
score_all <- predict(df_Log_Credirect_App_installments, 
                     newdata=df_new , type="response")
cut_off <- 0.2
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
score_all <- predict(df_Log_Credirect_App_installments, 
                     newdata=df_new , type="response")
df_new <- df
df_new <- cbind(df,score_all)
df_new$Score <- cut(df_new$score_all,bins)
df_new <- merge(df_new, df_raw[,c("credit_number","product_cat")], 
                by.x = "credit_number", by.y = "credit_number", all.x = TRUE)
ks(df_new,NA)


