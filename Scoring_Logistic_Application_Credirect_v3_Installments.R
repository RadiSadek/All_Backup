
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
set.seed(10)

# Load data #
main_dir <- "C:\\Projects\\Application_Scoring\\Credirect_v3\\data\\"
setwd(main_dir)
df <- read.csv("input_current.csv", sep=",")
df <- subset(df,df$product_cat=="Credirect_User")
df$date <- as.Date(df$date)
df_raw <- df

# Read results for emails
email_result <- read.csv(paste(main_dir,
                               "SEON_emails_v1.csv",sep=""),
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
phone_result1 <- read.csv(paste(main_dir,"SEON_phones_v1.csv",sep=""),sep=",")
phone_result2 <- read.csv(paste(main_dir,"SEON_phones_v2.csv",sep=""),sep=",")
phone_result2 <- phone_result2[,-which(names(phone_result2) %in%
                                         c("telegram_registered"))]
phone_result <- rbind(phone_result1,phone_result2)
phone_result <- phone_result[!duplicated(phone_result$phone_number),]
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

# Check consistency of phone variables
check <- df[,c("credit_number","date","pers_number_1","email",
               "whatsapp_registered","viber_registered","email_exists")]
View(check)
check$whatsapp <- ifelse(check$whatsapp_registered=="False" | 
      is.na(check$whatsapp_registered) ,0,1)
check$viber <- ifelse(check$viber_registered=="False" | 
      is.na(check$viber_registered),0,1)
check <- check[order(check$date),]
check$month <- substring(check$date,1,7)
check$whatsapp_cum <- 0
check$viber_cum <- 0
for(i in 2:nrow(check)){
   check$whatsapp_cum[i] <- check$whatsapp_cum[i-1] +  check$whatsapp[i]
   check$viber_cum[i] <- check$viber_cum[i-1] +  check$viber[i]
}
plot(check$viber_cum)
plot(check$whatsapp_cum)


#########################
# Select relevant data  #
#########################

df <- df[!duplicated(df$credit_number),]
all_cols <- c("egn","credit_number","date","default_flag", 
    "age","maturity","gender","ratio_installment_income",
    "ownership","education","household_total","on_address",
    "experience_employer","marital_status", "purpose", "status_work",
    "status_active_total","status_finished_total","monthly_installment_total",
    "source_entity_count_total","amount_drawn_total","cred_count_total",
     "outs_overdue_ratio_total",names(df)[c(89:125)])
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


# Maturity : OK
plot_infotables(IV, "maturity")
df$maturity_cut <- ifelse(df$maturity<=2,"2",
   ifelse(df$maturity<=3,"3",
   ifelse(df$maturity<=9,"4_9","more_10")))
df$maturity <- as.factor(df$maturity_cut)
df <- within(df, maturity <- relevel(maturity, ref = "4_9"))
gb_index_fct(df, df$maturity, "default_flag")

# Ratio installment income : OK
plot_infotables(IV, "ratio_installment_income")
df$ratio_installment_income_cut <- 
    ifelse(is.na(df$ratio_installment_income), "more_0.11",
    ifelse(df$ratio_installment_income<=0.11,"less_0.11","more_0.11"))
df$ratio_installment_income <- as.factor(df$ratio_installment_income_cut)
df <- within(df, ratio_installment_income <- 
    relevel(ratio_installment_income, ref = "more_0.11"))
gb_index_fct(df, df$ratio_installment_income, "default_flag")

# Age : OK
plot_infotables(IV, "age")
df$age_cut <- ifelse(df$age<=24,"24_less",
     ifelse(df$age<=39,"25_39",
     ifelse(df$age<=49,"40_49","more_50")))
df$age <- as.factor(df$age_cut)
df <- within(df, age <- relevel(age, ref = "25_39"))
gb_index_fct(df, df$age, "default_flag")

# Gender : OK 
plot_infotables(IV, "gender")
df$gender <- as.factor(df$gender)
df <- within(df, gender <- relevel(gender, ref = "1"))
gb_index_fct(df, df$gender, "default_flag")

# Ownership : OK 
plot_infotables(IV, "ownership")
df$ownership_cut <- ifelse(df$ownership %in% c(1), "1", 
   ifelse(is.na(df$ownership), "2_3", 
   ifelse(df$ownership %in% c(4), "4", "2_3")))
df$ownership <- as.factor(df$ownership_cut)
df <- within(df, ownership <- relevel(ownership, ref = "2_3"))
gb_index_fct(df, df$ownership, "default_flag")

# Education : OK
plot_infotables(IV, "education")
df$education_cut <- ifelse(is.na(df$education),"2",
   ifelse(df$education==1,"1",
   ifelse(df$education %in% c(3,4), "3_4", "2")))
df$education <- as.factor(df$education_cut)
df <- within(df, education <- relevel(education, ref = "2"))
gb_index_fct(df, df$education, "default_flag")

# Marital Status : OK
plot_infotables(IV, "marital_status")
df$marital_status_cut <- ifelse(is.na(df$marital_status), "not_1",
    ifelse(df$marital_status %in% c(2,3,4,5), "not_1", df$marital_status))
df$marital_status <- as.factor(df$marital_status_cut)
df <- within(df, marital_status <- relevel(marital_status, 
       ref =  "not_1"))
gb_index_fct(df, df$marital_status, "default_flag")

# Household total : NO
plot_infotables(IV, "household_total")
df$household_total_cut <- ifelse(is.na(df$household_total), "not_1",
 ifelse(df$household_total==1,"1","not_1"))
df$household_total <- as.factor(df$household_total_cut)
df <- within(df, household_total <- relevel(household_total, ref = "not_1"))
gb_index_fct(df, df$household_total, "default_flag")

# Experience employer : OK
plot_infotables(IV, "experience_employer")
df$experience_employer_cut <- ifelse(is.na(df$experience_employer), "less_36",
  ifelse(df$experience_employer<=2,"less_36",
  ifelse(df$experience_employer<=36,"less_36","36_more")))
df$experience_employer <- as.factor(df$experience_employer_cut)
df <- within(df, experience_employer <- relevel(experience_employer, 
  ref = "less_36"))
gb_index_fct(df, df$experience_employer, "default_flag")
                      
# Purpose : NO
plot_infotables(IV, "purpose")
df$purpose_cut <- ifelse(is.na(df$purpose), "1_6",
  ifelse(df$purpose %in% c(2,3,4,5,7),"2_3_4_5_7",
  ifelse(df$purpose==1,"1_6",
  ifelse(df$purpose==6,"1_6","1_6"))))
df$purpose <- as.factor(df$purpose_cut)
df <- within(df, purpose <- relevel(purpose, ref = "1_6"))
gb_index_fct(df, df$purpose, "default_flag")

# Status work : OK
plot_infotables(IV, "status_work")
df$status_work_cut <- ifelse(is.na(df$status_work), "other",
  ifelse(df$status_work %in% c(4,5), "4_5","other"))
df$status_work <- as.factor(df$status_work_cut)
df <- within(df, status_work <- relevel(status_work, ref = "other"))
gb_index_fct(df, df$status_work, "default_flag")

# Status finished total : OK 
plot_infotables(IV, "status_finished_total")
df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
   "0_71_72_73",
  ifelse(df$status_finished_total %in% c(0,71,72,73),"0_71_72_73", "74_75"))
df$status_finished_total <- as.factor(df$status_finished_total_cut)
df <- within(df, status_finished_total <- relevel(status_finished_total,
  ref = "0_71_72_73"))
gb_index_fct(df, df$status_finished_total, "default_flag")

# Status active total : OK
plot_infotables(IV, "status_active_total")
df$status_active_total_cut <- ifelse(is.na(df$status_active_total),
                                     "no_dpd_71_72_73",
  ifelse(df$status_active_total %in% c(0), "no_dpd_71_72_73",
  ifelse(df$status_active_total %in% c(71,72,73),"no_dpd_71_72_73", 
         "no_credit_74_75")))
df$status_active_total <- as.factor(df$status_active_total_cut)
df <- within(df, status_active_total <- relevel(status_active_total, 
          ref = "no_dpd_71_72_73"))
gb_index_fct(df, df$status_active_total, "default_flag")

# Source entity count : OK 
plot_infotables(IV, "source_entity_count_total")
df$source_entity_count_total_cut <- ifelse(is.na(df$source_entity_count_total),
  "more_2",
  ifelse(df$source_entity_count_total==0, "0",
  ifelse(df$source_entity_count_total==1, "1","more_2")))
df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
df <- within(df, source_entity_count_total <- relevel(source_entity_count_total, 
       ref = "more_2"))
gb_index_fct(df, df$source_entity_count_total, "default_flag")                                     
             
# Cred count total : OK 
plot_infotables(IV, "cred_count_total")
df$cred_count_total_cut <- ifelse(is.na(df$cred_count_total), "other",
  ifelse(df$cred_count_total==0,"0",
  ifelse(df$cred_count_total==1,"1","more_2")))
df$cred_count_total <- as.factor(df$cred_count_total_cut)
df <- within(df, cred_count_total <- relevel(cred_count_total, 
    ref = "more_2"))
gb_index_fct(df, df$cred_count_total, "default_flag")                                     

# Ratio : OK
plot_infotables(IV, "outs_overdue_ratio_total")
df$outs_overdue_ratio_total_cut <- ifelse(
  is.na(df$outs_overdue_ratio_total), "0_0.01",
  ifelse(df$outs_overdue_ratio_total==-999,"0_0.01",
  ifelse(df$outs_overdue_ratio_total<=0.01,"0_0.01",
  ifelse(df$outs_overdue_ratio_total<=0.08,"0.01_0.08",
  ifelse(df$outs_overdue_ratio_total<=1,"0.08_1","more_0.08")))))
df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
df <- within(df, outs_overdue_ratio_total <- relevel(outs_overdue_ratio_total, 
  ref = "0.01_0.08"))
gb_index_fct(df, df$outs_overdue_ratio_total, "default_flag")

# Whatsapp registered : Yes 
gb_index_fct(df, df$whatsapp_registered, "default_flag")
df$whatsapp_registered_cut <- ifelse(is.na(df$whatsapp_registered), "other",
    ifelse(df$whatsapp_registered=="False", "other",
    ifelse(df$whatsapp_registered=="True", "True", "other")))
df$whatsapp_registered <- as.factor(df$whatsapp_registered_cut)
df <- within(df, whatsapp_registered <- relevel(whatsapp_registered, 
                                                ref = "other"))
gb_index_fct(df, df$whatsapp_registered, "default_flag")

# Viber registered : YES
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

list_var_model_cor <-  c("default_flag","age","gender",
   "ownership","education","purpose", "household_children",
   "experience_employer","marital_status",
   "status_work","status_finished_total","status_active_total",
   "source_entity_count_total","cred_count_total",
   "amount_drawn_total","monthly_installment_total",
   "outs_overdue_ratio_total", "email_score","email_exists",
   "facebook_exists","google_exists","microsoft_exists",
   "instagram_exists")
list_var_model <-  c("default_flag","age","education",
   "marital_status","status_work",
   "maturity",
   "status_finished_total", 
   "source_entity_count_total","outs_overdue_ratio_total",
   #"email_exists",
   #"email_score"
   "viber_registered"
   )
df_select <- df[,list_var_model]
corr_df <- dummy.data.frame(df[,list_var_model], sep = "_")
View(round(abs(cor(corr_df)),2))


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
bins <- c(0,0.05,
          0.1,.15,0.2,0.25,0.275,0.3,
          0.325,0.35,0.375,0.4,0.425,0.45,0.5,0.55,0.6,0.65,1)



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
cut_off <- 0.2
predict <- ifelse(score_test>cut_off,1,0)
table(predict)[2]/sum(table(predict))*100
accuracy_table <- table(df20$default_flag,predict)
accuracy_table



##############################
# Output file for analysis   #
##############################
score_all <- predict(df_Log_Credirect_App_installments, newdata=df, type="response")
df_anal <- cbind(df,score_all)
names(df_anal)[length(df_anal)] <- 'Score'
df_anal <- merge(df_anal, 
    df_raw[,c("credit_number","product_cat","amount_paid","amount")], 
    by.x = "credit_number", by.y = "credit_number", all.x = TRUE)
df_anal$default_flag_predict <- ifelse(df_anal$Score>0.3,1,0)
#write.xlsx(df_anal[,c("credit_number","amount","amount_paid","default_flag",
#    "Score")],"Predicted_Default_Flag.xlsx")
write.csv(df_anal, "Results_Logit_for_Analysis_Installments.csv")



################
# Save RData   #
################
save(df_Log_Credirect_App_installments, file = "Credirect_new_installments.rdata")



#############################################
# Choose characteristics on entire dataset  #
#############################################

# Compute confusion matrix on whole dataset
df_new <- df
score_all <- predict(df_Log_Credirect_App_installments, newdata=df_new , type="response")
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
score_all <- predict(df_Log_Credirect_App_installments, newdata=df_new , type="response")
df_new <- df
df_new <- cbind(df,score_all)
df_new$Score <- cut(df_new$score_all,bins)
df_new <- merge(df_new, df_raw[,c("credit_number","product_cat")], 
                by.x = "credit_number", by.y = "credit_number", all.x = TRUE)
ks(df_new,NA)





######### END #############

