

##################################################
#          CITY CASH REPEAT - VERSION 2          #
# PROBABILITY OF DEFAULT - LOGTISTIC REGRESSION  #
##################################################


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
library(My.stepwise)
library(Information)


###################################################
# Load data, correct fields and define functions  #
###################################################

## This is to make sure we generate always the same random numbers ##
set.seed(1)

# Function for taking only complete rows 
complete_fct <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
# Function to count number of missing values
count_empty <- function(var){
  return(sum(is.na(var)))
}

# Load data #
setwd("C:\\Projects\\Application_Scoring\\CityCash_v3\\data\\")
df <- read.csv("input.csv", sep=",")
df_raw <- df

# Remove self-approval offices  ##??????????????????????????????????????????????????????????????
#df <- subset(df,df$self_approve==0)

# Make new CKR data
df$cession_bin <- ifelse(
  is.na(df$cession_total), "none",
  ifelse(df$cession_total==0,"0","cession"))
df$new_status_finished_total <- 
  ifelse(is.na(df$status_finished_total),NA,
  ifelse(df$status_finished_total==0 & df$cession_bin=="cession",1,
  ifelse(df$status_finished_total==0 & df$cession_bin=="0",0,
  ifelse(df$status_finished_total==71 & df$cession_bin=="cession",711,
  ifelse(df$status_finished_total==71 & df$cession_bin=="0",71,
  ifelse(df$status_finished_total==72 & df$cession_bin=="cession",721,
  ifelse(df$status_finished_total==72 & df$cession_bin=="0",72,
  ifelse(df$status_finished_total==73 & df$cession_bin=="cession",731,
  ifelse(df$status_finished_total==74 & df$cession_bin=="0",73,
  ifelse(df$status_finished_total==74 & df$cession_bin=="cession",741,
  ifelse(df$status_finished_total==75 & df$cession_bin=="0",74,
  ifelse(df$status_finished_total==75 & df$cession_bin=="cession",751,75
          ))))))))))))
                      
# Function to calculate GB index
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

# Function for calculating Kolmogorov-Smirnoff coefficient
ks <- function(def, def_test){
  df_train <- def
  df_test <- def_test
  df_ks <- data.frame(t(table(
    df_train$default_flag,df_train$Score))[,1])
  names(df_ks)[1] <- "True_Goods_Train"
  df_ks$True_Bads_Train <-t(table(
    df_train$default_flag,df_train$Score))[,2]
  if(!is.na(df_test)){
    df_ks$True_Goods_Test <- t(table(
      df_test$default_flag,df_test$Score))[,1]
    df_ks$True_Bads_Test <- t(table(
      df_test$default_flag,df_test$Score))[,2]}
  
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
    df_ks$Cumulative_Goods_Train_Pct[i]=
      df_ks$Cumulative_Goods_Train[i]/
      df_ks$Cumulative_Goods_Train[nrow(df_ks)]
    df_ks$Cumulative_Bads_Train_Pct[i]=
      df_ks$Cumulative_Bads_Train[i]/
      df_ks$Cumulative_Bads_Train[nrow(df_ks)]
    if(!is.na(df_test)){
      df_ks$KS_Test[i] <- df_ks$Cumulative_Goods_Train_Pct[i]-
        df_ks$Cumulative_Goods_Train_Pct[i]
      df_ks$Cumulative_Goods_Test_Pct[i]=
        df_ks$Cumulative_Goods_Test[i]/
        df_ks$Cumulative_Goods_Test[nrow(df_ks)]
      df_ks$Cumulative_Bads_Test_Pct[i]=
        df_ks$Cumulative_Bads_Test[i]/
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

# Function to calculate R square
rsq <- function(x, y) summary(lm(y~x))$r.squared

# Select relevant data
df$date <- as.Date(df$date)
df <- df[with(df, order(date)), ]

# Rearrange some fields
df$total_income <- ifelse(df$total_income>=10000, 10000, df$total_income)

# Check data which was used for modeling 
list_all_var_model <-  c("credit_number","default_flag", "date",
  "amount_paid", "amount",
  "ownership","education","gender","age", "marital_status", "status_work", 
  "purpose","household_children",
  "experience_employer","on_address","total_income",
  "ratio_installment_income",
  "maturity",
  "status_finished_total", "new_status_finished_total",
  "status_active_total",
  "cred_count_total","monthly_installment_total",
  "source_entity_count_total","outs_overdue_ratio_total",
  "cession_total","codebtor_tot","guarantor_tot","same_city",
  "pop_city_current")
df <- df[,c(list_all_var_model)]



######################################
# Create tables of Information Value #
######################################

# Factor variables
cols_fac <- c("education","gender","marital_status","ownership","purpose",
              "status_work","status_active_total","status_finished_total")
df[cols_fac] <- lapply(df[cols_fac], factor)
create_infotables(data=df, y="default_flag", bins=5,parallel=FALSE)$Tables

# Create an Information Value table
IV <- create_infotables(data=df, y="default_flag", bins=10, parallel=FALSE) 



###################################
# Check validity of CKR variables #
###################################

df$outs <- ifelse(is.na(df$outs_overdue_ratio_total),"no_ckr",
   ifelse(df$outs_overdue_ratio_total==-999,"no_credit",
   ifelse(df$outs_overdue_ratio_total==0,"zero ratio",">=0 ratio")))
table(df$status_active_total,df$outs, exclude=NULL)
table(df$status_finished_total,df$outs, exclude=NULL)
table(df$status_active_total,df$default_flag)




############################
# Fine and coarse classing #
############################

# Same City : YES
gb_index_fct(df,df$same_city,"default_flag")
df$same_city <- as.factor(df$same_city)
df <- within(df, same_city <- relevel(same_city, ref = "0"))

# Ownership : YES
gb_index_fct(df, df$ownership, "default_flag")
df$ownership <- ifelse(is.na(df$ownership),"2_4",
                ifelse(df$ownership %in% c(1,3),"1_3","2_4"))
df$ownership <- as.factor(df$ownership)
df <- within(df, ownership<- relevel(ownership, ref = "1_3"))
gb_index_fct(df, df$ownership, "default_flag")

# Education : YES
gb_index_fct(df, df$education, "default_flag")
df$education_cut <- ifelse(is.na(df$education), "2",
      ifelse(df$education==1,"1",
      ifelse(df$education==2,"2","3_4")))   
df$education <- as.factor(df$education_cut)
df <- within(df, education <- relevel(education, ref = "2"))
gb_index_fct(df, df$education, "default_flag")

# Gender : YES
gb_index_fct(df, df$gender,"default_flag")
df$gender <- as.factor(df$gender)
df <- within(df, gender <- relevel(gender, ref = "1"))
gb_index_fct(df, df$gender, "default_flag")

# Marital status : YES
gb_index_fct(df, df$marital_status,"default_flag")
df$marital_status_cut <- ifelse(is.na(df$marital_status),"2_3", 
    ifelse(df$marital_status %in% c(2,3), "2_3",
    ifelse(df$marital_status==4,"1_4",
    ifelse(df$marital_status==1,"1_4","2_3"))))
df$marital_status <- as.factor(df$marital_status_cut)
df <- within(df, marital_status<- relevel(marital_status, ref = "2_3"))
gb_index_fct(df, df$marital_status, "default_flag")

# Status work : YES
gb_index_fct(df, df$status_work,"default_flag")
df$status_work_cut <- ifelse(is.na(df$status_work), "other",
    ifelse(df$status_work %in% c(2,3,4,7,8,9),"2_3_4_7_8_9",
    ifelse(df$status_work==5,"5",
    ifelse(df$status_work %in% c(1,6,10,11,12),"1_6_10_11_12","other"))))
df$status_work <- as.factor(df$status_work_cut)
df <- within(df, status_work <- relevel(status_work, ref = "2_3_4_7_8_9"))
gb_index_fct(df, df$status_work, "default_flag")

# Purpose : YES
plot_infotables(IV, "purpose")
df$purpose_cut <- ifelse(is.na(df$purpose), "not_2_3",
  ifelse(df$purpose %in% c(2,3),"2_3","not_2_3"))
df$purpose <- as.factor(df$purpose_cut)
df <- within(df, purpose <- relevel(purpose, ref = "not_2_3"))
gb_index_fct(df, df$purpose, "default_flag")

# Household Children : YES
plot_infotables(IV, "household_children")
df$household_children_cut <- ifelse(is.na(df$household_children), "0_1",
    ifelse(df$household_children %in% c(0,1),"0_1","more_2")) 
df$household_children <- as.factor(df$household_children_cut)
df <- within(df, household_children <- relevel(household_children, ref = "0_1"))
gb_index_fct(df, df$household_children, "default_flag")

# Maturity : YES
plot_infotables(IV, "maturity")
df$maturity_cut <- ifelse(df$maturity<=2, "less_2",
    ifelse(df$maturity<=4,"3_4","more_4"))
df$maturity <- as.factor(df$maturity_cut)
df <- within(df, maturity <- relevel(maturity, ref = "3_4"))
gb_index_fct(df, df$maturity, "default_flag")

# Age : YES
plot_infotables(IV, "age")
df$age_cut <- ifelse(df$age<=33, "less_33",
     ifelse(df$age<=45,"33_45","more_45"))      
df$age <- as.factor(df$age_cut)
df <- within(df, age <- relevel(age, ref = "33_45"))
gb_index_fct(df, df$age, "default_flag")

# Experience Employer : YES
plot_infotables(IV, "experience_employer")
df$experience_employer_cut <- ifelse(is.na(df$experience_employer),"9_120",
  ifelse(df$experience_employer<=9,"9_less",
  ifelse(df$experience_employer<=120,"9_120","120_more")))
df$experience_employer <- as.factor(df$experience_employer_cut)
df <- within(df, experience_employer <- relevel(experience_employer, 
          ref = "9_120"))
gb_index_fct(df, df$experience_employer, "default_flag")

# On Address : YES
plot_infotables(IV, "on_address")
df$on_address_cut <- ifelse(is.na(df$on_address),"36_336",
  ifelse(df$on_address<=36,"1_36",
  ifelse(df$on_address<=336,"36_336","more_336")))
df$on_address <- as.factor(df$on_address_cut)
df <- within(df, on_address <- relevel(on_address, ref = "36_336"))
gb_index_fct(df, df$on_address, "default_flag")

# Ratio installment income : YES
plot_infotables(IV, "ratio_installment_income")
df$ratio_installment_income_cut <- ifelse(
   is.na(df$ratio_installment_income),"300_less",
   ifelse(df$ratio_installment_income<=0.05,"less_0.05","more_0.05"))
df$ratio_installment_income <- as.factor(df$ratio_installment_income_cut)
df <- within(df, ratio_installment_income <- 
               relevel(ratio_installment_income, ref = "more_0.05"))
gb_index_fct(df, df$ratio_installment_income, "default_flag")

# Status active total : YES
gb_index_fct(df,df$status_active_total,"default_flag")
df$status_active_total_cut <- ifelse(is.na(df$status_active_total),"other",
  ifelse(df$status_active_total==0,"0",
  ifelse(df$status_active_total %in% c(75), "75","other")))
df$status_active_total <- as.factor(df$status_active_total_cut)
df <- within(df, status_active_total <- relevel(status_active_total, 
  ref = "other"))
gb_index_fct(df, df$status_active_total, "default_flag")

# Status finished total : YES
gb_index_fct(df,df$status_finished_total,"default_flag")
df$status_finished_total_cut <- ifelse(
  is.na(df$status_finished_total),"other",
  ifelse(df$status_finished_total %in% c(71,72), "71_72", "other"))
df$status_finished_total <- as.factor(df$status_finished_total_cut)
df <- within(df, status_finished_total <- relevel(status_finished_total, 
                            ref = "other"))
gb_index_fct(df, df$status_finished_total, "default_flag")

# Source entity count total : YES
plot_infotables(IV, "source_entity_count_total")
df$source_entity_count_total_cut <- 
   ifelse(is.na(df$source_entity_count_total), "4_less",
   ifelse(df$source_entity_count_total %in% c(0:4), "4_less","5_more"))
df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
df <- within(df, source_entity_count_total <- relevel(
  source_entity_count_total,ref = "4_less"))
gb_index_fct(df, df$source_entity_count_total, "default_flag")

# Cred count total : 
plot_infotables(IV, "cred_count_total")
df$cred_count_total_cut <- 
  ifelse(is.na(df$cred_count_total),"not_1",
  ifelse(df$cred_count_total %in% c(1),"1","not_1"))
df$cred_count_total <- as.factor(
  df$cred_count_total_cut)
df <- within(df, cred_count_total <- relevel(
  cred_count_total,ref = "not_1"))
gb_index_fct(df, df$cred_count_total, "default_flag")

# Ratio total : YES
plot_infotables(IV, "outs_overdue_ratio_total")
df$outs_overdue_ratio_total_cut <- ifelse(
  is.na(df$outs_overdue_ratio_total), "more_0.51",
   ifelse(df$outs_overdue_ratio_total==-999,"more_0.51",
   ifelse(df$outs_overdue_ratio_total<=0.09,"0.09_less",
   ifelse(df$outs_overdue_ratio_total<=0.51,"more_0.51","more_0.51"))))
df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
df <- within(df, outs_overdue_ratio_total <- relevel(
  outs_overdue_ratio_total, ref = "more_0.51"))
gb_index_fct(df, df$outs_overdue_ratio_total, "default_flag")

# If Cession : 
plot_infotables(IV, "cession_total")
df$cession_total_cut <- ifelse(
  is.na(df$cession_total), "none",
  ifelse(df$cession_total==0,"none","cession"))
df$cession_total <- as.factor(df$cession_total_cut)
df <- within(df, cession_total <- relevel(
  cession_total, ref = "none"))
gb_index_fct(df, df$cession_total, "default_flag")



#####################################################
# Choose variable to be enter beta regression model #
#####################################################
list_var_model <-  c("default_flag",
"ownership","marital_status",
"age","gender","education", 
#"household_children",
"status_work",
"purpose",
"maturity","ratio_installment_income",
"experience_employer","on_address",
"status_active_total",
"status_finished_total",
#"cred_count_total"
"source_entity_count_total",
"outs_overdue_ratio_total",
"pop_city_current"
#"same_city"
#"cession_total",
)

df_temp <- df
df_select <- df[,list_var_model]

corr_df <- dummy.data.frame(df_select, sep = "_")
cor(corr_df)
View(abs(cor(corr_df)))


###############################################
# Separate into 80% Training and 20% Testing  #
###############################################

split <- sample.split(df_select$default_flag, SplitRatio = 0.8)
df80 <- subset(df_select, split == TRUE)
df20 <- subset (df_select, split == FALSE)



###########################################################
# Carry-out stepwise regression with backward elimination #
###########################################################

df_Log_CityCash_App <- glm(default_flag ~ ., family=binomial, data=df80)
step(df_Log_CityCash_App, direction = c("backward"))



####################################
# Build logistic regression model  #
####################################

# Summary of coefficients of Logistic Regression
summary(df_Log_CityCash_App)

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
df80$score <- predict(df_Log_CityCash_App, newdata=df80, type="response")
forecast <- predict(df_Log_CityCash_App, newdata=df80, type="response")
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
forecast2 <- predict(df_Log_CityCash_App, newdata=df20, type="response")
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
df$score <- predict(df_Log_CityCash_App, newdata=df, type="response")

# R square of the model application
rsq(df80$default_flag,forecast)
rsq(df20$default_flag,forecast2)



##############
# Save RData #
##############
#save(df_Log_CityCash_App, file = "PD_Logistic_Application_CityCash.rdata")



##########################################
# User chooses bins for rest of analysis #
##########################################
bins <- c(0,0.0125,0.025,0.0375,0.05,0.0625,0.075,0.875,
          0.1,0.125,0.15,0.175,0.2,0.225,0.25,0.275,0.3,
          0.325,0.35,0.375,0.4,0.5,0.6)



##########################################################
# Kolmogorov-Smirnoff Test on training/testing datasets  #
##########################################################

score_train <- forecast
score_test <- forecast2
df_train <- cbind(df80,score_train)
df_test <- cbind(df20,score_test)
df_train$Score <- cut(df_train$score_train,bins)
df_test$Score <- cut(df_test$score_test,bins)
ks(df_train,df_test)



####################
# Confusion Matrix #
####################

# Get the confusion matrix and choose the cut-off point for bads
cut_off <- 0.2
predict <- ifelse(score_test>cut_off,1,0)
table(predict)[2]/sum(table(predict))*100
accuracy_table <- table(df20$default_flag,predict)
accuracy_table



###################################
# Output file for profit analysis #
###################################
score_all <- predict(df_Log_CityCash_App, newdata=df,
                     type="response")
df_anal <- cbind(df,score_all)
names(df_anal)[length(df_anal)] <- 'Score'
#write.csv(df_anal,"Results_Logit_for_Analysis.csv")



##############################################################
# Confusion matrix and Kolmogorov-Smirnoff on whole dataset  #
##############################################################

# Compute confusion matrix on whole dataset
df_new <- df
score_all <- predict(df_Log_CityCash_App, newdata=df_new , 
                     type="response")
cut_off <- 0.2
predict <- ifelse(score_all>cut_off,1,0)
table(predict)[2]/sum(table(predict))*100
accuracy_table <- table(df$default_flag,predict)
accuracy <- sum(diag(accuracy_table))/
  sum(accuracy_table)*100
sensitivity <-  accuracy_table[2,2]/
  (accuracy_table[2,2]+accuracy_table[2,1])
specificity <-  accuracy_table[1,1]/
  (accuracy_table[1,1]+accuracy_table[1,2])
tot_acc <- sensitivity + specificity
accuracy_table
accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)
accuracy

# Get KS test on all data set
score_all <- predict(df_Log_CityCash_App, newdata=df_new , 
                     type="response")
df_new <- df
df_new <- cbind(df,score_all)
df_new$Score <- cut(df_new$score_all,bins)
df_new <- merge(df_new, df_raw[,c("credit_number","product_cat")], 
                by.x = "credit_number", by.y = "credit_number", 
                all.x = TRUE)
ks(df_new,NA)


###########################
######### END #############
###########################


