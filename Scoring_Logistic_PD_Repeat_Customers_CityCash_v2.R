

##################################################
#          CITYCASH REPEAT - VERSION 2           #
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
library(openxlsx)


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
setwd("C:\\Projects\\Behavioral_Scoring\\cityCash_v2\\data\\")
df <- read.xlsx("input_citycash_v2.xlsx")
df <- subset(df, df$credits_cum>0)
df$date <- as.Date(df$date,  origin = "1900-01-01")
df_raw <- df 

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

# Correct some fields
df$days_diff_last_credit <- ifelse(df$days_diff_last_credit<0,0,
                                   df$days_diff_last_credit)
df$days_diff_last_credit <- ifelse(is.na(df$days_diff_last_credit),0,
                                   df$days_diff_last_credit)
df$flag_high_last_paid <- ifelse(df$days_diff_last_credit %in% c(0,1) & 
                                   df$ratio_last_paid>=0.5, 1, 0)
df$flag_high_last_paid <- ifelse(is.na(df$flag_high_last_paid), 0 , 
                                 df$flag_high_last_paid)
df$amount_diff <- df$amount - df$amount_prev
df$default_cum <- ifelse(df$credits_cum==0,NA,df$default_cum)

# Select relevant data
df <- df[with(df, order(date)), ]

# Rearrange some fields
df$total_income <- ifelse(df$total_income>=10000, 10000, df$total_income)

# Check data which was used for modeling 
list_all_var_model <-  c("credit_number","default_flag", "amount","amount_paid",
  "ownership","education","gender","age", "marital_status", "status_work", 
  "purpose","household_children", "maturity","experience_employer",
  "on_address","total_income","ratio_installment_income","credits_cum",
  "days_diff_last_credit","max_delay","amount_diff","prev_online",
  "flag_high_last_paid","maturity_ratio","ratio_nb_payments_prev",
  "refinance_ratio","status_finished_total","status_active_total",
  "cred_count_total","monthly_installment_total","source_entity_count_total",
  "outs_overdue_ratio_total","cession_total","codebtor_tot","guarantor_tot"
  )
df <- df[,c(list_all_var_model)]



######################################
# Create tables of Information Value #
######################################

# Factor variables
cols_fac <- c("education","gender","marital_status","ownership","purpose",
              "status_work","status_active_total","status_finished_total",
              "prev_online","cession_total","codebtor_tot","guarantor_tot",
              "flag_high_last_paid")
df[cols_fac] <- lapply(df[cols_fac], factor)
create_infotables(data=df, y="default_flag", bins=5,parallel=FALSE)$Tables

# Create an Information Value table
IV <- create_infotables(data=df, y="default_flag", bins=10, parallel=FALSE) 




############################
# Fine and coarse classing #
############################

# Ownership : YES
plot_infotables(IV, "ownership")
df$ownership <- ifelse(is.na(df$ownership),"2_3_4",df$ownership)
df$ownership <- ifelse(df$ownership==1, "1", "2_3_4")
df$ownership <- as.factor(df$ownership)
df <- within(df, ownership<- relevel(ownership, ref = "2_3_4"))
gb_index_fct(df, df$ownership, "default_flag")

# Education : YES 
plot_infotables(IV, "education")
df$education_cut <- ifelse(is.na(df$education) %in% c(2,3) ,"2_3",
    ifelse(df$education==1,"1",
    ifelse(df$education==4,"4","2_3")))
df$education <- as.factor(df$education_cut)
df <- within(df, education <- relevel(education, ref = "2_3"))
gb_index_fct(df, df$education, "default_flag")

# Gender : NO
plot_infotables(IV, "gender")
df$gender <- as.factor(df$gender)
df <- within(df, gender <- relevel(gender, ref = "1"))
gb_index_fct(df, df$gender, "default_flag")

# Marital status : YES
plot_infotables(IV, "marital_status")
df$marital_status <- ifelse(is.na(df$marital_status),
                            "1_2_3_4", df$marital_status)
df$marital_status <- ifelse(df$marital_status==5, "5", "1_2_3_4")
df$marital_status <- as.factor(df$marital_status)
df <- within(df, marital_status<- relevel(marital_status, 
       ref = "1_2_3_4"))

# Experience Employer : YES
plot_infotables(IV, "experience_employer")
df$experience_employer_cut <- 
  ifelse(is.na(df$experience_employer),"13_156", 
  ifelse(df$experience_employer<=12,"0_12",
  ifelse(df$experience_employer<=156,"13_156","more_156")))
df$experience_employer <- as.factor(df$experience_employer_cut)
df <- within(df, experience_employer<- relevel(experience_employer, 
      ref = "13_156"))

# On Address : YES
plot_infotables(IV, "on_address")
df$on_address_cut <- 
  ifelse(is.na(df$on_address),"more_24", 
  ifelse(df$on_address<=24,"0_24", "more_24"))
df$on_address <- as.factor(df$on_address_cut)
df <- within(df, on_address<- relevel(on_address, 
   ref = "more_24"))

# Status work : YES
plot_infotables(IV, "status_work")
df$status_work_cut <- ifelse(is.na(df$status_work), "other",
    ifelse(df$status_work %in% c(5,10) ,"5_10","other"))
df$status_work <- as.factor(df$status_work_cut)
df <- within(df, status_work <- relevel(status_work, ref = "other"))
gb_index_fct(df, df$status_work, "default_flag")

# Purpose : YES
plot_infotables(IV, "purpose")
df$purpose_cut <- ifelse(is.na(df$purpose), "other",
  ifelse(df$purpose %in% c(6), "6",
  ifelse(df$purpose %in% c(4), "4", 
  ifelse(df$purpose %in% c(2,5),"2_5","other"))))
df$purpose <- as.factor(df$purpose_cut)
df <- within(df, purpose <- relevel(purpose, ref = "other"))
gb_index_fct(df, df$purpose, "default_flag")

# Flag High Last Paid : Yes
plot_infotables(IV, "flag_high_last_paid")
df$flag_high_last_paid <- as.factor(df$flag_high_last_paid)
df <- within(df, flag_high_last_paid <- relevel(flag_high_last_paid, 
            ref = "0"))
gb_index_fct(df,df$flag_high_last_paid,"default_flag")

# Household Children : YES
plot_infotables(IV, "household_children")
df$household_children_cut <- ifelse(is.na(df$household_children), "0_1",
    ifelse(df$household_children<=1,"0_1", "more_1"))
df$household_children <- as.factor(df$household_children_cut)
df <- within(df, household_children <- relevel(household_children, ref = "0_1"))
gb_index_fct(df, df$household_children, "default_flag")

# Age : YES 
plot_infotables(IV, "age")
df$age_cut <- ifelse(df$age<=29,"less_29",
              ifelse(df$age<=37,"30_37",
              ifelse(df$age<=53,"38_53","more_54")))
df$age <- as.factor(df$age_cut)
df <- within(df, age <- relevel(age, ref = "38_53"))
gb_index_fct(df, df$age, "default_flag")

# Credits Cum : YES
plot_infotables(IV, "credits_cum")
df$credits_cum_cut <- ifelse(df$credits_cum<=1,"1",
                      ifelse(df$credits_cum<=5,"2_5",
                      ifelse(df$credits_cum<=8,"6_8","more_9")))
df$credits_cum <- as.factor(df$credits_cum_cut)
df <- within(df, credits_cum <- relevel(credits_cum , ref = "2_5"))
gb_index_fct(df, df$credits_cum, "default_flag")

# Prev Online : NO
plot_infotables(IV, "prev_online")
df$prev_online <- as.factor(df$prev_online)
df <- within(df, prev_online <- relevel(prev_online, ref = "1"))
gb_index_fct(df,df$prev_online,"default_flag")

# Days Diff Last Credit : YES
plot_infotables(IV, "days_diff_last_credit")
df$days_diff_last_credit_cut <- ifelse(is.na(df$days_diff_last_credit), 
      "3_more",
  ifelse(df$days_diff_last_credit<=2,"less_2","3_more"))
df$days_diff_last_credit <- as.factor(df$days_diff_last_credit_cut)
df <- within(df, days_diff_last_credit  <- relevel(days_diff_last_credit , 
      ref = "less_2"))
gb_index_fct(df, df$days_diff_last_credit, "default_flag")

# Max Delay : YES
plot_infotables(IV, "max_delay")
df$max_delay_cut <- ifelse(df$max_delay<=2,"less_2",
    ifelse(df$max_delay<=6,"3_6",
    ifelse(df$max_delay<=60,"7_60","60_more")))
df$max_delay <- as.factor(df$max_delay_cut)
df <- within(df, max_delay   <- relevel(max_delay, ref = "7_60"))
gb_index_fct(df,df$max_delay,"default_flag")

# Amount Diff : YES
plot_infotables(IV, "amount_diff")
df$amount_diff_cut <- ifelse(is.na(df$amount_diff),"0_250",
  ifelse(df$amount_diff<0,"less_0",
  ifelse(df$amount_diff>=300,"more_300","0_250")))
df$amount_diff <- as.factor(df$amount_diff_cut)
df <- within(df, amount_diff <- relevel(amount_diff,ref = "0_250"))
gb_index_fct(df,df$amount_diff,"default_flag")

# Maturity : YES
plot_infotables(IV, "maturity")
df$maturity_cut <- ifelse(is.na(df$maturity),"0.62_1.14",
   ifelse(df$maturity<=3.03,"less_3.03",
   ifelse(df$maturity<=6.3,"3.03_6.3","more_6.3")))
df$maturity <- as.factor(df$maturity_cut)
df <- within(df, maturity <- relevel(maturity, 
    ref = "3.03_6.3"))
gb_index_fct(df,df$maturity,"default_flag")

# Maturity Ratio : YES
plot_infotables(IV, "maturity_ratio")
df$maturity_ratio_cut <- ifelse(is.na(df$maturity_ratio),"0.62_1.14",
    ifelse(df$maturity_ratio<=0.62,"less_0.62",
    ifelse(df$maturity_ratio<=1.14,"0.62_1.14",
    ifelse(df$maturity_ratio<=1.48,"1.14_1.48","more_1.48"))))
df$maturity_ratio <- as.factor(df$maturity_ratio_cut)
df <- within(df, maturity_ratio <- relevel(maturity_ratio, 
      ref = "0.62_1.14"))
gb_index_fct(df,df$maturity_ratio,"default_flag")

# Number of payments of previous : YES
plot_infotables(IV, "ratio_nb_payments_prev")
df$ratio_nb_payments_prev_cut <- ifelse(is.na(df$ratio_nb_payments_prev),
           "less_0.49",
   ifelse(df$ratio_nb_payments_prev<=0.49,"less_0.49",
   ifelse(df$ratio_nb_payments_prev<=0.59,"0.49_0.59",
   ifelse(df$ratio_nb_payments_prev<=0.94,"0.59_0.94","more_0.94"))))
df$ratio_nb_payments_prev <- as.factor(df$ratio_nb_payments_prev_cut)
df <- within(df, ratio_nb_payments_prev <- relevel(ratio_nb_payments_prev, 
       ref = "less_0.49"))
gb_index_fct(df,df$ratio_nb_payments_prev,"default_flag")

# Refinance Ratio : YES
plot_infotables(IV, "refinance_ratio")
df$refinance_ratio_cut <- ifelse(is.na(df$refinance_ratio),"less_1",
   ifelse(df$refinance_ratio==1,"1","less_1"))
df$refinance_ratio <- as.factor(df$refinance_ratio_cut)
df <- within(df, refinance_ratio <- relevel(refinance_ratio, ref = "less_1"))
gb_index_fct(df,df$refinance_ratio,"default_flag")

# Status active total : YES
plot_infotables(IV, "status_active_total")
df$status_active_total_cut <- ifelse(is.na(df$status_active_total), 
           "no_credit_74_75",
    ifelse(df$status_active_total==0,"0",
    ifelse(df$status_active_total %in% c(71,72),"71_72", 
           "no_credit_74_75")))
df$status_active_total <- as.factor(df$status_active_total_cut)
df <- within(df, status_active_total <- relevel(status_active_total, 
  ref = "no_credit_74_75"))
gb_index_fct(df, df$status_active_total, "default_flag")

# Status finished total : YES 
plot_infotables(IV, "status_finished_total")
df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
                                       "other",
  ifelse(df$status_finished_total %in% c(71,72),"71_72","other"))
df$status_finished_total <- as.factor(df$status_finished_total_cut)
df <- within(df, status_finished_total <- relevel(status_finished_total, 
                            ref = "other"))
gb_index_fct(df, df$status_finished_total, "default_flag")

# Source entity count total : YES
plot_infotables(IV, "source_entity_count_total")
df$source_entity_count_total_cut <- ifelse(
 is.na(df$source_entity_count_total), "3_8",
 ifelse(df$source_entity_count_total<=2,"2_less",
 ifelse(df$source_entity_count_total<=8,"3_8","more_9")))
df$source_entity_count_total <- as.factor(
  df$source_entity_count_total_cut)
df <- within(df, source_entity_count_total <- relevel(
  source_entity_count_total,ref = "3_8"))
gb_index_fct(df, df$source_entity_count_total, "default_flag")

# Ratio total : YES
plot_infotables(IV, "outs_overdue_ratio_total")
df$outs_overdue_ratio_total_cut <- 
  ifelse(is.na(df$outs_overdue_ratio_total),"0.04_0.5",
  ifelse(df$outs_overdue_ratio_total==-999,"0.04_0.5",
  ifelse(df$outs_overdue_ratio_total<=0.04,"0.04_less",
  ifelse(df$outs_overdue_ratio_total<=0.5,"0.04_0.5",
  ifelse(df$outs_overdue_ratio_total<0.9,"0.5_0.9","more_0.9"))))) 
df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
df <- within(df, outs_overdue_ratio_total <- relevel(
  outs_overdue_ratio_total, ref = "0.04_0.5"))
gb_index_fct(df, df$outs_overdue_ratio_total, "default_flag")



#####################################################
# Choose variable to be enter beta regression model #
#####################################################
list_var_model <-  c("default_flag",
"education", 
"days_diff_last_credit","ratio_nb_payments_prev",
"amount_diff","max_delay","credits_cum","maturity","status_work")

df_temp <- df
df_select <- df[,list_var_model]

corr_df <- dummy.data.frame(df_select, sep = "_")
cor(corr_df)
write.xlsx((abs(cor(corr_df))),"correlation_report.xlsx")

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


###############################################
# Separate into 80% Training and 20% Testing  #
###############################################

split <- sample.split(df_select$default_flag, SplitRatio = 0.8)
df80 <- subset(df_select, split == TRUE)
df20 <- subset (df_select, split == FALSE)



###########################################################
# Carry-out stepwise regression with backward elimination #
###########################################################

df_Log_beh_CityCash <- glm(default_flag ~ ., family=binomial, data=df80)
step(df_Log_beh_CityCash, direction = c("backward"))



####################################
# Build logistic regression model  #
####################################

# Summary of coefficients of Logistic Regression
summary(df_Log_beh_CityCash)

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
df80$score <- predict(df_Log_beh_CityCash, newdata=df80, type="response")
forecast <- predict(df_Log_beh_CityCash, newdata=df80, type="response")
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
forecast2 <- predict(df_Log_beh_CityCash, newdata=df20, type="response")
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
df$score <- predict(df_Log_beh_CityCash, newdata=df, type="response")

# R square of the model application
rsq(df80$default_flag,forecast)
rsq(df20$default_flag,forecast2)



##############
# Save RData #
##############
save(df_Log_beh_CityCash, 
     file = "PD_Logistic_Behavioral_CityCash.rdata")



##########################################
# User chooses bins for rest of analysis #
##########################################
bins <- c(0,0.0125,0.025,0.0375,0.05,0.0625,0.075,0.0875,
          0.1,0.125,0.15,0.175,0.2,0.225,0.25,0.275,0.3,
          0.325,0.35,0.375,0.4,0.5,0.6,1)



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
score_all <- predict(df_Log_beh_CityCash, newdata=df,
                     type="response")
df_anal <- cbind(df,score_all)
names(df_anal)[length(df_anal)] <- 'Score'
write.csv(df_anal,"Results_Logit_for_Analysis.csv")



##############################################################
# Confusion matrix and Kolmogorov-Smirnoff on whole dataset  #
##############################################################

# Compute confusion matrix on whole dataset
df_new <- df
score_all <- predict(df_Log_beh_CityCash, newdata=df_new , 
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
score_all <- predict(df_Log_beh_CityCash, newdata=df_new , 
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


