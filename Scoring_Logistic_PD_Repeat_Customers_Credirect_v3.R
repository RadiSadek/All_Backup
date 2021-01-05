

#########################################################
#             CREDIRECT REPEAT - VERSION 3              #
# PREDICT PROBABILITY OF DEFAULT - LOGISTIC REGRESSION  #
#########################################################


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



#############
# Functions #
#############

# Function for taking only complete rows 
complete_fct <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
# Function to count number of missing values
count_empty <- function(var){
  return(sum(is.na(var)))
}
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



##############################
# Load data, correct fields  #
##############################

## This is to make sure we generate always the same random numbers ##
set.seed(1)

# set main directory
main_dir <- "C:\\Projects\\Behavioral_Scoring\\Credirect_v3\\data\\"

# Load data #
setwd(main_dir)
df <- read.csv("input_credirect.csv", sep=",")
df <- subset(df,df$credits_cum>0)
#df <- subset(df,df$product_name=="Credirect Installments")
df$date <- as.Date(df$date)
df_raw <- df 

# Select relevant data
df <- df[with(df, order(date)), ]

# Rearrange some fields
df$total_income <- ifelse(df$total_income>=10000, 10000, df$total_income)

# Check data which was used for modeling 
list_all_var_model <-  c("credit_number","egn","date","default_flag", 
  "amount_paid","ownership","education","gender","age", "marital_status", 
  "status_work", "purpose","household_total","maturity",
  "experience_employer","on_address","total_income",
  "ratio_installment_income","credits_cum",
  "days_diff_last_credit","max_delay","amount_diff",
  "prev_online","ratio_online","ratio_last_paid",
  "refinance_cum","maturity_ratio","refinance_ratio",
  "ratio_nb_payments_prev","first_credit","prev_credit",
  "flag_high_last_paid","flag_high_last_paid_ratio",
  "status_finished_total","status_active_total",
  "cred_count_total","monthly_installment_total",
  "source_entity_count_total","outs_overdue_ratio_total",
  "cession_total","codebtor_tot","guarantor_tot")
df <- df[,c(list_all_var_model)]




#######################
## Get phone numbers ##
#######################
phone_result1 <- read.csv(paste(main_dir,"SEON_phones_v1.csv",sep=""),sep=",")
phone_result2 <- read.csv(paste(main_dir,"SEON_phones_v2.csv",sep=""),sep=",")
phone_result3 <- read.csv(paste(main_dir,"SEON_phones_v3.csv",sep=""),sep=",")
phone_result2 <- phone_result2[,-which(names(phone_result2) %in%
                                         c("telegram_registered"))]
phone_result3 <- phone_result3[,-which(names(phone_result3) %in%
                                         c("telegram_registered"))]
phone_result <- rbind(phone_result1,phone_result2,phone_result3)
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

phone_result$whatsapp_regphoto <- 
  ifelse(is.na(phone_result$whatsapp_registered),"NA",
  ifelse(phone_result$whatsapp_registered=="False","No_Whatsapp",
  ifelse(phone_result$whatsapp_registered=="True" & 
         phone_result$whatsapp.photo==0,"Whatsapp_NoPhoto",
  ifelse(phone_result$whatsapp_registered=="True" & 
        phone_result$whatsapp.photo==1,"Whatsapp_Photo","NA"))))

phone_result$viber_regphoto <- 
  ifelse(is.na(phone_result$viber_registered),"NA",
  ifelse(phone_result$viber_registered=="False","No_viber",
  ifelse(phone_result$viber_registered=="True" & 
         phone_result$viber.photo==0,"viber_NoPhoto",
  ifelse(phone_result$viber_registered=="True" & 
         phone_result$viber.photo==1,"viber_Photo","NA"))))

phone_numbers <- read.xlsx(paste(main_dir,"all_phone_numbers.xlsx",sep=""))
df <- merge(df,phone_numbers[,c("credit_number","pers_number_1")],
            by.x = "credit_number", by.y = "credit_number",
            all.x = TRUE)
df <- merge(df,phone_result,
            by.x = "pers_number_1", by.y = "phone_number_corr",
            all.x = TRUE)

# Check consistency of phone variables
check <- df[,c("credit_number","date","pers_number_1","egn",
               "whatsapp_registered","viber_registered")]
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




######################################
# Create tables of Information Value #
######################################

# Factor variables
cols_fac <- c("education","gender","marital_status","ownership","purpose",
              "status_work","status_active_total","status_finished_total",
              "first_credit","prev_credit","cession_total",
              "codebtor_tot","guarantor_tot")
df[cols_fac] <- lapply(df[cols_fac], factor)
create_infotables(data=df, y="default_flag", bins=5,parallel=FALSE)$Tables

# Create an Information Value table
IV <- create_infotables(data=df, y="default_flag", bins=10, parallel=FALSE) 



############################
#  Make additional fields  #
############################

df$new_cred_old_city <- ifelse(df$prev_credit=="City Cash" & df$credits_cum==1,
                               1,0)



############################
# Fine and coarse classing #
############################

# Ownership : OK
gb_index_fct(df,df$ownership,"default_flag")
df$ownership_cut <- ifelse(is.na(df$ownership),"1_2_3",
    ifelse(df$ownership==4, "4", "1_2_3"))
df$ownership <- as.factor(df$ownership_cut)
df <- within(df, ownership<- relevel(ownership, ref = "1_2_3"))
gb_index_fct(df, df$ownership, "default_flag")

# Education : OK
gb_index_fct(df,df$education,"default_flag")
df$education_cut <- ifelse(is.na(df$education), "2_3",
    ifelse(df$education==1,"1",
    ifelse(df$education==4,"4","2_3")))
df$education <- as.factor(df$education_cut)
df <- within(df, education <- relevel(education, ref = "2_3"))
gb_index_fct(df, df$education, "default_flag")

# Gender : NO
gb_index_fct(df,df$gender,"default_flag")
df$gender <- as.factor(df$gender)
df <- within(df, gender <- relevel(gender, ref = "1"))
gb_index_fct(df, df$gender, "default_flag")

# Marital status : NO
gb_index_fct(df,df$marital_status,"default_flag")
df$marital_status_cut <- ifelse(is.na(df$marital_status),"1_2_3_4", 
    ifelse(df$marital_status==5, "5", "1_2_3_4"))
df$marital_status <- as.factor(df$marital_status_cut)
df <- within(df, marital_status<- relevel(marital_status, 
    ref = "1_2_3_4"))
gb_index_fct(df, df$marital_status, "default_flag")

# Status work : OK  
gb_index_fct(df,df$status_work,"default_flag")
df$status_work_cut <- ifelse(is.na(df$status_work), "other",
   ifelse(df$status_work %in% c(1,4,5),"1_4_5","other"))
df$status_work <- as.factor(df$status_work_cut)
df <- within(df, status_work <- relevel(status_work, ref = "other"))
gb_index_fct(df, df$status_work, "default_flag")

# Purpose : OK
gb_index_fct(df,df$purpose,"default_flag")
df$purpose_cut <- ifelse(is.na(df$purpose), "other",
   ifelse(df$purpose %in% c(2,4,5),"2_4_5","other"))
df$purpose <- as.factor(df$purpose_cut)
df <- within(df, purpose <- relevel(purpose, ref = "other"))
gb_index_fct(df, df$purpose, "default_flag")

# Household Total : NO 
plot_infotables(IV, "household_total")
df$household_total_cut <- ifelse(is.na(df$household_total), "other",
      ifelse(df$household_total>=4,"4_more", "other"))
df$household_total <- as.factor(df$household_total_cut)
df <- within(df, household_total <- relevel(household_total, ref = "other"))
gb_index_fct(df, df$household_total, "default_flag")

# Age :  OK 
plot_infotables(IV, "age")
df$age_cut <- ifelse(df$age<=23,"23_less",
              ifelse(df$age<50,"24_50","51_more"))
df$age <- as.factor(df$age_cut)
df <- within(df, age <- relevel(age, ref = "24_50"))
gb_index_fct(df, df$age, "default_flag")

# Experience Employer : OK
plot_infotables(IV, "experience_employer")
df$experience_employer_cut <- ifelse(is.na(df$experience_employer),"more_3",
    ifelse(df$experience_employer<=2,"0_2","more_3"))
df$experience_employer <- as.factor(df$experience_employer_cut)
df <- within(df, experience_employer <- relevel(experience_employer, 
                                                ref = "more_3"))
gb_index_fct(df, df$experience_employer, "default_flag")

# On Address:  NO
plot_infotables(IV, "on_address")
df$on_address_cut <- ifelse(is.na(df$on_address),"other",
    ifelse(df$on_address<=48,"48_less","more_48"))
df$on_address <- as.factor(df$on_address_cut)
df <- within(df, on_address <- relevel(on_address, ref = "other"))
gb_index_fct(df, df$on_address, "default_flag")

# Flag High Last Paid : OK 
gb_index_fct(df,df$flag_high_last_paid,"default_flag")
df$flag_high_last_paid <- as.factor(df$flag_high_last_paid)
df <- within(df, flag_high_last_paid <- relevel(flag_high_last_paid, 
            ref = "0"))
gb_index_fct(df,df$flag_high_last_paid,"default_flag")

# Flag High Last Paid Ratio :  OK
plot_infotables(IV, "flag_high_last_paid_ratio")
df$flag_high_last_paid_ratio_cut <- 
  ifelse(is.na(df$flag_high_last_paid_ratio),"more_0.24",
  ifelse(df$flag_high_last_paid_ratio<=0.24,"0_0.24","more_0.24"))
df$flag_high_last_paid_ratio <- as.factor(df$flag_high_last_paid_ratio_cut)
df <- within(df, flag_high_last_paid_ratio <- relevel(flag_high_last_paid_ratio, 
  ref = "more_0.24"))
gb_index_fct(df,df$flag_high_last_paid_ratio,"default_flag")

# Prev Online : OK 
gb_index_fct(df,df$prev_online,"default_flag")
df$prev_online <- as.factor(df$prev_online)
df <- within(df, prev_online <- relevel(prev_online, ref = "1"))
gb_index_fct(df,df$prev_online,"default_flag")

# Ratio online :  OK 
plot_infotables(IV, "ratio_online")
df$ratio_online_cut <- ifelse(df$ratio_online<=0.13,"0_0.13","0.14_1")
df$ratio_online <- as.factor(df$ratio_online_cut)
df <- within(df, ratio_online <- relevel(ratio_online, ref = "0.14_1"))
gb_index_fct(df,df$ratio_online,"default_flag")

# Prev Credit : OK 
gb_index_fct(df,df$prev_credit,"default_flag")
df$prev_credit <- as.factor(df$prev_credit)
df <- within(df, prev_credit <- relevel(prev_credit, ref = "PayDay"))
gb_index_fct(df,df$prev_credit,"default_flag")

# First Credit :  OK 
gb_index_fct(df,df$first_credit,"default_flag")
df$first_credit <- as.factor(df$first_credit)
df <- within(df, first_credit <- relevel(first_credit, ref = "PayDay"))
gb_index_fct(df,df$first_credit,"default_flag")

# Refinance ratio : OK 
plot_infotables(IV, "refinance_ratio")
df$refinance_ratio_cut <- ifelse(df$refinance_ratio<=0.47,"0_0.47","0.48_1")
df$refinance_ratio <- as.factor(df$refinance_ratio_cut)
df <- within(df, refinance_ratio <- relevel(refinance_ratio , ref = "0_0.47"))
gb_index_fct(df, df$refinance_ratio, "default_flag")

# Ratio nb prev payments : OK 
plot_infotables(IV, "ratio_nb_payments_prev")
df$ratio_nb_payments_prev_cut <- 
  ifelse(is.na(df$ratio_nb_payments_prev),"0_0.99", 
         ifelse(df$ratio_nb_payments_prev<1,"0_0.99","1"))
df$ratio_nb_payments_prev <- as.factor(df$ratio_nb_payments_prev_cut)
df <- within(df, ratio_nb_payments_prev <- relevel(ratio_nb_payments_prev , 
                                                   ref = "0_0.99"))
gb_index_fct(df, df$ratio_nb_payments_prev, "default_flag")

# Credits Cum : NO
plot_infotables(IV, "credits_cum")
df$credits_cum_cut <- ifelse(df$credits_cum<=5,"0_5","more_6")
df$credits_cum <- as.factor(df$credits_cum_cut)
df <- within(df, credits_cum <- relevel(credits_cum , ref = "0_5"))
gb_index_fct(df, df$credits_cum, "default_flag")

# Days Diff Last Credit :  OK 
plot_infotables(IV, "days_diff_last_credit")
df$days_diff_last_credit_cut <- ifelse(is.na(df$days_diff_last_credit), 
      "more_2",
  ifelse(df$days_diff_last_credit %in% c(0,1),"0_1","more_2"))
df$days_diff_last_credit <- as.factor(df$days_diff_last_credit_cut)
df <- within(df, days_diff_last_credit  <- relevel(days_diff_last_credit , 
      ref = "more_2"))
gb_index_fct(df, df$days_diff_last_credit, "default_flag")

# Max Delay : OK
plot_infotables(IV, "max_delay")
df$max_delay_cut <- ifelse(is.na(df$max_delay),"5_55",
  ifelse(df$max_delay<=4,"less_4",
  ifelse(df$max_delay<=55,"5_55","more_56")))
df$max_delay <- as.factor(df$max_delay_cut)
df <- within(df, max_delay   <- relevel(max_delay, ref = "5_55"))
gb_index_fct(df,df$max_delay,"default_flag")

# Amount Diff :  OK 
plot_infotables(IV, "amount_diff")
df$amount_diff_cut <- ifelse(is.na(df$amount_diff),"less_200",
   ifelse(df$amount_diff<250,"less_200","more_250"))
df$amount_diff <- as.factor(df$amount_diff_cut)
df <- within(df, amount_diff <- relevel(amount_diff, ref = "less_200"))
gb_index_fct(df,df$amount_diff,"default_flag")

# Ratio last paid : OK 
plot_infotables(IV, "ratio_last_paid")
df$ratio_last_paid_cut <- ifelse(is.na(df$ratio_last_paid),"0.11_0.63",
   ifelse(df$ratio_last_paid<=0.11,"0_0.11",
   ifelse(df$ratio_last_paid<0.63,"0.11_0.63","more_0.64")))
df$ratio_last_paid <- as.factor(df$ratio_last_paid_cut)
df <- within(df, ratio_last_paid <- relevel(ratio_last_paid, ref = "0.11_0.63"))
gb_index_fct(df,df$ratio_last_paid,"default_flag")

# Status active total : OK 
gb_index_fct(df,df$status_active_total,"default_flag")
df$status_active_total_cut <- ifelse(is.na(df$status_active_total),"other",
  ifelse(df$status_active_total==0,"0","other"))
df$status_active_total <- as.factor(df$status_active_total_cut)
df <- within(df, status_active_total <- relevel(status_active_total, 
  ref = "other"))
gb_index_fct(df, df$status_active_total, "default_flag")

# Status finished total : OK 
gb_index_fct(df,df$status_finished_total,"default_flag")
df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),"not_75",
  ifelse(df$status_finished_total==75,"75","not_75"))
df$status_finished_total <- as.factor(df$status_finished_total_cut)
df <- within(df,status_finished_total <- relevel(status_finished_total, 
 ref = "not_75"))
gb_index_fct(df, df$status_finished_total, "default_flag")

# Source entity count total : OK 
plot_infotables(IV, "source_entity_count_total")
df$source_entity_count_total_cut <- ifelse(
 is.na(df$source_entity_count_total), "9_less",
 ifelse(df$source_entity_count_total<=9,"9_less","10_more"))
df$source_entity_count_total <- as.factor(
  df$source_entity_count_total_cut)
df <- within(df, source_entity_count_total <- relevel(
  source_entity_count_total,ref = "9_less"))
gb_index_fct(df, df$source_entity_count_total, "default_flag")

# Ratio total : OK 
plot_infotables(IV, "outs_overdue_ratio_total")
df$outs_overdue_ratio_total_cut <- ifelse(
  is.na(df$outs_overdue_ratio_total), "0_0.16",
   ifelse(df$outs_overdue_ratio_total==-999,"0_0.16",
   ifelse(df$outs_overdue_ratio_total==0,"0",
   ifelse(df$outs_overdue_ratio_total<=0.16,"0_0.16","more_0.16"))))
df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
df <- within(df, outs_overdue_ratio_total <- relevel(
  outs_overdue_ratio_total, ref = "0_0.16"))
gb_index_fct(df, df$outs_overdue_ratio_total, "default_flag")

# Whatsapp registered : NO 
gb_index_fct(df, df$whatsapp_registered, "default_flag")
df$whatsapp_registered_cut <- ifelse(is.na(df$whatsapp_registered), "other",
   ifelse(df$whatsapp_registered=="False", "other",
   ifelse(df$whatsapp_registered=="True", "True", "other")))
df$whatsapp_registered <- as.factor(df$whatsapp_registered_cut)
df <- within(df, whatsapp_registered <- relevel(whatsapp_registered,
   ref = "other"))
gb_index_fct(df, df$whatsapp_registered, "default_flag")

# Viber registered : OK 
gb_index_fct(df, df$viber_registered, "default_flag")
df$viber_registered_cut <- ifelse(is.na(df$viber_registered), "other",
  ifelse(df$viber_registered=="False", "False",
  ifelse(df$viber_registered=="True", "other", "other")))
df$viber_registered <- as.factor(df$viber_registered_cut)
df <- within(df, viber_registered <- relevel(viber_registered, 
                                             ref = "other"))
gb_index_fct(df, df$viber_registered, "default_flag")

# New Credirect Old City Cash :  OK 
gb_index_fct(df,df$new_cred_old_city,"default_flag")
df$new_cred_old_city <- as.factor(df$new_cred_old_city)
df <- within(df, new_cred_old_city <- relevel(new_cred_old_city, ref = "0"))
gb_index_fct(df,df$new_cred_old_city,"default_flag")




#####################################################
# Choose variable to be enter beta regression model #
#####################################################
list_var_model <-  c("default_flag",
"ownership","education","status_work","purpose","age","experience_employer",
"flag_high_last_paid","flag_high_last_paid_ratio","prev_online","ratio_online",
"prev_credit","first_credit","refinance_ratio","ratio_nb_payments_prev",
"days_diff_last_credit","max_delay","amount_diff","ratio_last_paid",
"status_active_total","status_finished_total","source_entity_count_total",
"outs_overdue_ratio_total","viber_registered")
list_var_model <-  c("default_flag",
"education","status_work","purpose","age","experience_employer",
"new_cred_old_city", 
"days_diff_last_credit","max_delay","amount_diff",
"status_active_total","status_finished_total","source_entity_count_total",
"outs_overdue_ratio_total","viber_registered")
                     
df_temp <- df
df_select <- df[,list_var_model]

corr_df <- dummy.data.frame(df_select, sep = "_")
View(abs(cor(corr_df)))
write.xlsx(abs(cor(corr_df)),
"C:\\Projects\\Behavioral_Scoring\\Credirect_v3\\data\\corr_report_installments.xlsx")
mat_corr <- as.matrix(abs(cor(corr_df)))
mat_corr <- mat_corr[2:nrow(mat_corr),2:ncol(mat_corr)]

corr_report <- numeric(1000)
k <- 0
for(i in 1:nrow(mat_corr)){
  for(j in 1:ncol(mat_corr)){
    if(mat_corr[i,j]>0.45 & i<j){
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

df_Log_beh_Credirect <- glm(default_flag ~ ., family=binomial, 
                                         data=df80)
step(df_Log_beh_Credirect, direction = c("backward"))



####################################
# Build logistic regression model  #
####################################

# Summary of coefficients of Logistic Regression
summary(df_Log_beh_Credirect)

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
df80$score <- predict(df_Log_beh_Credirect, newdata=df80, 
                      type="response")
forecast <- predict(df_Log_beh_Credirect, newdata=df80, 
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
forecast2 <- predict(df_Log_beh_Credirect, newdata=df20, 
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
df$score <- predict(df_Log_beh_Credirect, newdata=df, 
                    type="response")

# R square of the model application
rsq(df80$default_flag,forecast)
rsq(df20$default_flag,forecast2)



##############
# Save RData #
##############
save(df_Log_beh_Credirect, 
    file = "PD_Logistic_Behavioral_Credirect.rdata")



##########################################
# User chooses bins for rest of analysis #
##########################################
bins <- c(0,0.0375,0.05,0.0625,0.075,
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
View(ks(df_train,df_test))



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
score_all <- predict(df_Log_beh_Credirect, newdata=df,
                     type="response")
df_anal <- cbind(df,score_all)
names(df_anal)[length(df_anal)] <- 'Score'
df_anal <- merge(df_anal, df_raw[,c("credit_number",
  "amount")], by.x = "credit_number", 
  by.y = "credit_number", all.x = TRUE)
write.csv(df_anal,"Results_Logit_for_Analysis.csv")




##############################################################
# Confusion matrix and Kolmogorov-Smirnoff on whole dataset  #
##############################################################
# Compute confusion matrix on whole dataset
df_new <- df
score_all <- predict(df_Log_beh_Credirect, newdata=df_new , 
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
score_all <- predict(df_Log_beh_Credirect, newdata=df_new , 
                     type="response")
df_new <- df
df_new <- cbind(df,score_all)
df_new$Score <- cut(df_new$score_all,bins)
ks(df_new,NA)


###########################
######### END #############
###########################


