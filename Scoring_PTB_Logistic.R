

##################################################
# PROBABILITY OF DEFAULT - LOGTISTIC REGRESSION  #
##################################################


#################
# Libraries     #
#################
library(smbinning)
library(caTools)
library(ROCR)
library(Hmisc)
library(dummies)
library(plyr)
library(binr)
library(My.stepwise)
library(openxlsx)


##############
# Load Data  #
##############

## This is to make sure we generate always the same random numbers ##
set.seed(100)

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
setwd("C:\\Projects\\PTB\\data\\")
df <- read.xlsx("input.xlsx")

# Function to calculate GB index
gb_index_fct <- function(data,var,gb_flag){
  gb_ratio_all <- count(data, gb_flag)[1,2]/count(data, gb_flag)[2,2]
  index <- vector(length = length(unique(var)))
  for (i in 1:length(unique(var))) {
    local <- count(subset(data,var==unique(var)[i]), gb_flag)[1,2]/count(subset(data,var==unique(var)[i]), gb_flag)[2,2]
    all <- gb_ratio_all
    index[i] <- ifelse(local>all,paste(round(local/all*100,1),"B",sep=""),paste(round(all/local*100,1),"G",sep=""))
  }
  index <- as.data.frame(t(index))
  for (i in 1:length(unique(var))) {
    names(index)[i] <- paste(unique(var)[i])
  }
  return (index[ , order(names(index))])
}

# Function to calculate R square
rsq <- function(x, y) summary(lm(y~x))$r.squared

# Correct some fields
#df$days_diff_last_credit <- ifelse(df$days_diff_last_credit<0,0,df$days_diff_last_credit)
#df$flag_high_last_paid <- ifelse(df$days_diff_last_credit %in% c(0,1) & df$ratio_last_amount>=0.5, 1, 0)
#df$amount_diff <- df$amount - df$amount_prev
#df$default_cum <- ifelse(df$credits_cum==0,NA,df$default_cum)

# Select relevant data
df <- subset(df,df$total_income<10000)



###########################
# Correlation report      #
###########################

# list_var <- c("default_flag",
#               "ownership","education","marital_status","household_children","household_total","on_address","ckr_status","gender",
#               "age","maturity","total_income","experience_employer","purpose","ratio_installment_income","credits_cum",
#               "default_previous","default_cum","refinance_cum","days_diff_last_credit","refinance_previous","max_delay",
#               "status_work","period","flag_high_last_paid","ratio_last_amount","amount_diff")
list_var <- c("default_flag",
              "ownership","education","marital_status","household_children","on_address","gender",
              "age","maturity","total_income","experience_employer","purpose","ratio_installment_income","credits_cum",
              "recency", "total_amount_paid", "credits_cum","status_work","period")

# Take relevant variables to new dataframe for correlation report 
df_corr <- df[ , which(names(df) %in% list_var)]

# Take only complete rows
df_corr <- complete_fct(df_corr, names(df_corr))

# Run correlation matrix
#View(cor(df_corr))


#######################################
# Factoring categorial variables      #
#######################################

df$ownership <- ifelse(df$ownership %in% c(1,3),"1_3","2_4")
df$ownership <- ifelse(is.na(df$ownership),"2",df$ownership)
df$ownership <- as.factor(df$ownership)
df <- within(df, ownership<- relevel(ownership, ref = "1_3"))

df$household_children <- ifelse(df$household_children==0 | df$household_children==1,"0_1",
                         ifelse(df$household_children>=3,"3_more","2"))
df$household_children <- ifelse(is.na(df$household_children),"0_1",df$household_children)
df$household_children <- as.factor(df$household_children)
df <- within(df, household_children <- relevel(household_children, ref = "0_1"))

bins(df$age, target.bins = 10, minpts = 10)$binct
df$age_cut <- cut (df$age, c(17,30,50,55,60,100))
df$age <- ifelse(df$age<=30,"30_less",
          ifelse(df$age<=50,"31_50",
          ifelse(df$age<=55,"51_55",
          ifelse(df$age<=60,"56_60","61_more"))))
df$age <- as.factor(df$age)
df <- within(df, age <- relevel(age, ref = "31_50"))

df$recency_cut <- cut (df$recency, c(-1,0,35,50,65,82,100,128,170,250,20000))


#####################################################
# Choose variable to be enter beta regression model #
#####################################################
list_var_model <-  c("default_flag",
                     "ownership","education","household_children",
                     "on_address","gender","age","experience_employer","purpose",
                     "max_delay",
                     "maturity",
                     "ratio_installment_income","credits_cum","status_work",
                     "days_diff_last_credit",
                     "flag_high_last_paid","amount_diff")

df_temp <- df
df_select <- df[,list_var_model]

#corr_df <- dummy.data.frame(df_select, sep = "_")
#write.xlsx(corr_df, "correlation_report_dummies.xlsx")


#########################################################
# Separate into 80% Training and 20% Testing            #
#########################################################

split <- sample.split(df_select$default_flag, SplitRatio = 0.8)
df80 <- subset(df_select, split == TRUE)
df20 <- subset (df_select, split == FALSE)



#####################################
# Carry-out stepwise regression     #
#####################################

df_Log_beh <- glm(default_flag ~ ., family=binomial, data=df80)
#step(df_Log_beh,direction = c("both"))



#####################################
# Model with lositic regression     #
#####################################

# Summary of coefficients of Logistic Regression
summary(df_Log_beh)

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
df80$score <- predict(df_Log_beh, newdata=df80, type="response")
forecast <- predict(df_Log_beh, newdata=df80, type="response")
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
forecast2 <- predict(df_Log_beh, newdata=df20, type="response")
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
df$score <- predict(df_Log_beh, newdata=df, type="response")

# R square of the model application
rsq(df80$default_flag,forecast)
rsq(df20$default_flag,forecast2)





