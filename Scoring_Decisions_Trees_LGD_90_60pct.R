

#####################################################################
# MODELLING LOSS GIVEN DEFAULT WITH DECISION TREES(BINARY OUTCOME)  #
#####################################################################

# set the seed
set.seed(550)

# Load libraries
library(caTools)
library(ROCR)
library(Hmisc)
library(plyr)
library(binr)
library(gmodels)
library(cluster)
library(gmodels)
library(party)
library(nnet)

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
setwd("C:\\Projects\\LGD_model\\data\\")
df <- read.csv("all_data_buckets.csv", sep=",")

# Correct some fields
df$return_rate <- ifelse(df$return_rate>1, 1, ifelse(df$return_rate<0,0,df$return_rate))
df$return_rate <- ifelse(df$return_rate>0.6, 1, 0)
df$return_rate90 <- ifelse(df$return_rate90>1, 1, ifelse(df$return_rate90<0,0,df$return_rate90))
df$return_rate90 <- ifelse(df$return_rate90>0.6, 1, 0)
df <- subset(df,df$EAD90>10)


df$days_diff_last_credit <- ifelse(df$days_diff_last_credit<0,0,df$days_diff_last_credit)
df$ratio_last_amount_paid <- ifelse(is.na(df$ratio_last_amount_paid),0,df$ratio_last_amount_paid)

# Choose variables to enter model
list_var_model <-  c("return_rate","gender", "flag_paid_installments","last_payment_before_def","ratio_last_amount_paid",
                     "days_diff_last_credit","ckr_status","ownership","education","marital_status","age","experience_employer",
                     "ratio_installment_income","max_delay","on_address","household_children","credits_cum","period",
                     "payment_time_diff90", "maturity","total_income","ratio_paid_installments", "ratio_passed_installments",
                     "refinance_previous","default_previous","default_cum","refinance_cum","return_rate90","paid_time90","return_rate90")
df_select <- df[,list_var_model]

# Get % of missing data
pct_missing_data <- round(sapply(df_select,count_empty)/nrow(df_select)*100,2)

# Treat missing data
#df_select[is.na(df_select)] <- -999

# Split into 80% training and 20% testing
split <- sample.split(df_select$return_rate90, SplitRatio = 0.8)
df80 <- subset(df_select, split == TRUE)
df20 <- subset(df_select, split == FALSE)



# Build decision tree model from "party" library
decision_tree_model <- ctree(return_rate90 ~ education + ratio_passed_installments + ratio_last_amount_paid + 
                               max_delay + gender + age + ratio_installment_income + last_payment_before_def +
                               days_diff_last_credit + experience_employer + period + paid_time90 + 
                               credits_cum, 
                             data = df80, 
                             controls = ctree_control(mincriterion = 0.95, minsplit = 100))
plot(decision_tree_model)

# Score training and testings samples
score_train <- predict(decision_tree_model, df80)
score_test <- predict(decision_tree_model, df20)

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
ROCRpred <- ROCR::prediction (score_train, df80$return_rate90)
ROCRperf <- performance (ROCRpred, "tpr", "fpr")
plot(ROCRperf, col='blue', lwd=5)
par(new=TRUE)
plot(y, col='green',axes=F,xlab='',ylab='',lwd=1)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
giniLog <- 2*(auc - 0.5)
gini_train <- giniLog*100
gini_train

# Apply on testing dataset
ROCRpred <- ROCR::prediction (score_test, df20$return_rate90)
ROCRperf <- performance (ROCRpred, "tpr", "fpr")
plot(ROCRperf,col='blue', lwd=5)
par(new=TRUE)
plot(y, col='green',axes=F,xlab='',ylab='',lwd=1)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
giniLog <- 2*(auc - 0.5)
gini_test <- giniLog*100
gini_test


# Kolmogorov-Smirnoff test
bins <- c(0,0.1,0.2,0.3,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,1)

df_train <- cbind(df80,score_train)
df_test <- cbind(df20,score_test)
names(df_train)[ncol(df_train)] <- "score_train"
names(df_test)[ncol(df_test)] <- "score_test"
df_train$Score <- cut(df_train$score_train,bins)
df_test$Score <- cut(df_test$score_test,bins)

df_ks <- data.frame(t(table(df_train$return_rate90,df_train$Score))[,1])
names(df_ks)[1] <- "True_Goods_Train"
df_ks$True_Bads_Train <-t(table(df_train$return_rate90,df_train$Score))[,2]
df_ks$True_Goods_Test <- t(table(df_test$return_rate90,df_test$Score))[,1]
df_ks$True_Bads_Test <- t(table(df_test$return_rate90,df_test$Score))[,2]

df_ks$Cumulative_Goods_Train[1] <- df_ks$True_Goods_Train[1]
df_ks$Cumulative_Bads_Train[1] <- df_ks$True_Bads_Train[1]
df_ks$Cumulative_Goods_Test[1] <- df_ks$True_Goods_Test[1]
df_ks$Cumulative_Bads_Test[1] <- df_ks$True_Bads_Test[1]


for (i in (2:nrow(df_ks))) {
  df_ks$Cumulative_Goods_Train[i]=df_ks$Cumulative_Goods_Train[i-1]+df_ks$True_Goods_Train[i]
  df_ks$Cumulative_Bads_Train[i]=df_ks$Cumulative_Bads_Train[i-1]+df_ks$True_Bads_Train[i]
  df_ks$Cumulative_Goods_Test[i]=df_ks$Cumulative_Goods_Test[i-1]+df_ks$True_Goods_Test[i]
  df_ks$Cumulative_Bads_Test[i]=df_ks$Cumulative_Bads_Test[i-1]+df_ks$True_Bads_Test[i]
}
for (i in (1:nrow(df_ks))) {
  df_ks$Cumulative_Goods_Train_Pct[i]=df_ks$Cumulative_Goods_Train[i]/df_ks$Cumulative_Goods_Train[nrow(df_ks)]
  df_ks$Cumulative_Bads_Train_Pct[i]=df_ks$Cumulative_Bads_Train[i]/df_ks$Cumulative_Bads_Train[nrow(df_ks)]
  df_ks$KS_Test[i] <- df_ks$Cumulative_Goods_Train_Pct[i]-df_ks$Cumulative_Goods_Train_Pct[i]
  df_ks$Cumulative_Goods_Test_Pct[i]=df_ks$Cumulative_Goods_Test[i]/df_ks$Cumulative_Goods_Test[nrow(df_ks)]
  df_ks$Cumulative_Bads_Test_Pct[i]=df_ks$Cumulative_Bads_Test[i]/df_ks$Cumulative_Bads_Test[nrow(df_ks)]
  df_ks$KS_Test[i] <- df_ks$Cumulative_Goods_Test_Pct[i]-df_ks$Cumulative_Goods_Test_Pct[i]
}
df_ks$KS_Test_Train <- df_ks$Cumulative_Goods_Train_Pct-df_ks$Cumulative_Bads_Train_Pct
df_ks$KS_Test_Test <- df_ks$Cumulative_Goods_Test_Pct-df_ks$Cumulative_Bads_Test_Pct

df_ks


# Get the confusion matrix and choose the cut-off point for bads
cut_off <- 0.4
predict <- ifelse(score_train>cut_off,1,0)
accuracy_table <- table(df80$return_rate90,predict)
accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)*100
sensitivity <-  accuracy_table[2,2]/(accuracy_table[2,2]+accuracy_table[2,1])
specificity <-  accuracy_table[1,1]/(accuracy_table[1,1]+accuracy_table[1,2])
tot_acc <- sensitivity + specificity
accuracy_table
tot_acc


# Compute accuracy of confusion matrix
accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)
accuracy


# Output decision trees 
decision_tree_model_90 <- decision_tree_model
setwd("C:\\Projects\\LGD_model\\models\\60pct\\")
save(decision_tree_model_90, file = "Decision_Trees_90_60pct.rdata")


# Maximums and minimums of probabilities
max(score_train)
min(score_test)



#############
#   END     #
#############

     