

#######################################################
# PROBABILTY TO CHURN MODEL WITH LOGISTIC REGRESSION  #
#######################################################



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



##############
# Load Data  #
##############

## This is to make sure we generate always the same random numbers ##
set.seed(100)

# Load data #
setwd("C:\\Projects\\PTC\\data")
load("input_data.rdata")



#########################
# Select relevant data  #
#########################

all_cols <- c("egn","credit_number","status","date_entry","churn",
   "age","maturity","gender","ratio_installment_income",
   "ownership","education","household_children","on_address",
   "experience_employer","marital_status",
   "purpose", "cum_rej","avg_maturity","credits_cum","max_delay","activity_cum",
   "cred_per_app","total_amount_paid","recency","avg_recency",
   "status_active_total","status_finished_total","monthly_installment_total",
   "source_entity_count_total","amount_drawn_total","cred_count_total")
df <- output[,c(all_cols)]
df <- subset(df, df$date_entry>="2018-04-10")



#################
# Analyze data  #
#################

# Get information value
df_ana <- df
cols_fac <- c("education","gender","marital_status","ownership","purpose",
              "status","status_active_total","status_finished_total")
df_ana[cols_fac] <- lapply(df_ana[cols_fac], factor)
df_ana$education <- as.factor(df_ana$education)
create_infotables(data=df_ana, y="churn", bins=5,parallel=FALSE)$Tables

# Get correlation report
cols_cor <- c("status","ratio_installment_income",
  "cum_rej","avg_maturity",
  "credits_cum","max_delay","activity_cum",
  "cred_per_app","total_amount_paid","recency","avg_recency",
  "status_active_total","status_finished_total","monthly_installment_total",
  "source_entity_count_total","amount_drawn_total","cred_count_total")
df_ana <- df[,cols_cor]
corr_df <- dummy.data.frame(df_ana, sep = "_")
corr_df <- complete_fct(corr_df, cols_cor)
View(cor(corr_df))



#########################
# Cut and bin variables #
#########################

# Create an Information Value table
IV <- create_infotables(data=df, y="churn", bins=10, parallel=FALSE) 

plot_infotables(IV, "status")
df$status_cut <- ifelse(df$status==2,"2","not_2")
df$status <- as.factor(df$status_cut)
df <- within(df, status <- relevel(status, ref = "not_2"))
gb_index_fct(df, df$status_cut, "churn")

plot_infotables(IV, "cum_rej")
df$cum_rej_cut <- ifelse(df$cum_rej==0,"0","more_1")
df$cum_rej <- as.factor(df$cum_rej_cut)
df <- within(df, cum_rej <- relevel(cum_rej, ref = "0"))
gb_index_fct(df, df$cum_rej_cut, "churn")

plot_infotables(IV, "avg_maturity")
df$avg_maturity_cut <- ifelse(df$avg_maturity<=1.5,"less_1p5",
                         ifelse(df$avg_maturity<=8.6, "1p5_8p6", "8p6_more"))
df$avg_maturity <- as.factor(df$avg_maturity_cut)
df <- within(df, avg_maturity <- relevel(avg_maturity, ref = "1p5_8p6"))
gb_index_fct(df, df$avg_maturity_cut, "churn")

plot_infotables(IV, "credits_cum")
df$credits_cum_cut <- ifelse(df$credits_cum==1,"1",
                      ifelse(df$credits_cum==2,"2", 
                      ifelse(df$credits_cum==3,"3",
                      ifelse(df$credits_cum<=5,"4_5","6_more"))))
df$credits_cum <- as.factor(df$credits_cum_cut)
df <- within(df, credits_cum <- relevel(credits_cum, ref = "2"))
gb_index_fct(df, df$credits_cum_cut, "churn")

plot_infotables(IV, "max_delay")
df$max_delay_cut <- ifelse(df$max_delay>=1 & df$max_delay<=48,"0_48","49_more")
df$max_delay <- as.factor(df$max_delay_cut)
df <- within(df, max_delay <- relevel(max_delay, ref = "49_more"))
gb_index_fct(df, df$max_delay_cut, "churn")

plot_infotables(IV, "activity_cum")
df$activity_cum_cut <- ifelse(df$activity_cum==1,"1",
                       ifelse(df$activity_cum==2,"2",
                       ifelse(df$activity_cum==3,"3",
                       ifelse(df$activity_cum<=5,"4_5",
                       ifelse(df$activity_cum<=7,"6_7","8_more")))))
df$activity_cum <- as.factor(df$activity_cum_cut)
df <- within(df, activity_cum <- relevel(activity_cum, ref = "3"))
gb_index_fct(df, df$activity_cum_cut, "churn")

plot_infotables(IV, "cred_per_app")
df$cred_per_app_cut <- ifelse(df$cred_per_app<=0.32,"less_0p32",
                       ifelse(df$cred_per_app<1,"0p32_0p99", "1"))
df$cred_per_app <- as.factor(df$cred_per_app_cut)
df <- within(df, cred_per_app <- relevel(cred_per_app, ref = "0p32_0p99"))
gb_index_fct(df, df$cred_per_app_cut, "churn")

plot_infotables(IV, "avg_recency")
df$avg_recency_cut <- ifelse(is.na(df$avg_recency),"none",
                      ifelse(df$avg_recency<=21,"less_21",
                      ifelse(df$avg_recency<=52,"22_52",
                      ifelse(df$avg_recency<=84,"53_84","more_85"))))
df$avg_recency <- as.factor(df$avg_recency_cut)
df <- within(df, avg_recency <- relevel(avg_recency, ref = "none"))
gb_index_fct(df, df$avg_recency_cut, "churn")

plot_infotables(IV, "status_active_total")
df$status_active_total_cut <- ifelse(is.na(df$status_active_total),"none_71_72",
   ifelse(df$status_active_total==-1,"no_credit",
   ifelse(df$status_active_total==0,"no_delay",
   ifelse(df$status_active_total %in% c(71,72),"none_71_72",
   ifelse(df$status_active_total %in% c(73,74),"73_74", "75")))))      
df$status_active_total <- as.factor(df$status_active_total_cut)
df <- within(df, status_active_total <- relevel(status_active_total, 
       ref = "none_71_72"))
gb_index_fct(df, df$status_active_total_cut, "churn")

plot_infotables(IV, "status_finished_total")
df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
       "none_71_72_73_no_delay",
 ifelse(df$status_finished_total==0,"none_71_72_73_no_delay",
 ifelse(df$status_finished_total %in% c(71,72),"none_71_72_73_no_delay",
 ifelse(df$status_finished_total==74,"74", "75"))))
df$status_finished_total <- as.factor(df$status_finished_total_cut)
df <- within(df, status_finished_total <- relevel(status_finished_total, 
       ref = "none_71_72_73_no_delay"))
gb_index_fct(df, df$status_finished_total_cut, "churn")

plot_infotables(IV, "source_entity_count_total")
df$source_entity_count_total_cut <- ifelse(is.na(df$source_entity_count_total),
                                           "none_3_4",
 ifelse(df$source_entity_count_total==0,"0",
 ifelse(df$source_entity_count_total==1,"1",
 ifelse(df$source_entity_count_total==2,"2",
 ifelse(df$source_entity_count_total %in% c(3,4),"none_3_4",
 ifelse(df$source_entity_count_total<6,"5_6",
 ifelse(df$source_entity_count_total<9,"7_9", "more_10")))))))
df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
df <- within(df, source_entity_count_total <- relevel(source_entity_count_total, 
                                                      ref = "none_3_4"))
gb_index_fct(df, df$source_entity_count_total_cut, "churn")

plot_infotables(IV, "monthly_installment_total")
df$monthly_installment_total_cut <- ifelse(is.na(df$monthly_installment_total),
                                           "none_300_600",
  ifelse(df$monthly_installment_total<100,"less_100",
  ifelse(df$monthly_installment_total<300,"100_300",
  ifelse(df$monthly_installment_total<600,"none_300_600",
  ifelse(df$monthly_installment_total<900,"600_900",
  ifelse(df$monthly_installment_total<1400,"900_1400","more_1400"))))))
df$monthly_installment_total <- as.factor(df$monthly_installment_total_cut)
df <- within(df, monthly_installment_total <- relevel(monthly_installment_total,
                                                      ref = "none_300_600"))
gb_index_fct(df, df$monthly_installment_total_cut, "churn")

plot_infotables(IV, "cred_count_total")
df$cred_count_total_cut <- ifelse(is.na(df$cred_count_total),"1_5_none",
  ifelse(df$cred_count_total==0,"0",
  ifelse(df$cred_count_total %in% c(1,2,3,4,5),"1_5_none",
  ifelse(df$cred_count_total<=7,"6_7",
  ifelse(df$cred_count_total<=11,"8_11", "more_12")))))
df$cred_count_total <- as.factor(df$cred_count_total_cut)
df <- within(df, cred_count_total <- relevel(cred_count_total, 
                                             ref = "1_5_none"))
gb_index_fct(df, df$cred_count_total_cut, "churn")
                               

############################################
# Check colinearity of variables selected  #
############################################


list_var_model <-  c("churn","cum_rej","status","activity_cum","cred_per_app",
  "max_delay", "avg_recency","avg_maturity","status_finished_total",
  "source_entity_count_total_cut")
df_select <- df[,list_var_model]
corr_df <- dummy.data.frame(df_select, sep = "_")
View(cor(corr_df))



####################################
# Select variables to enter model  #
####################################

cols_select <- c("churn","activity_cum","max_delay",
                 "avg_maturity","status_finished_total","cred_count_total")
df_select <- df[,c(cols_select)]



#########################################################
# Separate into 80% Training and 20% Testing            #
#########################################################

split <- sample.split(df_select$churn, SplitRatio = 0.8)
df80 <- subset(df_select, split == TRUE)
df20 <- subset (df_select, split == FALSE)



#####################################
# Carry-out stepwise regression     #
#####################################

df_Churn <- glm(churn ~ ., family=binomial, data=df80)
step(df_Churn,direction = c("backward"))


#####################################
# Model with lositic regression     #
#####################################

# Summary of coefficients of Logistic Regression
summary(df_Churn)

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
df80$score <- predict(df_Churn, newdata=df80, type="response")
forecast <- predict(df_Churn, newdata=df80, type="response")
ROCRpred <- ROCR::prediction (forecast, df80$churn)
ROCRperf <- performance (ROCRpred, "tpr", "fpr")
plot(ROCRperf, col='blue', lwd=5)
par(new=TRUE)
plot(y, col='green',axes=F,xlab='',ylab='',lwd=1)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
giniLog <- 2*(auc - 0.5)
gini_train <- giniLog*100
gini_train

# Apply on testing dataset
forecast2 <- predict(df_Churn, newdata=df20, type="response")
ROCRpred <- ROCR::prediction (forecast2, df20$churn)
ROCRperf <- performance (ROCRpred, "tpr", "fpr")
plot(ROCRperf,col='blue', lwd=5)
par(new=TRUE)
plot(y, col='green',axes=F,xlab='',ylab='',lwd=1)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
giniLog <- 2*(auc - 0.5)
gini_test <- giniLog*100
gini_test

# Apply on whole dataset
df$score <- predict(df_Churn, newdata=df, type="response")



##########################################
# User chooses bins for rest of analysis #
##########################################
bins <- c(0,0.0375,0.05,0.0625,0.075,0.0875,0.1,0.125,0.15,0.175,0.2,0.225,0.25,
          0.275,0.3,0.325,0.35,0.375,0.4,0.5,0.8)



##############################
# Kolmogorov-Smirnoff Test   #
##############################

score_train <- forecast
score_test <- forecast2
df_train <- cbind(df80,score_train)
df_test <- cbind(df20,score_test)
df_train$Score <- cut(df_train$score_train,bins)
df_test$Score <- cut(df_test$score_test,bins)


df_ks <- data.frame(t(table(df_train$churn,df_train$Score))[,1])
names(df_ks)[1] <- "True_Goods_Train"
df_ks$True_Bads_Train <-t(table(df_train$churn,df_train$Score))[,2]
df_ks$True_Goods_Test <- t(table(df_test$churn,df_test$Score))[,1]
df_ks$True_Bads_Test <- t(table(df_test$churn,df_test$Score))[,2]

df_ks$Cumulative_Goods_Train[1] <- df_ks$True_Goods_Train[1]
df_ks$Cumulative_Bads_Train[1] <- df_ks$True_Bads_Train[1]
df_ks$Cumulative_Goods_Test[1] <- df_ks$True_Goods_Test[1]
df_ks$Cumulative_Bads_Test[1] <- df_ks$True_Bads_Test[1]


for (i in (2:nrow(df_ks))) {
  df_ks$Cumulative_Goods_Train[i]=df_ks$Cumulative_Goods_Train[i-1]+
    df_ks$True_Goods_Train[i]
  df_ks$Cumulative_Bads_Train[i]=df_ks$Cumulative_Bads_Train[i-1]+
    df_ks$True_Bads_Train[i]
  df_ks$Cumulative_Goods_Test[i]=df_ks$Cumulative_Goods_Test[i-1]+
    df_ks$True_Goods_Test[i]
  df_ks$Cumulative_Bads_Test[i]=df_ks$Cumulative_Bads_Test[i-1]+
    df_ks$True_Bads_Test[i]
}
for (i in (1:nrow(df_ks))) {
  df_ks$Cumulative_Goods_Train_Pct[i]=df_ks$Cumulative_Goods_Train[i]/
    df_ks$Cumulative_Goods_Train[nrow(df_ks)]
  df_ks$Cumulative_Bads_Train_Pct[i]=df_ks$Cumulative_Bads_Train[i]/
    df_ks$Cumulative_Bads_Train[nrow(df_ks)]
  df_ks$KS_Test[i] <- df_ks$Cumulative_Goods_Train_Pct[i]-
    df_ks$Cumulative_Goods_Train_Pct[i]
  df_ks$Cumulative_Goods_Test_Pct[i]=df_ks$Cumulative_Goods_Test[i]/
    df_ks$Cumulative_Goods_Test[nrow(df_ks)]
  df_ks$Cumulative_Bads_Test_Pct[i]=df_ks$Cumulative_Bads_Test[i]/
    df_ks$Cumulative_Bads_Test[nrow(df_ks)]
  df_ks$KS_Test[i] <- df_ks$Cumulative_Goods_Test_Pct[i]-
    df_ks$Cumulative_Goods_Test_Pct[i]
}
df_ks$KS_Test_Train <- df_ks$Cumulative_Goods_Train_Pct-
  df_ks$Cumulative_Bads_Train_Pct
df_ks$KS_Test_Test <- df_ks$Cumulative_Goods_Test_Pct-
  df_ks$Cumulative_Bads_Test_Pct
df_ks



######################
# Confusion Matrix   #
######################
# Get the confusion matrix and choose the cut-off point for bads
cut_off <- 0.25
predict <- ifelse(score_train>cut_off,1,0)
table(predict)[2]/sum(table(predict))*100
accuracy_table <- table(df80$churn,predict)
accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)*100
sensitivity <-  accuracy_table[2,2]/(accuracy_table[2,2]+accuracy_table[2,1])
specificity <-  accuracy_table[1,1]/(accuracy_table[1,1]+accuracy_table[1,2])
tot_acc <- sensitivity + specificity
accuracy_table

accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)
accuracy



################
# Save RData   #
################
save(df_Churn, file = "churn_model.rdata")



###########
#   End   #
###########



