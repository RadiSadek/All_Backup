

#####################################################################
# MODELLING LOSS GIVEN DEFAULT WITH DECISION TREES(BINARY OUTCOME)  #
#####################################################################

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
library(RMySQL)

# Function to get split variables in decision trees
splitted_names <- function(ctemp ) {
  intnodes <- unique(where(ctemp))
  intnodes <- sort(intnodes)
  diffnodes <- seq(1:intnodes[length(intnodes)])
  primenodes <- setdiff(diffnodes,intnodes)
  split.names <- vector()
  
  for (i in primenodes){
    temp <- unlist(nodes(ctemp,i)[[1]][[5]])
    split.names <- append(split.names, as.character(temp[length(temp)]))
  }
  
  return(paste(unique(split.names), collapse = ', '))  
}

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, host=db_host, port = df_port)
data_sql <- suppressWarnings(dbSendQuery(con, "select * from test.data_final"))
df <- fetch(data_sql, n=-1)

# set the seed
set.seed(1105)

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
setwd("C:\\Projects\\PD_model\\data\\")
df <- read.csv("buckets\\Bucket3.csv")
nrow(df)
df <- subset(df, df$installment_num!=1)

# Make factor some variabeles
df$purpose <- as.factor(df$purpose)
df$education <- as.factor(df$education)
df$ownership <- as.factor(df$ownership)
df$marital_status <- as.factor(df$ownership)

# Get % of missing data
pct_missing_data <- round(sapply(df,count_empty)/nrow(df)*100,2)

# Treat missing data
#df[is.na(df)] <- -999
df$max_delay <- ifelse(is.na(df$max_delay), -999, df$max_delay)


# Split into 80% training and 20% testing
split <- sample.split(df$default_next, SplitRatio = 0.8)
df80 <- subset(df, split == TRUE)
df20 <- subset(df, split == FALSE)

# Build decision tree model from "party" library
pd_dt_bucket3 <- ctree(default_next ~ max_DPD_upto + ratio_passed_installments + last_payment + payments_per_installments,
                             data = df80, 
                             controls = ctree_control(mincriterion = 0.99, minsplit = 250))
plot(pd_dt_bucket3)

# Get stats
df80$cluster <- where(pd_dt_bucket3)
#View(subset(df80,df80$cluster==29))
aggregate(df80$default_next, by=list(df80$cluster), FUN=mean)

# Score training and testings samples
score_train <- predict(pd_dt_bucket3, df80)
score_test <- predict(pd_dt_bucket3, df20)

# Draw a line for Gini plot
x <- seq(0,1,0.001)
y <- x

# Apply on training dataset
ROCRpred <- ROCR::prediction (score_train, df80$default_next)
ROCRperf <- performance (ROCRpred, "tpr", "fpr")
plot(ROCRperf, col='blue', lwd=5)
par(new=TRUE)
plot(y, col='green',axes=F,xlab='',ylab='',lwd=1)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
giniLog <- 2*(auc - 0.5)
gini_train <- giniLog*100
gini_train

# Apply on testing dataset
ROCRpred <- ROCR::prediction (score_test, df20$default_next)
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
table(df_test$default_next,df_test$score_test)

df_ks <- data.frame(t(table(df_train$default_next,df_train$Score))[,1])
names(df_ks)[1] <- "True_Goods_Train"
df_ks$True_Bads_Train <-t(table(df_train$default_next,df_train$Score))[,2]
df_ks$True_Goods_Test <- t(table(df_test$default_next,df_test$Score))[,1]
df_ks$True_Bads_Test <- t(table(df_test$default_next,df_test$Score))[,2]

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
cut_off <- 0.25
predict <- ifelse(score_train>cut_off,1,0)
accuracy_table <- table(df80$default_next,predict)
accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)*100
sensitivity <-  accuracy_table[2,2]/(accuracy_table[2,2]+accuracy_table[2,1])
specificity <-  accuracy_table[1,1]/(accuracy_table[1,1]+accuracy_table[1,2])
tot_acc <- sensitivity + specificity
accuracy_table
tot_acc


# Compute accuracy of confusion matrix
accuracy <- sum(diag(accuracy_table))/sum(accuracy_table)
accuracy


# Maximums and minimums of probabilities
score_all <- predict(pd_dt_bucket3, df)
df <- cbind(df,score_all)
max(score_all)
min(score_all)
mean(score_all)
nrow(df)
plot(pd_dt_bucket3)


# Get variables of decision tree
splitted_names(pd_dt_bucket3)

# Output model 
setwd("C:\\Projects\\PD_model\\models\\")
save(pd_dt_bucket3, file = "PD_Bucket3.RData")



#############
#   END     #
#############

     