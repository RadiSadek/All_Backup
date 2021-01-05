

###############################################################
# CLUSTERING WITH DECISION TREES ON CITY 2WEEK, WEEK, MONTH   #
###############################################################


# Load libraries
library(caTools)
library(ROCR)
library(Hmisc)
library(plyr)
library(binr)
library(gmodels)
library(cluster)
library(HSAUR)
library(ggplot2)
library(gmodels)
library(party)


# Function for taking only complete rows 
complete_fct <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
# Function to count number of missing values
count_empty <- function(var){
  return(sum(is.na(var)))
}


# set the seed
set.seed(111)


# Load data 
setwd("C:\\Projects\\Cluster_Analysis\\data\\")
df <- read.csv("Data_All.csv", sep=",")


# Select the data
df <- subset(df,df$product_name %in% c("City 2-Week","City Month","City Week"))
df <- df[with(df, order(date)), ]
df <- df[1:120000,]


# Rearrange data
df <- df[ , -which(names(df)==c("default_flag"))]
names(df)[names(df) == 'default_abs'] <- 'default_flag'
df <- subset(df,df$online_cum==0 & df$offline_cum==0)


# Take only complete rows
df <- complete_fct(df, c("total_income", "sub_status", "ckr_status", "on_address"))


# Table percentage of missing data
pct_missing_data <- sapply(df,count_empty)
pct_missing_data


# Rearrange some fields
for (i in (1:nrow(df))){
  if (is.na(df$income[i])) {df$income[i]=0}
  if (is.na(df$additional_income[i])) {df$additional_income[i]=0}
}


# Split into 80% training and 20% testing
split <- sample.split(df$default_flag, SplitRatio = 0.8)
df_80 <- subset(df, split == TRUE)
df_20 <- subset(df, split == FALSE)


# Apply decision tree model from "party" library
decision_tree_model <- ctree(default_flag ~ amount + ownership + education + marital_status +
                             household_total + on_address + age + gender + period +
                             ratio_installment_income + total_income, data = df, 
                             controls = ctree_control(mincriterion = 0.9999999, minsplit = 1000))
plot(decision_tree_model)


# Seperate into cluster numbers
df$cluster <- where(decision_tree_model)


# Get cluster means
df_clust <- df[,c("default_flag","amount","education","marital_status","on_address",
                  "age","gender","period","total_income", "cluster")]
cluster_means <- aggregate(df_clust, by=list(Category=df_clust$cluster), FUN=mean)
cluster_means <- cbind(cluster_means,table(df_clust$cluster))
names(cluster_means)[ncol(cluster_means)] <- "size"
cluster_means <- cluster_means[ , -which(names(cluster_means) %in% c("cluster","Var1"))]
View(cluster_means)


# Plotting some variables
df_sub <- subset(df, df$cluster == 6 | df$cluster == 7 | df$cluster == 25 | df$cluster == 11)
df_sub$cluster_name <- paste("Cluster",df_sub$cluster , sep="")
# qplot(df_sub$amount, df_sub$total_income, data = df_sub, xlab = "Amount", ylab = "Income",
#       colour = df_sub$cluster_name, log = "xy", size = I(4))
# qplot(df_sub$amount, df_sub$education, data = df_sub, xlab = "Amount", ylab = "Education",
#       colour = df_sub$cluster_name, log = "x", size = I(4))
# qplot(df_sub$age, df_sub$total_income, data = df_sub, xlab = "Age", ylab = "Income",
#       colour = df_sub$cluster_name, log = "y", size = I(4))
qplot(df_sub$amount, df_sub$age, data = df_sub, xlab = "Amount", ylab = "Age",
      colour = df_sub$cluster_name, log = "x", size = I(4))


# Split into 80% and 20% and validate on 20% to get gini of decision tree model
decision_tree_model_test <- ctree(default_flag ~ amount + ownership + education + marital_status +
                                  household_total + on_address + age + gender + period +  
                                  ratio_installment_income + total_income, data = df_80, 
                                  controls = ctree_control(mincriterion = 0.9999999, minsplit = 1000))
score_test <- predict(decision_tree_model_test, df_20)
ROCRpred <- ROCR::prediction (score_test, df_20$default_flag)
ROCRperf <- performance (ROCRpred, "tpr", "fpr")
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
giniLog <- 2*(auc - 0.5)
gini_train <- giniLog*100
gini_train


# Coarse class clusters
df$cluster <- ifelse(df$cluster==7 | df$cluster==8,"7_8",
              ifelse(df$cluster==11 | df$cluster==12,"11_12",
                     df$cluster))


# Select where ckr status becomes valid
df <- df[3313:25859,]
write.csv(df,"Data_All_Clusters.csv")


# Subset dataframes
df_11_12 <- subset(df,df$cluster=="11_12")
df_17    <- subset(df,df$cluster=="17")
df_18    <- subset(df,df$cluster=="18")
df_20    <- subset(df,df$cluster=="20")
df_22    <- subset(df,df$cluster=="22")
df_23    <- subset(df,df$cluster=="23")
df_24    <- subset(df,df$cluster=="24")
df_25    <- subset(df,df$cluster=="25")
df_6     <- subset(df,df$cluster=="6")
df_7_8   <- subset(df,df$cluster=="7_8")
df_9     <- subset(df,df$cluster=="9")


# Output each cluster data
write.csv(df_11_12,"Cluster_11_12.csv")
write.csv(df_17,"Cluster_17.csv")
write.csv(df_18,"Cluster_18.csv")
write.csv(df_20,"Cluster_20.csv")
write.csv(df_22,"Cluster_22.csv")
write.csv(df_23,"Cluster_23.csv")
write.csv(df_24,"Cluster_24.csv")
write.csv(df_25,"Cluster_25.csv")
write.csv(df_6,"Cluster_6.csv")
write.csv(df_7_8,"Cluster_7_8.csv")
write.csv(df_9,"Cluster_9.csv")


###############################################################
#                       END                                   #
###############################################################

