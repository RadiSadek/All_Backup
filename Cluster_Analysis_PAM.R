

#############################################################################
# PARTITIONING AROUND MEDOIDS (PAM) CLUSTERING ON CITY 2WEEK, WEEK, MONTH   #
#############################################################################


# Load libraries
library(caTools)
library(Hmisc)
library(plyr)
library(binr)
library(HSAUR)
library(ggplot2)
library(plyr)
library(dplyr)
library(cluster)


# Function for taking only complete rows 
complete_fct <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
normalize <- function(x) {
  return ((x-min(x)) /  (max(x) - min(x)))
}


# set the seed
set.seed(666)


# Load data 
setwd("C:\\Projects\\Cluster_Analysis\\data\\")
df <- read.csv("Data_All.csv", sep=",")


# Select the data
df <- subset(df,df$product_name %in% c("City 2-Week","City Month","City Week"))
df <- df[with(df, order(date)), ]
#write.csv(df,"Data_filt_products.csv")
df <- df[1:120000,]


# Rearrange data
df <- df[ , -which(names(df)==c("default_flag"))]
names(df)[names(df) == 'default_abs'] <- 'default_flag'
df <- subset(df,df$online_cum==0 & df$offline_cum==0)


# Giving product number for each product
df$product_nb <- ifelse(df$product_name=="City Week", 1,
                 ifelse(df$product_name=="City 2-Week", 2,
                 ifelse(df$product_name=="City Month", 3, 4)))


# Take only complete rows
df <- complete_fct(df, c("total_income", "sub_status", "ckr_status", "on_address"))


# Rearrange some fields
for (i in (1:nrow(df))){
  if (is.na(df$income[i])) {df$income[i]=0}
  if (is.na(df$additional_income[i])) {df$additional_income[i]=0}
}


# Test with all potential characteristics
cols <- c("default_flag", "amount", "ownership", "education", "marital_status",
          "household_total", "household_children", "on_address", "maturity", "age", "gender", "period", 
          "income","additional_income", "ratio_installment_income", "total_income")


# Reselect important characteristics
cols <- c("amount", "ratio_installment_income", "total_income", "default_flag","gender", "education")
df_clust <- df[,cols]


# Create dissymmetry maxtrix 
split <- sample.split(df_clust$default_flag, SplitRatio = 0.5)
df_sample <- subset(df_clust, split == TRUE)
#df_sample <- as.data.frame(lapply(df_sample, normalize))
diss_matrix <- daisy(df_sample, metric = "gower", stand = TRUE)


# Apply PAM 
pam_model <- pam(diss_matrix, diss = TRUE, k = 6)


# Check results
pam_results <- df_sample %>%
  mutate(cluster = pam_model$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


# Rejoin cluster number to initial dataframe
df_all <- cbind(df_sample,pam_model$clustering)
names(df_all)[ncol(df_all)] <- "cluster"
table(df_all$cluster)


# Get the best number of clusters (the higher the value of the Silhouette Width, the better)
#sil_width <- c(NA)
#for(i in 2:20){
# pam_fit <- pam(diss_matrix, diss = TRUE, k = i)
# sil_width[i] <- pam_fit$silinfo$avg.width
#}




