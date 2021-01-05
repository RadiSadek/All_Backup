

###################################################
# K-MEANS CLUSTERING ON CITY 2WEEK, WEEK, MONTH   #
###################################################


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
library(plyr)



# Function for taking only complete rows 
complete_fct <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
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
cols <- c("default_flag", "amount", "ownership", "education", "marital_status",
          "household_total", "on_address", "age", "gender", "period", 
          "ratio_installment_income", "total_income")
df_clust <- df[,cols]


# Elbow method for choosing the right number of clusters given this data 
#k_max <- 30
#wss <- sapply(1:k_max, function(k){kmeans(df_clust, k, nstart = 50, iter.max = 30)$tot.withinss})
#plot(1:k_max, wss/wss[1], type="b", pch = 19,  frame = FALSE, xlab="Number of clusters K", 
#     ylab="Adim. sum of squares")


# Apply K-means clustering
fit <- kmeans(df_clust, centers = 8,  nstart = 50, iter.max = 30)
cluster_centers <- as.data.frame(fit$centers)
cluster_centers$size <- fit$size
cluster_centers 


# Rejoin cluster number with whole data
df <- cbind(df, fit$cluster)
#df_clust <- cbind(df[,cols], fit$cluster)
names(df)[ncol(df)] <- "cluster"
View(df_clust)


# Visualize results
table(df$product_nb, df$cluster)
table(df$cluster)
plot(df$total_income, df$amount, col = df$cluster, log = "xy")
legend("topright", legend = unique(df$cluster), pch = 16, col = unique(df$cluster))
#text(df$total_income, df$amount, labels=df$cluster, cex= 0.7)


