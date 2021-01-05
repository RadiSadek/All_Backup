

######################
# K-MODES CLUSTERING #
######################


######### LIBRARIES ##########

library(caTools)
library(Hmisc)
library(plyr)
library(binr)
library(HSAUR)
library(plyr)
library(dplyr)
library(cluster)
library(openxlsx)
library(clustMixType)
library(klaR)
library(data.table)


######### FUNCTIONS ##########

# Function to count % of empties for each variables
count_empty <- function(var){
  return(sum(is.na(var)))
}
# Function for taking only complete rows 
complete_fct <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
# Function for calculating % distribution for each cluster
distr_pct <- function(var) {
  a <- table(df$cluster, var)
  b <- rowSums (a, na.rm = FALSE, dims = 1)
  return (a/b*100)
}
# Define functions for continuous or categorical variables to aggreagate data
mean_cont <- function(x){
  return (aggregate(df[x],data=df,by=list(cluster=df$cluster), FUN=mean))
}
std_cont <- function(x){
  return (aggregate(df[x],by=list(df$cluster),FUN=sd,na.rm=TRUE))
}
mfv_cat <- function(x){
  result <- as.data.frame(as.table(with(df,by(df[x],df$cluster,
                                              function(xx) names(which.max(table(xx)))))))[2]
  names(result) <- x
  return (result)
}


######### BUILD MODEL ##########
# set the seed
set.seed(666000)

# Load data 
setwd("C:\\Projects\\Marketing_Segmentation\\data\\")
df <- read.xlsx("Input_Data.xlsx")

# Default 
df$nb_credirect <- as.numeric(df$nb_credirect)
df$nb_credits <- as.numeric(df$nb_credits)
df$nb_citycash <- as.numeric(df$nb_citycash)
df$credirect_pct <- df$nb_credirect/(df$nb_credits)*100

# Change type of variables
df$default <- as.character(df$default)

# Correct fields
df$purpose <- ifelse(is.na(df$purpose), "Няма данни", df$purpose)
df$total_income <- ifelse(df$total_income>=7000,7000,
                   ifelse(df$total_income<=100,NA,df$total_income))

# Select only credirect
df <- subset(df,df$nb_credirect>0)

# Take only complete rows
round(sapply(df,count_empty)/nrow(df)*100,2)

# Make factors all character variables 
df$gender <- as.factor(df$gender)
df$marital_status <- as.factor(df$marital_status)
df$purpose <- as.factor(df$purpose)
df$type_contract <- as.factor(df$type_contract)
df$day_purchase <- as.factor(df$day_purchase)
df$ownership <- as.factor(df$ownership)
df$address_cat <- as.factor(df$address_cat)
df$source <- as.factor(df$source)
df$education <- as.factor(df$education)

# Filter
cols <- c("age_range","gender","nb_children_range","marital_status",
          "ownership","address_cat","education","total_income_range",
          "default","nb_credits","credirect_pct"
)
df <- df[, cols]

# Test with all potential characteristics
df <- complete_fct(df,names(df))

# Make back-up
df_raw <- df[,c("default","nb_credits","credirect_pct")]

# Select columns (make three types of variable selections)
chars <- c("age_range","gender","nb_children_range","marital_status",
           "ownership","address_cat",
           "total_income_range","education")

# Select relevant variables
df <- df[, chars]

# Build K-prototype model
#kmodel <- kproto(df, k=5, lambda = NULL, iter.max = 1000,
#                 nstart = 1, na.rm = TRUE, keep.data = TRUE, verbose = FALSE)
kmodel <- kmodes(df, modes=4, iter.max = 1000, weighted = FALSE, fast = TRUE)

# Get data from K-prototype model 
cluster_centers <- as.data.frame(kmodel$centers)

# Bind a cluster number column to main dataframe
df <- cbind(df, kmodel$cluster)
names(df)[ncol(df)] <- "cluster"
df$cluster <- as.factor(df$cluster)


######### ANALYZE MODEL ##########

# Analyze clusters (draw interesing plots)
#clprofiles(kmodel, df)

# Get stats for each cluster
character <- as.data.frame(lapply(names(df[ , !unlist(lapply(df, is.numeric))]), mfv_cat))
character <- character[, -grep("cluster", colnames(character))]
character

# Get % of variable distrubtion for each variable
sapply(df[sapply(df, function(x) !is.numeric(x))],distr_pct)

# Rejoin default to make analysis
df <- cbind(df,df_raw)
df$cluster <- as.numeric(df$cluster)
df$default <- as.numeric(df$default)
agg_def <- aggregate(df$default, by=list(id=df$cluster), FUN=mean)
agg_nbcredits <- aggregate(df$nb_credits, by=list(id=df$cluster), FUN=mean)

# Display aggregated results
agg_def
agg_nbcredits

# Check results on most frequent value on cluster 1 (pay attention: it is different than clustering)
cluster1 <- subset(df, df$cluster==1)
cluster1$id <- 1 
c1 <- setDT(cluster1)[, .N, by=.(id,age_range,gender,nb_children_range,marital_status,
                                ownership,address_cat,total_income_range,education)][, .SD[which.max(N)], by = id]



######### END ###########