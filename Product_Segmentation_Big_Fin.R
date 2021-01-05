

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
library(RMySQL)
library(here)
library(party)

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

# Load data SQL query
sql_query <- paste("SELECT * FROM test.data_final")
sql_query2 <- paste("SELECT id,name FROM citycash_db.products")

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, host=db_host, port = df_port)
data_sql <- dbSendQuery(con, sql_query)
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, host=db_host, port = df_port)
data_sql2 <- dbSendQuery(con, sql_query2)

# Read table of product amount and installments  
df <- fetch(data_sql, n=-1)
df <- subset(df, df$status %in% c(4,5))
df <- subset(df, df$date_entry>="2017-06-01")
#df <- subset(df, df$date_entry>="2018-01-01")
products <- fetch(data_sql2, n=-1)
names(products)[2] <- "product_name"
df <- merge(df, products, by.x = "product_id", by.y = "id")
Encoding(df$product_name) <- "UTF-8"

# Load data for city
setwd("C:\\Projects\\Product_Segmentation\\")
cities <- read.xlsx("Input_Data.xlsx")
cities <- cities[,c("egn","address_cat")]
df <- merge(df, cities, by.x = "egn", by.y = "egn")
df$address <- ifelse(df$address_cat=="София",1,
              ifelse(df$address_cat=="Много големи градове",2,
              ifelse(df$address_cat=="Големи градове",3,
              ifelse(df$address_cat=="Средни градове",4,
              ifelse(df$address_cat=="Малки градове",5,6)))))

# Correct fields
df$total_income <- ifelse(df$total_income>=7000,7000,
                   ifelse(df$total_income<=100,NA,df$total_income))

# Categorize product id
df$product <- ifelse(substring(df$product_name,0,6)=="City W","City Week",
              ifelse(substring(df$product_name,0,6)=="City M","City Month",
              ifelse(substring(df$product_name,0,6)=="City 2","City 2-Week",
              ifelse(substring(df$product_name,0,11)=="Credirect П","Credirect",
              ifelse(substring(df$product_name,0,11)=="CrediRect14" | 
                     substring(df$product_name,0,11)=="CrediRect30", "Credirect",
              ifelse(substring(df$product_name,0,5)=="Пенси","Пенсионер",
              ifelse(substring(df$product_name,0,3)=="Big","BigFin","Друг")))))))

df <- subset(df , df$product!="Пенсионер" & df$product!="Друг")


# Factor variables
df$product <- as.factor(df$product)
df$education <- as.factor(df$education)
df$gender <- as.factor(df$gender)
df$marital_status <- as.factor(df$marital_status)
df$status_work <- as.factor(df$status_work)
df$purpose <- as.factor(df$purpose)
df$ownership <- as.factor(df$ownership)
df$ckr_status <- as.factor(df$ckr_status)
df$address <- as.factor(df$address)


# Build decision tree model from "party" library
decision_tree_model <- ctree(product ~ 
                               education + gender + ownership + age + total_income + marital_status + 
                               household_children + address,
                             data = df, 
                             controls = ctree_control(mincriterion = 0.95, minsplit = 15000))

plot(decision_tree_model)

# Get stats on each cluster
df$cluster <- where(decision_tree_model)
results <- suppressWarnings(table(df$product,df$cluster)/matrix(rep(colSums(table(df$product,df$cluster)),3), 
                                    nrow=nrow(table(df$product,df$cluster)), 
                                    ncol=ncol(table(df$product,df$cluster)), byrow=T)*100)
results
results2 <- suppressWarnings(table(df$cluster,df$product)/matrix(rep(colSums(table(df$cluster,df$product)),3), 
                                    nrow=nrow(table(df$cluster,df$product)), 
                                    ncol=ncol(table(df$cluster,df$product)), byrow=T)*100)
results2

# Table 
table(df$product,df$cluster)
