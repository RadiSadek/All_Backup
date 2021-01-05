

# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)

# Make connection with SQL
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
df <- subset(df,df$status %in% c(4,5))

# Compute flag if repeat for entire company
df <- df[order(df$date),]
df <- df[order(df$egn),]
for(i in 2:nrow(df)){
  if(df$egn[i]==df$egn[i-1]){
    df$has_prev_cred[i] <- 1
  }
}

# Compute flag if repeat for each branch
df_credirect <- subset(df, df$online_offline=="online")
df_credirect <- df_credirect[order(df_credirect$date),]
df_credirect <- df_credirect[order(df_credirect$egn),]
df_credirect$has_prev_cred_company <- 0
for(i in 2:nrow(df_credirect)){
  if(df_credirect$egn[i]==df_credirect$egn[i-1]){
    df_credirect$has_prev_cred_company[i] <- 1
  }
}

# Compute flag if repeat for each branch
df_citycash <- subset(df, df$online_offline=="offline")
df_citycash <- df_citycash[order(df_citycash$date),]
df_citycash <- df_citycash[order(df_citycash$egn),]
df_citycash$has_prev_cred_company <- 0
for(i in 2:nrow(df_citycash)){
  if(df_citycash$egn[i]==df_citycash$egn[i-1]){
    df_citycash$has_prev_cred_company[i] <- 1
  }
}

# Rebind 
df <- rbind(df_credirect, df_citycash)
df <- df[,c("credit_number","id",
            "online_offline","date",
            "has_prev_cred","has_prev_cred_company")]

# Output
setwd("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\")
write.xlsx(df,"has_prev_credits_company.xlsx")




