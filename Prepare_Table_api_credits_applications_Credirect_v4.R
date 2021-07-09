
# Load libraries and data
library(openxlsx)
library(dplyr)
library(RMySQL)
library(openxlsx)
library(rjson)
library(jsonlite)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Load data
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
                 host=db_host, port = df_port)
data_encoding <- suppressWarnings(dbSendQuery(con,'set character set "utf8"'))
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
df_raw <- df

# Set working directory
setwd("C:\\Projects\\Application_Scoring\\Credirect_v4\\data\\")

# Read credits to append API
credits <- read.csv("input_current.csv")
credits <- merge(credits,df_raw[,c("credit_number","id")],
                 by.x = "credit_number",by.y = "credit_number",all.x = TRUE)

# Read table
df <- suppressWarnings(fetch(dbSendQuery(con,paste(
"SELECT * FROM
citycash_db.api_credits_applications", sep="")), n=-1))
Encoding(df$payload) <- "UTF-8"
df <- df[df$application_id %in% credits$id,]

# Get all possible column names from API credits applications
tot <- unique(names(as.data.frame(do.call(rbind,lapply(df$payload[1],
   function(j) as.list(unlist(fromJSON(j, flatten=TRUE))))))))
for(i in 1:nrow(df)){
  tot <- c(tot,names(as.data.frame(do.call(rbind,lapply(df$payload[i],
  function(j) as.list(unlist(fromJSON(j, flatten=TRUE))))))))
}
columnnames <- unique(tot)

# Create new dataframe
df_json <- as.data.frame(matrix(NA,ncol=(length(columnnames)+1),nrow=nrow(df)))
names(df_json) <- c("app_id",columnnames)

# Get fields
for(i in 1:nrow(df_json)){
  df_json$app_id[i] <- df$application_id[i]
  json <- as.data.frame(do.call(rbind,lapply(df$payload[i], 
    function(j) as.list(unlist(fromJSON(j, flatten=TRUE))))))
  for(j in 1:ncol(json)){
    df_json[i,c(names(json)[j])] <- json[[j]]
  }
}

# Create new fields
for(i in 1:nrow(df_json)){
  df_json$email_domain[i] <- strsplit(df_json$client.email[i],"@")[[1]][2]
  df_json$email_name[i] <- strsplit(df_json$client.email[i],"@")[[1]][1]
  df_json$os[i] <- strsplit(strsplit(
    df_json$user_agent[i],"\\(")[[1]][2],";")[[1]][1]
  df_json$device_os[i] <- strsplit(strsplit(
    df_json$user_agent[i],"\\(")[[1]][2],";")[[1]][2]
  df_json$email_char[i] <- nchar(df_json$email_name[i])
  df_json$email_char_ratio[i] <- nchar(rawToChar(
    unique(charToRaw(df_json$email_name[i])))) / nchar(df_json$email_name[i])
}
df_json$device <- ifelse(grepl("Android",df_json$user_agent),"Android",
   ifelse(grepl("iPhone",df_json$user_agent),"iPhone",
   ifelse(grepl("Win64",df_json$user_agent),"Windows",
   ifelse(grepl("Windows",df_json$user_agent),"Windows",
   ifelse(grepl("Linux",df_json$user_agent),"Linux","other")))))
df_json$hour_app <- as.numeric(substring(df_json$partial_application_at,12,13))


# Get interesting columns
final <- df_json[,c("app_id","amount","period","installments","payment.method",
 "product_id","referral_source","referral_campaign","device",
 "email_char","email_domain","email_char_ratio","hour_app")]
names(final) <- c("application_id","API_amount","API_period","API_installments",
  "API_payment_method","API_product_id","API_referral_source",
  "API_referral_campaign","API_device","API_email_char","API_email_domain",
  "API_email_char_ratio","API_hour_app")
final <- merge(final,df_raw[,c("id","credit_number")],
               by.x = "application_id",by.y = "id",all.x = TRUE)

# Output result
write.xlsx(final,"Api_Applications.xlsx")


