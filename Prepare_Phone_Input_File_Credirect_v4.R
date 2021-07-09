
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

# Read table
seon_phones <- read.csv("SEON_phone.csv")

# Remove redudant column names
for(i in 1:length(names(seon_phones))){
  names(seon_phones)[i] <- sub('data.phone_details.account_details.','',
                               names(seon_phones)[i])
  names(seon_phones)[i] <- sub('data.phone_details.','',names(seon_phones)[i])
  names(seon_phones)[i] <- sub('data.','',names(seon_phones)[i])
}

# Read phones
phones <- read.xlsx(
  "C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\all_phone_numbers.xlsx")
phones$pers_number_1_short <- substring(phones$pers_number_1,2,12)
seon_phones$phone_number_short <- substring(seon_phones$phone_number,4,12)
seon_phones <- merge(seon_phones,
                     phones[,c("pers_number_1_short","credit_number")],
   by.x = "phone_number_short",by.y = "pers_number_1_short",all.x = TRUE)

# Output 
write.csv(seon_phones,"seon_phone_v2.csv")



