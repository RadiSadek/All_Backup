

##################################################
## PREPARE INPUT FILE FOR CITY CASH APPLICATION ##
##################################################


######## Libraries and functions #########

# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)

# Create function for ordering by date 
order_fct <- function(data){
  return (data[with(data, order(date)), ])
}




############ Make connection with SQL  ##############

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
data_encoding <- suppressWarnings(dbSendQuery(con,'set character set "utf8"'))
df_raw <- df

# Read input files
setwd("C:\\Projects\\Application_Scoring\\Credirect_v4\\data")
df <- read.csv("input_current.csv", sep=",")

# Append result from API JSON transformation
json <- read.csv("APi_Applications.csv", sep=",")








