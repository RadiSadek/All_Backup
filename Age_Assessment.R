
# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)
library(basicTrendline)
library(mltools)
library(ggplot2)
library(rworldmap)
library(dplyr)
library(raster)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Read and make connection
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, 
                 host=db_host, port = df_port)

# Read entire dataframe
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

# Subset
df <- subset(df,df$status %in% c(4,5))
df <- subset(df,is.na(df$sub_status) | !(df$sub_status %in% c(122,129)))
df <- subset(df,df$online_offline=="offline")
df <- subset(df,df$date>="2019-06-01" & df$date<="2020-06-01")

# Create fields
df$days_delay_bin <- ifelse(df$days_delay<=30,"[0,30]",
     ifelse(df$days_delay<=60,"(30,60]",
     ifelse(df$days_delay<=90,"(60,90]","(90,Inf]")))
df$age_young <- ifelse(df$age<=23,1,
    ifelse(df$age<=60,2,3))
df$age_old <- ifelse(df$age>=70,1,0)
                            
# Get scoring
scoring <- fetch(suppressWarnings(dbSendQuery(con,
"SELECT application_id,score,amount,period 
FROM citycash_db.credits_applications_scoring")), n=-1)
df <- merge(df,scoring,by.x = c("id","amount","installments"),
  by.y = c("application_id","amount","period"),all.x = TRUE)

# Get pora4itel
poratchitel <- fetch(suppressWarnings(dbSendQuery(con,
"SELECT application_id, egn as egn_poratchitel
FROM citycash_db.credits_applications_surety")), n=-1)
df <- merge(df,poratchitel,by.x = "id",by.y = "application_id",all.x = TRUE)
df$pora4itel <- ifelse(!is.na(df$egn_poratchitel),"Yes","No")

# Output
output <- df[,c("credit_number","age_old","pora4itel")]
write.xlsx(output,"C:\\Users\\rsadek.CC\\Desktop\\out.xlsx")

