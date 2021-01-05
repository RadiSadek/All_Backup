
######## Get average credits after first one #######

# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)

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

# Subset City Cash
df <- subset(df,df$online_offline=="offline")
df_actives <- subset(df,df$status==4)

# Subset count of males
df1 <- subset(df,df$status==5)
df1 <- df1[rev(order(df1$deactivated_at)),]
df1 <- df1[order(df1$egn),]
df1 <- df1[!duplicated(df1$egn),]
df1 <- subset(df1,df1$deactivated_at<=(as.Date(Sys.time())-180))
df1 <- subset(df1,df1$deactivated_at>="2019-01-01")
df1 <- df1[!(df1$egn %in% df_actives$egn),]
df1_raw <- df1
df1 <- subset(df1,df1$gender==1)
nrow(df1)
df1 <- df1[,c("id","credit_number","client_id","egn",
              "sub_status","deactivated_at")]
df1 <- subset(df1,df1$sub_status %in% c(123,126,128))
# write.xlsx(df1,
#  "C:\\Projects\\Score_New_Cases\\Terminated\\input\\males_6months.xlsx")

# Subset count of pensioner
df2 <- subset(df1_raw,df1_raw$status_work %in% c(7,8,11))
df2 <- df2[,c("id","credit_number","client_id","egn",
              "sub_status","deactivated_at")]
df2 <- subset(df2,df2$sub_status %in% c(123,126,128))
nrow(df2)
# write.xlsx(df2,
#  "C:\\Projects\\Score_New_Cases\\Terminated\\input\\pensioner_6months.xlsx")

# Subset count of pensioner
df3 <- subset(df1_raw,df1_raw$age>=60)
df3 <- subset(df1_raw,df1_raw$age>=60 | 
              df1_raw$status_work %in% c(7,8,11))
df3 <- df3[,c("id","credit_number","client_id","egn","age","status_work",
              "sub_status","deactivated_at")]
df3 <- subset(df3,df3$sub_status %in% c(123,126,128))
nrow(df3)
# write.xlsx(df3,paste("C:\\Projects\\Score_New_Cases\\Terminated\\input\\",
# "pensioner_and_more_60years_6months.xlsx",sep = ""))





