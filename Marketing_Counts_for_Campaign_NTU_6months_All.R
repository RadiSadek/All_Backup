
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

# Check if acitves now 
df_actives <- subset(df,df$status==4)

# Subset those who answer criteria
df1 <- subset(df,df$status==5)
df1 <- df1[rev(order(df1$deactivated_at)),]
df1 <- df1[order(df1$egn),]
df1 <- df1[!duplicated(df1$egn),]
df1 <- subset(df1,df1$deactivated_at>="2019-07-01" & 
                  df1$deactivated_at<="2020-06-30")

# Remove those who have current active credit
df1 <- df1[!(df1$egn %in% df_actives$egn),]

# Take the last to be a nicely terminated credit
df1 <- subset(df1,df1$sub_status %in% c(123,126,128))

# Final output
df1 <- df1[,c("id","credit_number","client_id","egn",
              "sub_status","deactivated_at")]

# Remove eventual duplicates
df1 <- df1[!duplicated(df1$client_id),]

# Check number 
nrow(df1)

# Remove those who have current offers
offer_po <- suppressWarnings(fetch(dbSendQuery(con, 
"SELECT client_id,created_at,deleted_at
FROM citycash_db.clients_prior_approval_applications
WHERE deleted_at IS NULL"), n=-1))
df1 <- df1[!(df1$client_id %in% offer_po$client_id),]

# Output
write.xlsx(df1,
  "C:\\Projects\\Score_New_Cases\\Terminated\\input\\all_6months.xlsx")

