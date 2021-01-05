

# Call libraries 
library(dplyr)
library(RMySQL)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
data_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, "
SELECT *FROM test.data_final")))
df <- fetch(data_sql, n=-1)
Encoding(df$product_name) <- "UTF-8"

# Subset and choose month
df <- subset(df, df$status %in% c(4,5))
df$month <- substring(df$date,1,7)

# Read emails
emails <- fetch(suppressMessages(suppressWarnings(
dbSendQuery(con, "
SELECT id, email FROM citycash_db.clients"))), n=-1)
df <- merge(df, emails, by.x = "client_id", by.y = "id", all.x = TRUE)

# Remove empty emails
df <- subset(df, !is.na(df$email))

# Output results
setwd("C:\\Users\\rsadek.CC\\Desktop\\Generic\\")
write.csv(df[,c("credit_number","email")],"emails.csv")


