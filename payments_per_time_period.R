
############## PAID PER CREDIT  ##############

# Select time window (Included)
beginning <- "2013-01-01"
end <- "2020-08-01"

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

# Read database
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
    dbname=db_name, host=db_host, port = df_port)
data_sql <- suppressWarnings(dbSendQuery(con, 
"SELECT object_id, amount, pay_date 
FROM citycash_db.cash_flow
WHERE nomenclature_id in (90,100,101) 
AND deleted_at IS NULL AND object_type=4"))
paid <- fetch(data_sql, n=-1)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
df <- df[,c("id","egn","credit_number","status")]

# Merge to final dataframe
paid <- merge(paid, df, by.x = "object_id", 
              by.y = "id",
              all.x = TRUE)

# Subset according to time window
if(!is.na(beginning)){
  paid <- subset(paid, 
                 paid$pay_date<=end & 
                 paid$pay_date>=beginning)
}

# Aggregate by credit nummber
paid_agg <- aggregate(paid$amount, by=list(paid$credit_number),
                      FUN=sum)
names(paid_agg) <- c("credit_number","amount")

# Output
setwd("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\")
write.xlsx(paid_agg,"paid_per_credit_number.xlsx")

