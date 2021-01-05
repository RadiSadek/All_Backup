

library(RMySQL)

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
"SELECT application_id, amount, created_at
FROM citycash_db.credits_reversal_payments"))
paid <- fetch(data_sql, n=-1)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
df <- df[,c("id","egn","credit_number","status")]

# Merge to final dataframe
paid <- merge(paid, df, by.x = "application_id", 
              by.y = "id",
              all.x = TRUE)