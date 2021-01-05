

############# CHECK SCORE PERFORMANCE FROM DATABASE ###########

# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)
library(Hmisc)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Load data
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
                 host=db_host, port = df_port)
data_sql <- suppressWarnings(dbSendQuery(con, "SELECT * FROM test.data_final"))
df <- fetch(data_sql, n=-1)
df <- subset(df, df$status %in% c(4,5))

# Compute relevant fields
df <- df[order(df$date_entry),]
df <- df[order(df$egn),]
df$has_prev_credit <- 0
df$deactivated_prev <- NA
for(i in 2:nrow(df)){
  if(df$egn[i]==df$egn[i-1]){
    df$has_prev_credit[i] <- 1
    df$deactivated_prev[i] <- df$deactivated_at[i-1]
  }
}

# Output
setwd("C:\\Users\\rsadek.CC\\Desktop")
write.xlsx(df[,c("egn","id","signed_at","deactivated_prev")], "prev_deactivation.xlsx")


