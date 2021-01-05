

############# SELECT CREDITS TO BE SCORED  ###########

beginning <- "2019-06-01 00:00:00"
end <- "2019-07-31 23:59:59"

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
data_sql <- suppressWarnings(dbSendQuery(con, "
SELECT * FROM test.data_final"))
df <- fetch(data_sql, n=-1)

# Choose credits
df <- subset(df, df$status %in% c(5))
df$deactivated_short <- substring(df$deactivated_at,1,10)
df <- subset(df, df$deactivated_short>=beginning & df$deactivated_short<=end)

# Subset correct credits
df <- subset(df, df$sub_status %in% c(123,128))

# Load data
data_sql <- suppressWarnings(dbSendQuery(con, "
SELECT * FROM test.data_final"))
all <- fetch(data_sql, n=-1)

# Choose credits
all <- subset(all , all $status %in% c(4,5))

# Choose columns
df2 <- df[,c("id","egn","deactivated_at")]
df2 <- merge(df2, all[,c("egn","deactivated_at","signed_at")], by.x = "egn",
             by.y = "egn", all.x = TRUE)

# Make flag if active before
df2$flag_active_before <- ifelse(is.na(df2$deactivated_at.y) &
                                df2$deactivated_at.x>df2$signed_at, 1,
                          ifelse(df2$deactivated_at.x<df2$deactivated_at.y &
                                df2$deactivated_at.x>df2$signed_at, 1, 0))



