
#######################################################
############# CHECK SCORING PER TIME PERIOD ###########
#######################################################

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
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

# Read scoring table
scoring_sql <- suppressWarnings(dbSendQuery(con, "SELECT 
   application_id, amount, score, period
   FROM citycash_db.credits_applications_scoring"))
scoring <- fetch(scoring_sql, n=-1)

# Merge scoring
df <- df[,c("id","credit_number","amount","installments","date_entry")]
df <- merge(df, scoring, by.x = c("id","amount","installments"), 
            by.y = c("application_id","amount","period"), 
            all.x = TRUE)
df <- df[!duplicated(df$id),]

# Get those with no scoring
no_score <- subset(df,is.na(df$score) & df$id %in% scoring$application_id)

for(i in 1:nrow(no_score)){
  subset_scoring <- subset(scoring,scoring$application_id==no_score$id[i])
  closest_amount <- subset_scoring$amount[which.min(abs(subset_scoring$amount
                                                 - no_score$amount[i]))]
  
  
}







#######
# End #
#######

