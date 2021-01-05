

######################################################################
# GET CUSTOMER LIFETIME VALUE BASED ON MONTH OF FIRST CREDIT LENDING #
######################################################################


# Choose end date of study (not included)
end_date <- "2020-08-17"

# Load libraries
library(openxlsx)
library(RMySQL)

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


plan_main_sql <- suppressWarnings(dbSendQuery(con, "SELECT * FROM 
       citycash_db.credits_plan_main"))
plan_main <- fetch(plan_main_sql, n=-1)

# Get aggregate of total amount with installments which has passed
plan_main <- subset(plan_main, plan_main$pay_day<end_date)
plan_main <- merge(plan_main, df[,c("id","credit_number")], 
      by.x = "application_id", by.y = "id", 
      all.x = TRUE)
total_amount_passed <- aggregate(plan_main$principal, 
      by=list(plan_main$credit_number), FUN=sum)
names(total_amount_passed) <- c("credit_number","total_amount_passed")

# Output results
write.xlsx(total_amount_passed, 
           "C:\\Projects\\Customer_LTV\\results\\padejirala_sum.xlsx")



