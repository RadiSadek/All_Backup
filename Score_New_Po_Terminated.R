

################################################################################
#         Script for generating  daily offers for terminated credits           #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 1.0 (2020/06/23)                            #
################################################################################



########################
### Initial settings ###
########################

# Libraries
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))


# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)


# Define work directory
main_dir <- "C:\\Projects\\Apply_Scoring\\"


# Load other r files
source(paste(main_dir,"Apply_Models\\Terminated_Radi.r",sep=""))
source(paste(main_dir,"Apply_Models\\SQL_queries.r", sep=""))
source(paste(main_dir,"Apply_Models\\Useful_Functions.r", sep=""))


# Define product id
product_id <- NA



#####################
### Compute score ###
#####################

# Compute and append score
select_credits <- read.xlsx(
  "C:\\Projects\\Score_New_Cases\\Terminated\\input\\all_6months.xlsx",1)
select_credits$max_amount <- NA
select_credits$max_installment_amount <- NA
select_credits$score_max_amount <- NA
select_credits$max_delay <- NA
for(i in 1:nrow(select_credits)){
  suppressWarnings(tryCatch({
    client_id <- select_credits$client_id[i]
    last_id <- select_credits$id[i]
    calc <- gen_terminated_fct(con,client_id,product_id,last_id)
    select_credits$max_amount[i] <- calc[[1]]
    select_credits$max_installment_amount[i] <- calc[[2]]
    select_credits$score_max_amount[i] <- calc[[3]]
    select_credits$max_delay[i] <- as.numeric(calc[[4]])
  }, error=function(e){}))
}



##################################
### Reapply selection criteria ###
##################################

# Select based on score and DPD
select_credits_bu <- select_credits
select_credits <- select_credits_bu
save(select_credits, 
 file = "C:\\Projects\\Score_New_Cases\\Terminated\\results\\All_6months.rdata")
select_credits <- subset(select_credits,select_credits$max_amount>-Inf & 
 select_credits$max_amount<Inf)



#####################
### Output result ###
#####################

write.xlsx(select_credits,
  "C:\\Projects\\Score_New_Cases\\Terminated\\results\\All_6months_Scored.xlsx")


