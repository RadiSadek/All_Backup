


######## Libraries and functions #########

# Call libraries 
library(dplyr)
library(RMySQL)

# Create function for ordering by date 
order_fct <- function(data){
  return (data[with(data, order(date)), ])
}

############ Make connection with SQL  ##############

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
                 host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")


data_ckr_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, client_id, created_at
FROM citycash_db.clients_ckr_files")))
data_ckr <- fetch(data_ckr_sql, n=-1)

data_ckr_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT * FROM citycash_db.clients_ckr_files_data")))
data_ckr2 <- fetch(data_ckr_sql2, n=-1)

ckr <- suppressWarnings(merge(data_ckr, data_ckr2, 
    by.x = "id", by.y = "file_id", all.x = TRUE))


########## Read data ###########

# Main dataframe
setwd("C:\\Projects\\Fraud_Profiling\\input")

# Subset some more
df <- subset(df,df$status %in% c(4,5))
df <- subset(df,df$online_offline=="online")
df <- subset(df,df$has_prev_credits==0)

# Correct some fields
df$amount_paid <- ifelse(is.na(df$amount_paid), 0, df$amount_paid)

# Amount paid as percentage
df$profit <- df$amount_paid/df$amount

# Product category 
df$product_cat <- ifelse(df$product_id %in% c(9,48) , 
     "Credirect_User", "Credirect_14-30")

# Choose final date
df <- df[order(df$date),]
df$default_flag_cum <- 0
for(i in 2:nrow(df)){
  df$default_flag_cum[i] <- df$default_flag_cum[i-1] + df$default_flag[i]
}

# Filter time window
df$difftime <- difftime(as.Date(Sys.time()), as.Date(df$max_pay_day), 
    units=c("days"))
df <- subset(df, df$difftime>=90)
df <- subset(df, df$sub_status!=129 | is.na(df$sub_status))
df <- subset(df, df$date<"2019-11-13" & df$date>"2018-04-10")

# Correct age, total_income, ratio_installment_income
df$total_income <- ifelse(df$total_income<100, NA, df$total_income)
df$total_income <- ifelse(df$total_income>8000, 8000, df$total_income)
df$ratio_installment_income <- ifelse(df$ratio_installment_income>4, 4, 
    df$ratio_installment_income)

# Work on CKR
data_small <- df[,c("id","date", "client_id")]
names(data_small)[1] <- "application_id"

# CKR financial
ckr_financial <- subset(ckr, ckr$type==2)
data_ckr_fin <- merge(data_small, ckr_financial, 
     by.x = "client_id", by.y = "client_id")
data_ckr_fin$diff_time_ckr <- abs(difftime(data_ckr_fin$date, 
    data_ckr_fin$created_at, units=c("days")))
data_ckr_fin <- data_ckr_fin[with(data_ckr_fin, order(diff_time_ckr)), ]
data_ckr_fin <- data_ckr_fin[with(data_ckr_fin, order(application_id)), ]
data_ckr_fin <- data_ckr_fin[,c("application_id","cred_count",
     "source_entity_count","amount_drawn","monthly_installment",
     "current_status_active","status_active","status_finished",
     "outstanding_performing_principal","outstanding_overdue_principal")]
names(data_ckr_fin) <- c("application_id","cred_count_financial",
     "source_entity_count_financial","amount_drawn_financial",
     "monthly_installment_financial","current_status_active_financial",
     "status_active_financial","status_finished_financial",
     "outs_principal_financial","outs_overdue_financial")
data_ckr_fin <- data_ckr_fin[!duplicated(data_ckr_fin$application_id),]

# CKR bank
ckr_bank <- subset(ckr, ckr$type==1)
data_ckr_bank <- merge(data_small, ckr_bank, 
    by.x = "client_id", by.y = "client_id")
data_ckr_bank$diff_time_ckr <- abs(difftime(data_ckr_bank$date, 
    data_ckr_bank$created_at, units=c("days")))
data_ckr_bank <- data_ckr_bank[with(data_ckr_bank, order(diff_time_ckr)), ]
data_ckr_bank <- data_ckr_bank[with(data_ckr_bank, order(application_id)), ]
data_ckr_bank <- data_ckr_bank[,c("application_id","cred_count",
    "source_entity_count","amount_drawn","monthly_installment",
    "current_status_active","status_active","status_finished",
    "outstanding_performing_principal","outstanding_overdue_principal")]
names(data_ckr_bank) <- c("application_id","cred_count_bank",
    "source_entity_count_bank","amount_drawn_bank",
    "monthly_installment_bank","current_status_active_bank",
    "status_active_bank","status_finished_bank",
    "outs_principal_bank","outs_overdue_bank")
data_ckr_bank <- data_ckr_bank[!duplicated(data_ckr_bank$application_id),]


# Merge CKR data
df <- merge(df, data_ckr_fin, by.x = "id", 
            by.y = "application_id", all.x = TRUE)
df <- merge(df, data_ckr_bank, by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Re-work CKR
df$status_active_financial <- ifelse(df$cred_count_financial==0, -1, 
    df$status_active_financial)
df$status_active_bank <- ifelse(df$cred_count_bank==0, -1, 
    df$status_active_bank)
df$outs_overdue_ratio_bank <- ifelse(df$outs_overdue_bank==0 & 
    df$outs_principal_bank==0, -999,
    df$outs_overdue_bank/df$outs_principal_bank)                 
df$outs_overdue_ratio_financial <- ifelse(df$outs_overdue_financial==0 & 
    df$outs_principal_financial==0, -999,
    df$outs_overdue_financial/df$outs_principal_financial)

df$outs_overdue_ratio_bank <- ifelse(df$outs_overdue_ratio_bank>1, 1, 
    df$outs_overdue_ratio_bank)
df$outs_overdue_ratio_financial <- ifelse(df$outs_overdue_ratio_financial>1, 1, 
    df$outs_overdue_ratio_financial)


df$source_entity_count_total <- df$source_entity_count_bank + 
  df$source_entity_count_financial
df$amount_drawn_total <- df$amount_drawn_bank + df$amount_drawn_financial
df$monthly_installment_total <- df$monthly_installment_bank + 
  df$monthly_installment_financial
df$current_status_active_total <- ifelse(
  df$current_status_active_financial>df$current_status_active_bank, 
  df$current_status_active_financial, df$current_status_active_bank)
df$status_active_total <- ifelse(
  df$status_active_financial>df$status_active_bank, 
  df$status_active_financial, df$status_active_bank)
df$status_finished_total <- ifelse(
  df$status_finished_financial>df$status_finished_bank, 
  df$status_finished_financial, df$status_finished_bank)
df$cred_count_total <- df$cred_count_bank + df$cred_count_financial
df$outs_overdue_ratio_total <- ifelse(
  df$outs_overdue_ratio_bank>df$outs_overdue_ratio_financial, 
  df$outs_overdue_ratio_bank, df$outs_overdue_ratio_financial)

# Order by date
df <- df[order(df$date),]

# Output data 
setwd("C:\\Projects\\Fraud_Profiling\\input\\")
write.csv(df,"input.csv")



