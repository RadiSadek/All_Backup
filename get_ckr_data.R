

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
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

# Make SQL connection 
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)

# Read CKR data
data_ckr_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, client_id, created_at
FROM citycash_db.clients_ckr_files")))
data_ckr <- fetch(data_ckr_sql, n=-1)

data_ckr_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM citycash_db.clients_ckr_files_data")))
data_ckr2 <- fetch(data_ckr_sql2, n=-1)

# Get CKR data
ckr <- suppressWarnings(merge(data_ckr, data_ckr2, by.x = "id",
                              by.y = "file_id", all.x = TRUE))

# Work on CKR
data_small <- df[,c("id","date_entry", "client_id")]
names(data_small)[1] <- "application_id"

# CKR financial
ckr_financial <- subset(ckr, ckr$type==2)
data_ckr_fin <- merge(data_small, ckr_financial, by.x = "client_id", 
                      by.y = "client_id")
data_ckr_fin$diff_time_ckr <- difftime(data_ckr_fin$date_entry, 
    data_ckr_fin$created_at,  units=c("days"))
data_ckr_fin <- subset(data_ckr_fin,data_ckr_fin$diff_time_ckr>-1)
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
data_ckr_bank <- merge(data_small, ckr_bank, by.x = "client_id",
                       by.y = "client_id")
data_ckr_bank$diff_time_ckr <- difftime(data_ckr_bank$date_entry, 
    data_ckr_bank$created_at,  units=c("days"))
data_ckr_bank <- subset(data_ckr_bank,data_ckr_bank$diff_time_ckr>-1)
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
df <- merge(df, data_ckr_fin, by.x = "id", by.y = "application_id", 
            all.x = TRUE)
df <- merge(df, data_ckr_bank, by.x = "id", by.y = "application_id", 
            all.x = TRUE)

# Set status active
df$status_active_financial <- ifelse(df$cred_count_financial==0, -1, 
      df$status_active_financial)
df$status_active_bank <- ifelse(df$cred_count_bank==0, -1, 
      df$status_active_bank)
df$status_active_total <- ifelse(
  df$status_active_financial>df$status_active_bank, 
  df$status_active_financial, df$status_active_bank)

# Set status finished
df$status_finished_total <- ifelse(
  df$status_finished_financial>df$status_finished_bank, 
  df$status_finished_financial, df$status_finished_bank)

# Set credit counts, monthly installments and source entities
df$source_entity_count_total <- df$source_entity_count_bank + 
  df$source_entity_count_financial
df$amount_drawn_total <- df$amount_drawn_bank +
  df$amount_drawn_financial
df$monthly_installment_total <- df$monthly_installment_bank + 
  df$monthly_installment_financial
df$cred_count_total <- df$cred_count_bank + df$cred_count_financial

# Set current status active
df$current_status_active_total <- ifelse(
  df$current_status_active_financial>df$current_status_active_bank, 
  df$current_status_active_financial, df$current_status_active_bank)

# Set ratio outstanding overdue / outstanding principal 
df$outs_overdue_ratio_bank <- ifelse(df$outs_overdue_bank==0 & 
      df$outs_principal_bank==0, -999,
      df$outs_overdue_bank/df$outs_principal_bank)                 
df$outs_overdue_ratio_financial <- ifelse(
      df$outs_overdue_financial==0 & df$outs_principal_financial==0, -999,
      df$outs_overdue_financial/df$outs_principal_financial)
df$outs_overdue_ratio_bank <- ifelse(
     df$outs_overdue_ratio_bank>1, 1, df$outs_overdue_ratio_bank)
df$outs_overdue_ratio_financial <- ifelse(
     df$outs_overdue_ratio_financial>1, 1, df$outs_overdue_ratio_financial)
df$outs_overdue_ratio_total <- ifelse(
     df$outs_overdue_ratio_bank>df$outs_overdue_ratio_financial, 
     df$outs_overdue_ratio_bank, df$outs_overdue_ratio_financial)

# Set ratio outstanding overdue / amount drawn (only financial)
df$outs_overdue_ratio_amount_drawn_financial <- ifelse(
  df$outs_overdue_financial==0 & df$outs_principal_financial==0, -999,
  df$outs_overdue_financial/df$amount_drawn_financial)


# Order by date
df <- df[order(df$date),]

# Make final dataframe
df <- df[,c("id","credit_number","date","egn", "status_active_financial",
  "status_active_total","status_finished_total","source_entity_count_total",                
  "amount_drawn_total","monthly_installment_total","cred_count_total",
  "current_status_active_total","outs_overdue_ratio_total",
  "outs_overdue_ratio_amount_drawn_financial")]
df$outs_overdue_ratio_total <- round(df$outs_overdue_ratio_total,3)
df$outs_overdue_ratio_amount_drawn_financial <- 
  round(df$outs_overdue_ratio_amount_drawn_financial,3)

# Output data
write.xlsx(df,"C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\ckr_data.xlsx")




