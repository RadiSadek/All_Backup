


######## Libraries and functions #########

# Call libraries 
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(RMySQL)))
suppressWarnings(suppressMessages(library(smbinning)))
suppressWarnings(suppressMessages(library(caTools)))
suppressWarnings(suppressMessages(library(plyr)))
suppressWarnings(suppressMessages(library(binr)))
suppressWarnings(suppressMessages(library(party)))
suppressWarnings(suppressMessages(library(openxlsx)))

# Main dataframe
setwd("C:\\Projects\\Behavioral_Scoring\\Credirect\\data\\")

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
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, host=db_host, port = df_port)
data_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, "SELECT  id, client_id, credit_number, has_prev_credits, 
ownership, education, egn, household_children, household_total, on_address, 
experience_employer, maturity, date, total_income, marital_status, last_pay_date,
age, gender, purpose, ratio_installment_income, status_work, total_amount,
max_pay_day, online_offline, date_default, deactivated_at, days_delay, 
default_flag, status, sub_status, amount, amount_paid, product_id
FROM test.data_final")))
df <- fetch(data_sql, n=-1)

data_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, "SELECT  api_application_id , is_express, email,
referral_source, referral_campaign, ip, user_agent, created_at, api_request_at, phone
FROM credirect.applications")))
internet <- fetch(data_sql2, n=-1)

data_ckr_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, "SELECT id, client_id, created_at
FROM citycash_db.clients_ckr_files")))
data_ckr <- fetch(data_ckr_sql, n=-1)

data_ckr_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, "SELECT *
FROM citycash_db.clients_ckr_files_data")))
data_ckr2 <- fetch(data_ckr_sql2, n=-1)

data_sql3 <- suppressMessages(suppressWarnings(dbSendQuery(con, "select * from test.paid_per_client")))
paids <- fetch(data_sql3, n=-1)


########## Set data fields ###########

# Subset some more
df <- subset(df,df$status %in% c(4,5))
df <- subset(df, df$sub_status!=129 | is.na(df$sub_status))


# Calculate number of previous online, offline credits, date difference and number of previous defaulted and refinance credits
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]

df$online <- ifelse(df$online_offline=="online",1,0)
df$offline <- ifelse(df$online_offline=="offline",1,0)
df$amount_paid <- ifelse(is.na(df$amount_paid), 0, df$amount_paid)
df$total_amount_paid <- df$amount_paid
df$offline_cum <- 0
df$online_cum <- 0
df$credits_cum <- 0
df$next_credit <- 0
df$recency <- 0

df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
for (i in 2:(nrow(df)-1)) {
  if (df$egn[i]==df$egn[i-1] & df$status[i-1]==5) 
  {
  df$online_cum[i] <- df$online[i]+df$online_cum[i-1]
  df$offline_cum[i] <- df$offline[i]+df$offline_cum[i-1]
  }
  else if (df$egn[i]==df$egn[i-1]) {
    df$offline_cum[i] <- df$offline_cum[i-1]
    df$online_cum[i] <- df$online_cum[i-1]
  }
  if(df$egn[i]==df$egn[i-1]){
    df$total_amount_paid[i] <- df$total_amount_paid[i-1] + df$amount_paid[i]
    df$recency[i] <- difftime(df$date[i], df$date[i-1], units=c("days"))
  }
  df$credits_cum[i] <- df$online_cum[i] + df$offline_cum[i]
  if(df$egn[i]==df$egn[i+1]){
    df$next_credit[i] <- 1
  }
}

df <- df[2:(nrow(df)-1),]



########## Output ###########
setwd("C:\\Projects\\PTB\\data\\")
write.xlsx(df, "input.xlsx")


