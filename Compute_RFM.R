


######## Libraries and functions #########

# Call libraries 
library(dplyr)
library(RMySQL)
library(smbinning)
library(caTools)
library(plyr)
library(binr)
library(party)

# Main dataframe
setwd("C:\\Projects\\Behavioral_Scoring\\Credirect\\data\\")

# Create function for ordering by date 
order_fct <- function(data){
  return (data[with(data, order(date)), ])
}

string_split <- function(string,index) {
  return(as.numeric(strsplit(strsplit(names(bins(string, 
        target.bins = 5, minpts = 1000)$binct)[index], ",")[[1]][2], "]")[[1]][1]))
}

string_split2 <- function(string,index) {
  return(as.numeric(strsplit(strsplit(levels(cut(string, 5, include.lowest=TRUE))
         [index], ",")[[1]][2], "]")[[1]][1]))
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
data_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT  id, client_id, credit_number, has_prev_credits, 
ownership, education, egn, household_children, household_total, on_address, 
experience_employer, maturity, date, total_income, marital_status, last_pay_date,
age, gender, purpose, ratio_installment_income, status_work, total_amount,
max_pay_day, online_offline, date_default, deactivated_at, days_delay, 
default_flag, status, sub_status, amount, amount_paid, product_id
FROM test.data_final")))
df <- fetch(data_sql, n=-1)

data_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT  api_application_id , is_express, email,
referral_source, referral_campaign, ip, user_agent, created_at, api_request_at, phone
FROM credirect.applications")))
internet <- fetch(data_sql2, n=-1)

data_ckr_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, client_id, created_at
FROM citycash_db.clients_ckr_files")))
data_ckr <- fetch(data_ckr_sql, n=-1)

data_ckr_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM citycash_db.clients_ckr_files_data")))
data_ckr2 <- fetch(data_ckr_sql2, n=-1)

data_sql3 <- suppressMessages(suppressWarnings(dbSendQuery(con,
"select * from test.paid_per_client")))
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
df$offline_cum <- 0
df$online_cum <- 0
df$credits_cum <- 0

df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1] & df$status[i-1]==5) 
  {
  df$online_cum[i] <- df$online[i]+df$online_cum[i-1]
  df$offline_cum[i] <- df$offline[i]+df$offline_cum[i-1]
  }
  else if (df$egn[i]==df$egn[i-1]) {
    df$offline_cum[i] <- df$offline_cum[i-1]
    df$online_cum[i] <- df$online_cum[i-1]
  }
  df$credits_cum[i] <- df$online_cum[i] + df$offline_cum[i]
}

# Compute total amount paid up to credit




########## Filter relevant data (time and type of credit)  ###########

# Filter time window
df$difftime <- difftime(as.Date(Sys.time()), as.Date(df$max_pay_day), units=c("days"))
df <- subset(df, df$date<"2019-01-01" & df$status==5)

# Order
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]

# Compute Recency
df$recency <- difftime(as.Date(Sys.time()), df$date, units=c("days"))
recency <- aggregate(df$recency , by=list(df$egn), FUN=min)
names(recency) <- c("egn","recency_egn")
recency$recency_egn <- round(recency$recency_egn,2)
recency$recency_egn <- as.numeric(recency$recency_egn)
recency$recency_bin <- ifelse(recency$recency_egn<=quantile(recency$recency_egn,prob=0.2),5,
                       ifelse(recency$recency_egn<=quantile(recency$recency_egn,prob=0.4),4,
                       ifelse(recency$recency_egn<=quantile(recency$recency_egn,prob=0.6),3,
                       ifelse(recency$recency_egn<=quantile(recency$recency_egn,prob=0.8),2,1))))

# Compute Frequency
frequency <- aggregate(df$credits_cum, by=list(df$egn), FUN=max)
names(frequency) <- c("egn","frequency_egn")
frequency$frequency_egn <- frequency$frequency_egn + 1
frequency$frequency_bin <- ifelse(frequency$frequency_egn<=quantile(frequency$frequency_egn,prob=0.2),1,
                           ifelse(frequency$frequency_egn<=quantile(frequency$frequency_egn,prob=0.4),2,
                           ifelse(frequency$frequency_egn<=quantile(frequency$frequency_egn,prob=0.6),3,
                           ifelse(frequency$frequency_egn<=quantile(frequency$frequency_egn,prob=0.8),4,5))))

# Computer Monetary
monetary <- aggregate(df$amount_paid, by=list(df$egn), FUN=sum)
names(monetary) <- c("egn","monetary_egn")
monetary$monetary_egn <- as.numeric(monetary$monetary_egn)
monetary$monetary_egn <- ifelse(is.na(monetary$monetary_egn), 0, monetary$monetary_egn)
monetary$monetary_bin <- ifelse(monetary$monetary_egn<=quantile(monetary$monetary_egn,prob=0.2),1,
                         ifelse(monetary$monetary_egn<=quantile(monetary$monetary_egn,prob=0.4),2,
                         ifelse(monetary$monetary_egn<=quantile(monetary$monetary_egn,prob=0.6),3,
                         ifelse(monetary$monetary_egn<=quantile(monetary$monetary_egn,prob=0.8),4,5))))


# Get RFM
rfm <- merge(recency, monetary, by.x = "egn", by.y = "egn", all.x = TRUE)
rfm <- merge(rfm , frequency, by.x = "egn", by.y = "egn", all.x = TRUE)
rfm$rfm_tot <- rfm$recency_bin+rfm$frequency_bin+rfm$monetary_bin
rfm$rfm_bin <-ifelse(rfm$rfm_tot<=6,"high","low")


# Join demography data
df <- df[,c("egn","household_children","marital_status","age","gender","education",
            "ownership","total_income", "experience_employer","on_address")]
df <- df[!duplicated(df$egn),]
rfm <- merge(rfm, df, by.x = "egn", by.y = "egn", all.x = TRUE)


# Rearrange some fields
rfm$total_income <- ifelse(rfm$total_income>=10000, 10000, rfm$total_income)
rfm$rfm_bin <- as.factor(rfm$rfm_bin)


# Build decision tree model from "party" library

rfm$gender <- as.factor(rfm$gender)
rfm$ownership <- as.factor(rfm$ownership)
rfm$marital_status <- as.factor(rfm$marital_status)
rfm$education <- as.factor(rfm$education)



