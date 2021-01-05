

##################################################
#          CREDIRECT REPEAT - VERSION 3          #
#      PREPARE INPUT DATA FOR BUILDING MODEL     #
##################################################



######## Libraries and functions #########

# Call libraries 
library(dplyr)
library(RMySQL)

# Main dataframe
setwd("C:\\Projects\\Behavioral_Scoring\\Credirect_v3\\data\\")

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
con <- dbConnect(MySQL(), 
 user=db_user, password=db_password, dbname=db_name, host=db_host, 
 port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")


data_ckr_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, "
SELECT id, client_id, created_at
FROM citycash_db.clients_ckr_files")))
data_ckr <- fetch(data_ckr_sql, n=-1)

data_ckr_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, "
SELECT *
FROM citycash_db.clients_ckr_files_data")))
data_ckr2 <- fetch(data_ckr_sql2, n=-1)

data_sql3 <- suppressWarnings(dbSendQuery(con, 
"SELECT object_id AS application_id, amount, pay_date 
FROM citycash_db.cash_flow
WHERE nomenclature_id in (90,100,101) 
AND deleted_at IS NULL AND object_type=4"))
paids <- fetch(data_sql3, n=-1)
paids <- merge(paids, df[,c("id","credit_number","egn")], 
               by.x = "application_id", by.y = "id" , all.x = TRUE)

data_sql4 <- suppressMessages(suppressWarnings(dbSendQuery(con, "
SELECT application_id, is_revolved
FROM citycash_db.credits_applications_extras")))
data_4 <- fetch(data_sql4, n=-1)

data_sql5 <- suppressMessages(suppressWarnings(dbSendQuery(con, "
SELECT application_id, is_revolved
FROM citycash_db.credits_applications_extras")))
data_5 <- fetch(data_sql5, n=-1)



########## Set data fields ###########

ckr <- suppressWarnings(merge(data_ckr, data_ckr2, by.x = "id", 
                              by.y = "file_id", all.x = TRUE))

# Subset some more
df <- subset(df,df$status %in% c(4,5))
df <- subset(df, !(df$sub_status %in% c(129,122)) | is.na(df$sub_status))

# Get is revolved
df <- merge(df,data_4,by.x = "id",by.y = "application_id",all.x = TRUE)

# Calculate variables which depend on last credit
df <- df[with(df, order(egn)), ]

df$online <- ifelse(df$online_offline=="online",1,0)
df$offline <- ifelse(df$online_offline=="offline",1,0)
df$days_diff_last_credit <- NA
df$prev_credit <- NA
df$amount_prev <- NA
df$maturity_ratio <- NA
df$prev_online <- NA
df$offline_cum_int <- df$offline
df$online_cum_int <- df$online
df$credits_cum_int <- 0
df$refinance_cum_int <- 0
df$ratio_online_int <- 0
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]

for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]){
    
    df$online_cum_int[i] <- df$online_cum_int[i-1] + df$online[i]
    df$offline_cum_int[i] <- df$offline_cum_int[i-1] + df$offline[i]
    
    df$days_diff_last_credit[i] <- as.numeric(as.Date(df$date[i]) - 
         as.Date(df$deactivated_at[i-1],format="%Y-%m-%d"))
    df$prev_credit[i] <- df$credit_number[i-1]
    df$amount_prev[i] <- df$amount[i-1]
    df$maturity_ratio[i] <- df$maturity[i]/df$maturity[i-1]
    df$prev_online[i] <- ifelse(df$online[i-1]==1, 1, 0)
  }
  df$credits_cum_int[i] <- df$online_cum_int[i] + df$offline_cum_int[i]
  df$ratio_online_int[i] <-  df$online_cum_int[i] / (df$online_cum_int[i] +
                                                     df$offline_cum_int[i])
}

# Compute cumulative variables
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]

df$online_cum <- NA
df$offline_cum <- NA
df$credits_cum <- NA
df$ratio_online <- NA

for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {
    df$online_cum[i] <- df$online_cum_int[i-1]
    df$offline_cum[i] <- df$offline_cum_int[i-1]
    df$credits_cum[i] <- df$online_cum[i] + df$offline_cum[i]
    df$ratio_online[i] <- df$online_cum[i] / (df$online_cum[i] + 
                                              df$offline_cum[i])}
}

# Get flag of last payment to compute ratio of last payment
paids <- paids[with(paids, rev(order(pay_date))), ]
paids <- paids[with(paids, order(credit_number)), ]
paids1 <- paids[!duplicated(paids$credit_number),]
paids1 <- paids1[,c("credit_number","pay_date")]
paids2 <- merge(paids, paids1, by.x = c("credit_number","pay_date"), 
                by.y = c("credit_number","pay_date"))
paid_last_day <- aggregate(paids2$amount, 
                           by=list(paids2$credit_number), FUN=sum)
names(paid_last_day) <- c("credit_number","amount_paid_last_day")

paid_last_day <- merge(paid_last_day, df[,c("credit_number","total_amount")], 
                       by.x = "credit_number", 
                       by.y = "credit_number", 
                       all.x = TRUE)
paid_last_day$ratio_last_paid <- paid_last_day$amount_paid_last_day / 
  paid_last_day$total_amount
paid_last_day <- paid_last_day[,
      c("credit_number","amount_paid_last_day","ratio_last_paid")]
df <- merge(df, paid_last_day, by.x = "prev_credit", 
            by.y = "credit_number", all.x = TRUE)

# Calculate max previous delay days
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$max_delay <- 0
for (i in 3:nrow(df)) {
  if (df$egn[i]==df$egn[i-1] & df$egn[i]!=df$egn[i-2]) {
    df$max_delay[i]=df$days_delay[i-1]
  } 
  else if (df$egn[i]==df$egn[i-1] & df$egn[i]==df$egn[i-2]) {
    if (df$days_delay[i-1]<df$max_delay[i-1]) {
      df$max_delay[i] <- df$max_delay[i-1]} else {
        df$max_delay[i] <- df$days_delay[i-1]}
  } 
  else {df$max_delay[i]=NA} 
}

# Reorder
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]

# Compute other fields depending on last credit
df$days_diff_last_credit <- ifelse(df$days_diff_last_credit<0 | 
  is.na(df$days_diff_last_credit),0, df$days_diff_last_credit)
df$flag_high_last_paid <- ifelse(df$days_diff_last_credit %in% c(0,1,NA) & 
                                 df$ratio_last_paid>=0.5, 1, 0)
df$amount_diff <- df$amount - df$amount_prev

# Compute Refinance variables
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_previous <- 0
df$refinance_cum_int <- df$refinance_previous
df$revolved_cum_int <- df$is_revolved

for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {   
   df$refinance_previous[i] <- ifelse(abs(df$days_diff_last_credit[i])<=1,1,0)}
}


for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {df$refinance_cum_int[i] <- df$refinance_cum_int[i-1] + 
     df$refinance_previous[i]
   df$revolved_cum_int[i] <- df$revolved_cum_int[i-1] + 
     df$is_revolved[i]}
}
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {df$refinance_cum[i] <- df$refinance_cum_int[i-1]
   df$revolved_cum[i] <- df$revolved_cum_int[i-1]}
}

# Compute refinance ratio
df$refinance_ratio <- ifelse(df$credits_cum==0, NA,
                             df$refinance_cum /  df$credits_cum)

# Calculate what is first product
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
first_credit <- df[!duplicated(df$egn),]
first_credit <- first_credit[,c("egn","product_name")]
names(first_credit)[ncol(first_credit)] <- "first_credit"
df <- merge(df,first_credit,by.x="egn",by.y="egn",all.x=TRUE)

# Compute number of payments per installments
paids <- paids[with(paids, order(credit_number)), ]
paids3 <- paids[!duplicated(paids[c(3,4)]),]
paids3$id <- 1
count_payments <- aggregate(paids3$id,by=list(paids3$credit_number),FUN=sum)
names(count_payments) <- c("credit_number","nb_payments")
df <- merge(df,count_payments,by.x = "credit_number",
            by.y = "credit_number", all.x = TRUE)
df$nb_payments <- ifelse(is.na(df$nb_payments),0,df$nb_payments)
df$ratio_nb_payments <- df$nb_payments / df$installments
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$ratio_nb_payments_prev <- NA
df$flag_high_last_paid <- ifelse(is.na(df$flag_high_last_paid),0,
                                 df$flag_high_last_paid)
df$flag_high_last_paid_cum_int <- df$flag_high_last_paid
df$prev_credit_type <- NA
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {
    df$ratio_nb_payments_prev[i] <- df$ratio_nb_payments[i-1]
    df$prev_credit_type[i] <- df$product_name[i-1]
    df$flag_high_last_paid_cum_int[i] <- 
      df$flag_high_last_paid_cum_int[i-1] + df$flag_high_last_paid_cum[i]
  }
}

df$first_credit <- ifelse(substring(df$first_credit,1,5)!="CreDi","City Cash",
  ifelse(df$first_credit=="CreDirect Потребителски",
         "Installments","PayDay"))
df$prev_credit <- ifelse(substring(df$prev_credit_type,1,5)!="CreDi","City Cash",
  ifelse(df$prev_credit_type=="CreDirect Потребителски",
         "Installments","PayDay"))

# Make cumulative flag high last paid
df$flag_high_last_paid_cum <- 0
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {df$flag_high_last_paid_cum[i] <- df$flag_high_last_paid_cum_int[i-1]}
}
df$flag_high_last_paid_ratio <- df$flag_high_last_paid_cum  / 
  df$credits_cum

# Check data
View(df[,c("egn","id","date","deactivated_at","product_id","credit_number",
           "credits_cum","max_delay","days_diff_last_credit",
           "prev_online", "ratio_last_paid",
           "amount_diff","refinance_previous",
           "refinance_cum","maturity_ratio","refinance_ratio",
           "ratio_nb_payments_prev","ratio_online","first_credit",
           "prev_credit","flag_high_last_paid",
           "flag_high_last_paid_ratio"
           )])

# Make backup
df_bu <- df



########## Filter relevant data (time and type of credit)  ###########

# Filter credits
df <- subset(df,df$online_offline=="online")
df <- subset(df,df$credits_cum>0)

# Choose final date
df <- df[order(df$date),]
df$default_flag_cum <- 0
for(i in 2:nrow(df)){
  df$default_flag_cum[i] <- df$default_flag_cum[i-1] + df$default_flag[i]
}
df$nb_id <- seq(1:nrow(df))
df$ratio_default_flag_cum <- df$default_flag_cum / max(df$default_flag_cum)
plot(df$nb_id,df$ratio_default_flag_cum)
max(df$date[df$ratio_default_flag_cum<=0.97])

# Filter time window
df$difftime <- difftime(as.Date(Sys.time()), as.Date(df$max_pay_day), 
                        units=c("days"))
df <- subset(df, df$difftime>=90)
df <- subset(df, df$sub_status!=129 | is.na(df$sub_status))
df <- subset(df, df$date<"2020-01-17" & df$date>"2018-04-10")




########## Re-work some fields  ###########

# Correct_fields and filter wrong data
for (i in 1:nrow(df)){
  if(!(substring(df$egn[i],3,3) %in% c("5","4")) & (substring(df$egn[i],1,2) 
     %in% c("00","01","02","03","04","05","06"))){
    df$dob[i] <- NA
    df$age[i] <- 18 } else {
      df$dob_1[i] <- paste("20",substring(df$egn[i],1,2),"-",
           (as.numeric(substring(df$egn[i],3,3))-4),
           substring(df$egn[i],4,4),"-",substring(df$egn[i],5,6),sep="")
      df$dob_2[i] <- paste("19",substring(df$egn[i],1,2),"-",
           substring(df$egn[i],3,4),"-",substring(df$egn[i],5,6), sep="")
      df$dob_3[i] <- ifelse(as.character(substring(df$egn[i],1,2)) %in% 
           c("00","01","02","03","04","05","06"),
           paste("20",substring(df$egn[i],1,2),"-",
                (as.numeric(substring(df$egn[i],3,3))-4),
                 substring(df$egn[i],4,4),"-",substring(df$egn[i],5,6),
                 sep=""),
          paste("19",substring(df$egn[i],1,2),"-",
                 substring(df$egn[i],3,4),"-",substring(df$egn[i],5,6), 
                 sep=""))
  
    }
}
for (i in 1:nrow(df)){
  df$age[i] <- ifelse(is.na(df$dob_3[i]), 18, 
  floor(as.numeric(difftime(df$date[i], df$dob_3[i], units=c("days"))/365.242)))
}

df$age <- ifelse(df$age>90 | df$age<18,NA,df$age)
df$total_income <- as.numeric(df$total_income)
df$total_income <- ifelse(is.na(df$total_income),NA,
                          ifelse(df$total_income<100,NA,df$total_income))
df$ratio_installment_income <- as.numeric(df$ratio_installment_income)
df$ratio_installment_income <- ifelse(is.na(df$total_income),NA,
   ifelse(df$ratio_installment_income>4,NA,df$ratio_installment_income))

df$default_previous <- ifelse(is.na(df$max_delay),NA,df$default_previous)
df$refinance_previous <- ifelse(is.na(df$max_delay),NA,df$refinance_previous)
df$refinance_cum <- ifelse(is.na(df$max_delay),NA,df$refinance_cum)
df <- subset(df,!is.na(df$age))

# Correct some fields
df$month <- substring(df$date, 1, 7)
df$date_default <- as.Date(df$date_default, format="%Y-%m-%d")
df$amount_paid <- ifelse(is.na(df$amount_paid), 0, df$amount_paid)
df$flag_high_last_paid <- ifelse(is.na(df$flag_high_last_paid), 0, 
        df$flag_high_last_paid)

# Amount paid as percentage
df$profit <- df$amount_paid/df$amount

# Work on CKR
data_small <- df[,c("id","date", "client_id")]
names(data_small)[1] <- "application_id"

# CKR financial
ckr_financial <- subset(ckr, ckr$type==2)
data_ckr_fin <- merge(data_small, ckr_financial, by.x = "client_id", 
                      by.y = "client_id")
data_ckr_fin$diff_time_ckr <- abs(difftime(data_ckr_fin$date, 
  data_ckr_fin$created_at, units=c("days")))
data_ckr_fin <- data_ckr_fin[with(data_ckr_fin, order(diff_time_ckr)), ]
data_ckr_fin <- data_ckr_fin[with(data_ckr_fin, order(application_id)), ]
data_ckr_fin <- data_ckr_fin[,c("application_id","cred_count",
    "source_entity_count","amount_drawn","monthly_installment",
    "current_status_active","status_active","status_finished",
    "outstanding_performing_principal","outstanding_overdue_principal",
    "amount_cession","codebtor_status","guarantor_status")]
names(data_ckr_fin) <- c("application_id","cred_count_financial",
    "source_entity_count_financial","amount_drawn_financial",
    "monthly_installment_financial","current_status_active_financial",
    "status_active_financial","status_finished_financial",
    "outs_principal_financial","outs_overdue_financial",
    "cession_fin","codebtor_fin","guarantor_fin")
data_ckr_fin <- data_ckr_fin[!duplicated(data_ckr_fin$application_id),]

# CKR bank
ckr_bank <- subset(ckr, ckr$type==1)
data_ckr_bank <- merge(data_small, ckr_bank, by.x = "client_id", 
                                             by.y = "client_id")
data_ckr_bank$diff_time_ckr <- abs(difftime(data_ckr_bank$date, 
          data_ckr_bank$created_at, units=c("days")))
data_ckr_bank <- data_ckr_bank[with(data_ckr_bank, order(diff_time_ckr)), ]
data_ckr_bank <- data_ckr_bank[with(data_ckr_bank, order(application_id)), ]
data_ckr_bank <- data_ckr_bank[,c("application_id","cred_count",
        "source_entity_count","amount_drawn","monthly_installment",
        "current_status_active","status_active","status_finished",
        "outstanding_performing_principal","outstanding_overdue_principal",
        "amount_cession","codebtor_status","guarantor_status")]
names(data_ckr_bank) <- c("application_id","cred_count_bank",
        "source_entity_count_bank","amount_drawn_bank",
        "monthly_installment_bank","current_status_active_bank",
        "status_active_bank","status_finished_bank",
        "outs_principal_bank","outs_overdue_bank",
        "cession_bank","codebtor_bank","guarantor_bank")
data_ckr_bank <- data_ckr_bank[!duplicated(data_ckr_bank$application_id),]

# Merge CKR data
df <- merge(df, data_ckr_fin, by.x = "id", by.y = "application_id", 
            all.x = TRUE)
df <- merge(df, data_ckr_bank, by.x = "id", by.y = "application_id", 
            all.x = TRUE)

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
df$outs_overdue_ratio_total <- ifelse(df$outs_overdue_ratio_bank>
  df$outs_overdue_ratio_financial, 
   df$outs_overdue_ratio_bank, df$outs_overdue_ratio_financial)
df$cession_total <- df$cession_bank + df$cession_fin
df$codebtor_tot <- ifelse(df$codebtor_bank>df$codebtor_fin, 
   df$codebtor_bank,df$codebtor_fin)
df$guarantor_tot <- ifelse(df$guarantor_bank>df$guarantor_fin, 
   df$guarantor_bank,df$guarantor_fin) 

# Give name to selected products
df$product_name <- ifelse(df$product_id==1,"City Month",
   ifelse(df$product_id %in% c(5,11,23,24),"City Week",
   ifelse(df$product_id==6,"City 2-Week",
   ifelse(df$product_id==7,"Pensioner",
   ifelse(df$product_id==8,"VIP Week",
   ifelse(df$product_id %in% c(30,31,32,33,35),"0%",
   ifelse(df$product_id %in% c(55,58,57,56,25,36,41,43,50,
                               28,26,37,42,44,49,27),"Credirect PayDay",
   ifelse(df$product_id %in% c(9,48),"Credirect Installments","Other"))))))))

# Order by date
df <- df[order(df$date),]

# Delete certain unnecessary fields
df <- df[ , -which(names(df) %in% c("dob_1","dob_2","dob_3","nb_id",
  "flag_high_last_paid_cum_int","flag_high_last_paid_cum","default_flag_cum",
  "cred_count_financial","source_entity_count_financial",
  "amount_drawn_financial","monthly_installment_financial",
  "current_status_active_financial","status_active_financial",
  "status_finished_financial","outs_principal_financial",      
  "outs_overdue_financial","cession_fin","codebtor_fin",                   
  "guarantor_fin","cred_count_bank","source_entity_count_bank",       
  "amount_drawn_bank","monthly_installment_bank","current_status_active_bank",     
  "status_active_bank","status_finished_bank","outs_principal_bank",          
  "outs_overdue_bank","cession_bank","codebtor_bank" ,                 
  "guarantor_bank","outs_overdue_ratio_bank","outs_overdue_ratio_financial",
  "offline_cum_int","online_cum_int",
  "credits_cum_int","refinance_cum_int"
))]


######## Output data #########
setwd("C:\\Projects\\Behavioral_Scoring\\Credirect_v3\\data\\")
write.csv(df,"input_credirect.csv")


