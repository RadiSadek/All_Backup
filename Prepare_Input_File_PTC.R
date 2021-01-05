

#################################################
########    Prepare input file for      #########
########  Probability to Churn Model    #########
#################################################



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



############ Get data from Database ##############

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
"SELECT *
FROM citycash_db.clients_ckr_files_data")))
data_ckr2 <- fetch(data_ckr_sql2, n=-1)

data_ckr_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, client_id, created_at
FROM citycash_db.clients_ckr_files")))
data_ckr <- fetch(data_ckr_sql, n=-1)

data_ckr_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM citycash_db.clients_ckr_files_data")))
data_ckr2 <- fetch(data_ckr_sql2, n=-1)

ckr <- suppressWarnings(merge(data_ckr, data_ckr2, 
    by.x = "id", by.y = "file_id", all.x = TRUE))





######### Select relevant data ##############

# Leave only those with at least 1 credits
df$credit <- ifelse(df$status %in% c(4,5), 1, 0)
df$credit <- ifelse(df$status %in% c(129,133), 0, df$credit)
agg_egn <- aggregate(df$credit, by=list(df$egn), FUN=max)
names(agg_egn) <- c("egn","credits_tot")
agg_egn <- subset(agg_egn, agg_egn$credits_tot>0)
df <- df[df$egn %in% agg_egn$egn,]

# Leave only those who entered after 01/01/2016
df <- df[order(df$date_entry),]
df <- df[order(df$egn),]
first_credit <- df[!duplicated(df$egn),]
first_credit <- subset(first_credit, 
            first_credit$date_entry>="2016-01-01")
df <- df[df$egn %in% first_credit$egn,]

# Remove credits who have ever defaulted
ever_def <- as.data.frame(aggregate(df$days_delay, by=list(df$egn), 
            FUN=max, na.rm=TRUE))
names(ever_def) <- c("egn","max_days_delay")
ever_def$default_abs <- ifelse(ever_def$max_days_delay>=90, 1, 0)
df <- merge(df, ever_def[,c("egn","default_abs")], 
            by.x = "egn", by.y = "egn", all.x = TRUE)
df <- subset(df, df$default_abs==0)






######### Set CKR correctly #########

data_small <- df[,c("id","date", "client_id")]
names(data_small)[1] <- "application_id"

# CKR financial
ckr_financial <- subset(ckr, ckr$type==2)
data_ckr_fin <- merge(data_small, ckr_financial, by.x = "client_id", 
                      by.y = "client_id")
data_ckr_fin$diff_time_ckr <- abs(difftime(data_ckr_fin$date, 
                                           data_ckr_fin$created_at, 
                                           units=c("days")))
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
                                            data_ckr_bank$created_at, 
                                            units=c("days")))
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
                          df$codebtor_bank,
                          df$codebtor_fin)
df$guarantor_tot <- ifelse(df$guarantor_bank>df$guarantor_fin, 
                           df$guarantor_bank,
                           df$guarantor_fin) 




########## Set data variables ###########

# Subset some more
df <- subset(df,df$status %in% c(1,2,3,4,5))

# Calculate several useful fields
df$amount_paid <- ifelse(is.na(df$amount_paid), 0, df$amount_paid)
df$credits_cum <- ifelse(df$status %in% c(4,5), 1, 0)
df$max_delay <- ifelse(df$status %in% c(4,5), df$days_delay, -999)
df$activity_cum <- 1
df$recency <- NA
df$cum_rej <- ifelse(df$status==2, 1, 0)
df$recency_cum <- NA
df$total_amount_paid <- df$amount_paid
df$date_last <- ifelse(df$status %in% c(4,5), df$deactivated_at, df$date_entry)
df$avg_recency <- NA
df$avg_maturity <- df$maturity

df <- df[with(df, order(date_entry)),]
df <- df[with(df, order(egn)),]
for (i in 2:nrow(df)) {
  
  if (df$egn[i]==df$egn[i-1]){
    
    # Cumulative credits, rejects, amounts
    df$cum_rej[i] <- df$cum_rej[i] + df$cum_rej[i-1]
    df$credits_cum[i] <- df$credits_cum[i] + df$credits_cum[i-1]
    df$total_amount_paid[i] <- df$total_amount_paid[i] + 
      df$total_amount_paid[i-1]
    df$activity_cum[i] <- df$activity_cum[i-1] + 1
    df$recency[i] <- difftime(df$date_entry[i], 
      df$date_last[i-1], units=c("days"))
    df$recency[i] <- ifelse(df$status[i-1]==4, 0, 
       ifelse(df$recency[i]<0 & !is.na(df$recency[i]), 0 , 
      df$recency[i]))
    df$avg_recency[i] <- ifelse(is.na(df$avg_recency[i-1]),0
      ,df$avg_recency[i-1])+ifelse(is.na(df$recency[i]),0,df$recency[i])
    df$avg_maturity[i] <- df$maturity[i] + df$avg_maturity[i-1]
    
    # Demography
    df$education[i] <- ifelse(is.na(df$education[i]), df$education[i-1], 
      df$education[i])
    df$ownership[i] <- ifelse(is.na(df$ownership[i]), df$ownership[i-1], 
      df$ownership[i])
    df$household_children[i] <- ifelse(is.na(df$household_children[i]), 
       df$household_children[i-1], df$household_children[i])
    df$on_address[i] <- ifelse(is.na(df$on_address[i]), df$on_address[i-1], 
      df$on_address[i])
    df$experience_employer[i] <- ifelse(is.na(df$experience_employer[i]), 
      df$experience_employer[i-1],
      df$experience_employer[i])
    df$marital_status[i] <- ifelse(is.na(df$marital_status[i]), 
      df$marital_status[i-1], df$marital_status[i])
    df$purpose[i] <- ifelse(is.na(df$purpose[i]), 
      df$purpose[i-1], df$purpose[i])
    df$status_work[i] <- ifelse(is.na(df$status_work[i]), 
      df$status_work[i-1], df$status_work[i])
    df$source_entity_count_total[i] <- ifelse(
      is.na(df$source_entity_count_total[i]), 
      df$source_entity_count_total[i-1], df$source_entity_count_total[i])
    df$amount_drawn_total[i] <- ifelse(is.na(df$amount_drawn_total[i]), 
      df$amount_drawn_total[i-1], df$amount_drawn_total[i])
    df$monthly_installment_total[i] <- ifelse(
      is.na(df$monthly_installment_total[i]), 
      df$monthly_installment_total[i-1], df$monthly_installment_total[i])
    df$current_status_active_total[i] <- ifelse(
      is.na(df$current_status_active_total[i]), 
      df$current_status_active_total[i-1], df$current_status_active_total[i])
    df$status_active_total[i] <- ifelse(is.na(df$status_active_total[i]), 
      df$status_active_total[i-1], df$status_active_total[i])
    df$status_finished_total[i] <- ifelse(is.na(df$status_finished_total[i]), 
      df$status_finished_total[i-1], df$status_finished_total[i])
    df$cred_count_total[i] <- ifelse(is.na(df$cred_count_total[i]), 
      df$cred_count_total[i-1], df$cred_count_total[i])
    
    # Payment behavior
    if(df$max_delay[i]<df$max_delay[i-1]){df$max_delay[i] <- df$max_delay[i-1]}}
}

# Set and recorrect cetain fields
df$cred_per_app <- df$credits_cum/df$activity_cum
df$recency <- round(df$recency, 0)
df$avg_recency <- ifelse(is.na(df$avg_recency), NA, round(df$avg_recency/
    (df$activity_cum-1), 0))
df$avg_maturity <- round(df$avg_maturity/df$activity_cum, 1)
View(df[,c("egn","credit_number","status","amount_paid",
           "total_amount_paid","credits_cum")])

# Make backup
df_bu <- df 

# Get backup
df <- df_bu



########## Compute average period for churn assessment ###########

# Get average period for return of each repeat customer 
rep_cus <- subset(df, df$activity_cum>1)
rep_cus <- rep_cus[order(rep_cus$recency),]
rep_cus$id <- seq(1,nrow(rep_cus),1)
rep_cus$id <- round(rep_cus$id/nrow(rep_cus)*100,1)
plot(rep_cus$id,rep_cus$recency)
quantile(rep_cus$recency,0.92)
recency_max <- 180
###################### 92% return within 90 days , 10% later 
###################### -> 180 days is the period of churn/no churn criteria



########## Compute additional fields ###########

# Compute if churned next 
df <- df[with(df, order(date_entry)), ]
df <- df[with(df, order(egn)), ]
df$churn <- 1
for (i in 1:(nrow(df)-1)){
  if(df$egn[i+1]==df$egn[i] & df$recency[i+1]<=recency_max){
    df$churn[i] <- 0
  }
}
df$churn <- ifelse(df$status==4, 0, df$churn)

# Subset some more
output <- subset(df, df$credits_cum>=1 & df$status!=4)
output$date_diff <- difftime(Sys.time(), output$date_last, units=c("days"))
output <- subset(output, output$date_diff>180)

# Reordering
output <- output[with(output, order(date_entry)), ]
output <- output[with(output, order(egn)), ]

# Check result
View(output[,c("egn","credit_number","status","date_entry","date_last",
               "date_diff","churn","recency",
               "total_amount_paid","")])



########## Output ###########
setwd("C:\\Projects\\PTC\\data")
save(output, file = "input_data.rdata")


