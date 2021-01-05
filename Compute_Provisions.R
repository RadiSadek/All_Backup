

##################################################################
#           CALCULATE PREDICTED MONTHLY PROVISIONS               #
#        Compute Expected loss on all current actives            #
#                    Version 2.1 (2019/02/22)                    #
##################################################################



####################################
######## Manual settings   #########
####################################

# Choose date of provisions (time window end on previous day)
date_provisions <- "2021-04-01"

# Compute date for cession
date_prev_first_of_month <- "2021-01-01"



###########################################################
######## Additional once-in-a-lifetime settings   #########
###########################################################

# Set name for accounting file
accounting_file_name <- "Accounting_INPUT.xlsx"

# Set folder of input and output paths
main_dir <- "C:\\Projects\\Provisions_Modeled\\data"

# Set paths of RDATA models
dir_PD_buck <- "C:\\Projects\\PD_model\\models\\"
dir_LGD <- "C:\\Projects\\LGD_model\\models\\"

# Set paths for output charts
dir_charts <- paste(main_dir,"\\pics",sep="")

# Paths for previous first-of-month provision file
dir_prev_provisions <-  
  "C:\\Projects\\Provisions_Modeled\\data\\provisions\\"

# Database settings
# The db_name_backup has to refer to an older database, 
# i.e. before july 2019 because some older installments 
#were <unfortunately> deleted from citycash's real-time database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_name_backup <- "release"
db_host <- "127.0.0.1"
df_port <- 3306




#############################
######## Libraries  #########
#############################

# Call libraries 
library(RMySQL)
library(openxlsx)

# Define function
safe.ifelse <- function(cond, yes, no){ class.y <- class(yes)
X <- ifelse(cond, yes, no)
class(X) <- class.y; return(X)}



#####################################################
############ Make connection with SQL  ##############
#####################################################

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user,  password = db_password,
                 dbname=db_name, host=db_host, port = df_port)



#################
# Read SQL data #
#################

plan <- dbSendQuery(con, 'set character set "utf8"')
plan <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, installment_num, pay_day,
days_delay, payed_at , pmt_final 
FROM ",db_name,".credits_plan_main", sep="")), n=-1))
names(plan)[3:4] <- c("pay_day_det","days_delay_curr")
plan_raw <- plan

cash <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT object_id, pay_date, nomenclature_id, 
amount, object_type, type, deleted_at 
FROM ",db_name,".cash_flow", sep="")), n=-1))

clients <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, egn 
FROM ",db_name,".credits_applications_clients", sep="")), n=-1))

clients_info <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, 
ownership, education, household_children, on_address, 
marital_status
FROM ",db_name,".credits_applications_clients", sep="")), n=-1))

contract <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, amount, installments
FROM ",db_name,".credits_plan_contract", sep="")), n=-1))

products <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT id, period
FROM ",db_name,".products", sep="")), n=-1))

money_client <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, SUM(amount) AS income 
FROM ",db_name,".credits_applications_clients_money_income 
WHERE type=1 AND sub_type=1 AND deleted_at IS NULL
GROUP BY application_id", sep="")), n=-1))

money_client_add <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, 
SUM(amount) AS additional_income 
FROM ",db_name,".credits_applications_clients_money_income 
WHERE deleted_at IS NULL AND type<>1 OR sub_type<>1
GROUP BY application_id", sep="")), n=-1))

products_det <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT product_id, period, amount, installment_amount
FROM ",db_name,".products_periods_and_amounts", sep="")), n=-1))

applications <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT credit_number, id, status, signed_at, product_id, 
office_id, deactivated_at, sub_status, third_side_date
FROM ",db_name,".credits_applications", sep="")), n=-1))

exp_emp <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, experience_employer
FROM ",db_name,".credits_applications_clients_work", sep="")), n=-1))

data_other <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, purpose
FROM ",db_name,".credits_applications_data_other", sep="")),n=-1))

total_amount <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, final_credit_amount as total_amount
FROM ",db_name,".credits_plan_contract", sep="")), n=-1))

offices_id <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT id, name
FROM ",db_name,".structure_offices", sep="")), n=-1))
Encoding(offices_id$name) <- "UTF-8"

first_office <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT 
",db_name,".credits_applications.credit_number,
",db_name,".credits_applications.date,
",db_name,".credits_applications_transfers.old_office_id,
",db_name,".credits_applications_transfers.created_at,
",db_name,".structure_offices.name
FROM ",db_name,".credits_applications
INNER JOIN ",db_name,".credits_applications_transfers ON 
",db_name,".credits_applications.id = ",db_name,
".credits_applications_transfers.application_id
INNER JOIN ",db_name,".structure_offices ON 
",db_name,".credits_applications_transfers.old_office_id = ",
db_name,".structure_offices.id", sep="")), n=-1))

coorp_secu <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT credit_number 
FROM ",db_name,".credits_applications 
WHERE judicial_date is not null OR is_corporative_security=1", 
sep="")), n=-1))

coorp_secu <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT credit_number 
FROM ",db_name,".credits_applications 
WHERE judicial_date is not null OR is_corporative_security=1", 
sep="")), n=-1))

taxes <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, created_at
FROM ",db_name,".credits_plan_taxes 
WHERE tax_id IN (3,21)", sep="")), n=-1))
names(taxes) <- c("application_id","tax_20pct_created")
taxes <- taxes[order(taxes$tax_20pct_created),]
taxes <- taxes[order(taxes$application_id),]
taxes <- taxes[!duplicated(taxes$application_id),]




###########################
# Make one big dataframe  #
###########################

# Month of provisions
month_prov <- substring(as.Date(date_provisions)-27,1,7)

# Subset applications
applications <- subset(applications, applications$status %in% c(4,5))

# EGN
df <- merge(applications, clients, by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Get tax date
df <- merge(df, taxes, by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Refinance, cession, deactivated_at
df$refinance <- ifelse(df$sub_status==126, 1 ,0)
df$cession <- ifelse(df$sub_status==124, 1 ,0)
df$deactivated_at <- as.Date(df$deactivated_at, 
                             format="%Y-%m-%d")

# Maximum DPD
max_dpd <- aggregate(plan$days_delay, 
                     by=list(plan$application_id), FUN=max)
names(max_dpd) <- c("id","days_delay")
df <- merge(df, max_dpd, by.x = "id", by.y = "id", all.x = TRUE)
df$default_flag <- ifelse(df$days_delay>=90, 1, 0)
rm(max_dpd)

# Signed at
df$date <- as.Date(df$signed_at, format="%Y-%m-%d")

# Date of default (current)
date_def <- plan[,c("application_id", "installment_num", 
                    "pay_day_det", "days_delay_curr", "payed_at")]
date_def$paid <- ifelse(is.na(date_def$payed_at), 0, 1)
date_def <- subset(date_def, 
                   date_def$days_delay_curr>=90 & is.na(date_def$payed_at))
date_def <- date_def[!duplicated(date_def$application_id),]
date_def$date_default <- as.Date(date_def$pay_day_det) + 90
df <- merge(df, date_def[,c(1,7)], by.x = "id", 
            by.y = "application_id", all.x = TRUE)
rm(date_def)

# First day of padej
plan_work <- plan[,c("application_id", "pay_day_det")]
plan_work <- merge(plan_work, total_amount, 
                   by.x = "application_id", by.y="application_id", all.x=TRUE)
plan_work <- plan_work[order(plan_work$application_id, 
                             plan_work$pay_day_det),]
plan_work <- plan_work[!duplicated(plan_work$application_id),]
first_day <- plan_work[,c("application_id","pay_day_det")]
names(first_day)[2] <- "pay_day"
df <- merge(df, first_day, by.x = "id",
            by.y = "application_id", all.x = TRUE)
total_amount <- plan_work[,c("application_id","total_amount")]
df <- merge(df, total_amount , by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Remove unnessary data frames
rm(total_amount, plan_work, first_day)

# Amount and installments
df <- merge(df, contract , by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Period
df <- merge(df, products, by.x = "product_id", 
            by.y = "id", all.x = TRUE)

# Installment amount
df$installment_amount <- NA
df$product_id <- ifelse(df$product_id %in% c(2,3,4,10), 1, 
                        ifelse(df$product_id %in% c(11,23,24), 5, 
                               df$product_id))
for (i in 1:nrow(df)){
  products_det_subs <- subset(products_det, 
                              products_det$product_id==df$product_id[i])
  subs_amount <- products_det_subs[
    products_det_subs$product_id==df$product_id[i] & 
      products_det_subs$amount== products_det_subs$amount[
        which.min(abs(products_det_subs$amount - df$amount[i]))],]
  df$installment_amount[i] <- subs_amount$installment_amount[
    subs_amount$period==subs_amount$period[
      which.min(abs(subs_amount$period - df$installments[i]))]]
}

# Work to get first office (if changed)
Encoding(first_office$name) <- "UTF-8"
first_office <- first_office[order(first_office$credit_number, 
                                   first_office$created_at),]
first_office <- first_office[!duplicated(
  first_office$credit_number),]
first_office <- first_office[,c("credit_number","name")]
names(first_office)[2] <- "first_office"

# Get clients info
df <- merge(df, clients_info, by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Gender and age
df$gender <- ifelse(substring(df$egn,9,9) %in% c(0,2,4,6,8), 1, 0)
df$age <- ifelse(as.character(substring(df$egn,1,2)) %in% 
                   c("00","01","02","03","04","05"),
                 as.numeric(substring(df$date,1,4))-
                   as.numeric(paste("20",as.character(substring(df$egn,1,2)), 
                                    sep = "")), 
                 ifelse(as.character(substring(df$egn,1,2)) %in% 
                          c("10","11"), NA,
                        as.numeric(substring(df$date,1,4))-
                          as.numeric(paste("19",
                       as.character(substring(df$egn,1,2)), sep = ""))))

# Experience Employer
df <- merge(df, exp_emp, by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Purpose
df <- merge(df, data_other, by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Total income
df <- merge(df, money_client, by.x = "id" , 
            by.y = "application_id", all.x = TRUE)
df <- merge(df, money_client_add, by.x = "id" , 
            by.y = "application_id", all.x = TRUE)
df$income <- ifelse(is.na(df$income), 0, df$income)
df$additional_income <- ifelse(is.na(df$additional_income), 0 , 
                               df$additional_income)
df$total_income <- df$income + df$additional_income
df$ratio_installment_income <- ifelse(df$period==1, 
               df$installment_amount/(df$total_income*7/30),
               ifelse(df$period==2, df$installment_amount/
               (df$total_income*14/30),
               df$installment_amount/(df$total_income)))

# Paid per client and last paid date
cash <- subset(cash, cash$pay_date < date_provisions)
paid_client <- cash
paid_client <- subset(paid_client,
                      paid_client$nomenclature_id %in% c(90,100,101) &
                        is.na(paid_client$deleted_at))
paid_client <- paid_client[,c("object_id","amount","pay_date")]
paid_client <- merge(paid_client, df[,
                                     c("id","egn","credit_number")], 
                     by.x = "object_id", by.y = "id", all.x = TRUE)
paid_client <- paid_client[order(paid_client$object_id,
                                 paid_client$pay_date),]
paids <- paid_client
paid_client <- paid_client[order(
  paid_client$object_id),]
paid_client <- paid_client[rev(order(as.Date(
  paid_client$pay_date))),]
paid_client <- paid_client[order(paid_client$object_id),]
paid_client <- paid_client[!duplicated(
  paid_client$object_id),]
paid_client_amount <- paid_client[,c("object_id","amount")]
names(paid_client_amount)[2] <- "last_amount_paid"
df <- merge(df, paid_client_amount, by.x = "id", 
            by.y = "object_id", all.x = TRUE)
paid_client <- paid_client[,c("object_id","pay_date")]
names(paid_client)[2] <- "last_pay_date"
df <- merge(df, paid_client , by.x = "id", 
            by.y = "object_id", all.x = TRUE)
rm(paid_client)

# Offices
df <- merge(df, offices_id, by.x = "office_id" , 
            by.y = "id", all.x = TRUE)
names(df)[ncol(df)] <- "office_current"



#########################################################################
########## Read credits on which to evaluate credit provision ###########
#########################################################################

# Main dataframe
setwd(main_dir)

# Subset active and finished credits
df <- subset(df,df$status %in% c(4,5))

# Filter empty egn
df <- subset(df, !(df$egn=="" | is.na(df$egn)))

# Correct date default
df$date_default <- as.Date(df$date_default, format="%Y-%m-%d")

# Read credits to score
credits <- read.xlsx(accounting_file_name)

# Remove office "Finstart" which is a separate company
credits <- subset(credits, !credits$Офис=="Финстарт")

# Rework on credits to score
credits <- as.data.frame(credits)
credits <- credits[,c(1,2,32,31)]
names(credits) <- 
  c("credit_number","date_active","DPD", "saldo")
credits <- credits[,
                   c("credit_number","date_active","DPD")]
credits <- subset(credits, !(is.na(credits$DPD)))
credits$date_active <- as.Date(paste(substring(
  credits$date_active,7,10),"-",
  substring(credits$date_active,4,5),"-",
  substring(credits$date_active,1,2), sep = ""))
credits <- credits[order(rev(credits$date_active)), ]

# Set current date and filter
current_date <- as.Date(date_provisions)
paids <- subset(paids, paids$pay_date<date_provisions)
df <- subset(df, df$date<date_provisions)
df$date_default <- safe.ifelse(df$date_default>=date_provisions, 
                           as.Date(NA), df$date_default)


# Make back-up of payment behavior
paids_raw <- paids



###############################################
########## Leave only relevant EGNs ###########
###############################################

# Get relevant credits
credits_nb <- as.data.frame(credits[,c(1)])
names(credits_nb)[1] <- "credit_number"
credits_nb_abs <- credits_nb

# Get EGN of credits 
credits_nb <- merge(df, credits_nb, by.x = "credit_number", 
                    by.y = "credit_number", all.y = TRUE)
credits_nb <- credits_nb[,c("credit_number","egn")]
credits_nb <- credits_nb[!duplicated(credits_nb$egn),]
credits_nb <- as.data.frame(credits_nb[,c("egn")]) 
names(credits_nb)[1] <- "egn"

# Leave only data for relevant EGNs
df <- merge(df, credits_nb, by.x = "egn", 
            by.y = "egn", all.y = TRUE)

# Work on plan also
credits_appid <- merge(df, credits_nb_abs, 
                       by.x = "credit_number", 
                       by.y = "credit_number", all.y = TRUE) 
credits_appid <- credits_appid[,c("id","credit_number")]
plan <- merge(plan, credits_appid, 
              by.x = "application_id", by.y = "id", 
              all.y = TRUE)
plan <- plan[ , -which(names(plan) %in% c("credit_number"))]



####################################################
### Build data for applying and building model   ###
####################################################

########## Calculate EAD and RR at default ###########

# Compute amount paid before and after default (in a 1-year window span)
df_def_date <- df[,c("id","date_default")]
paids <- merge(paids , df_def_date, 
               by.x="object_id", by.y="id", TRUE)
paids$date_default_plus1year <- as.Date(date_provisions)
paids$amount_paid_after_def <- ifelse(
  as.Date(paids$pay_date)<=
    as.Date(paids$date_default_plus1year) & 
    as.Date(paids$pay_date)>=
    as.Date((as.Date(paids$date_default))),paids$amount,0)
paids$amount_paid_before_def <- ifelse(
  as.Date(paids$pay_date)<
    as.Date((as.Date(paids$date_default))),
  paids$amount,0)
paids_agg <- aggregate(paids$amount_paid_after_def, 
                       by=list(id=paids$object_id), FUN=sum)
names(paids_agg)[ncol(paids_agg)] <- "amount_paid_after_def"
df <- merge(df , paids_agg, 
            by.x="id", 
            by.y="id", TRUE, all.x=TRUE)

df$amount_paid_after_def <- ifelse(is.na(
  df$amount_paid_after_def),0,
  df$amount_paid_after_def)
paids_agg_bef <- aggregate(paids$amount_paid_before_def, 
                           by=list(id=paids$object_id), FUN=sum)
names(paids_agg_bef)[ncol(paids_agg_bef)] <- "amount_paid_before_def"
df <- merge(df , paids_agg_bef, by.x="id", 
            by.y="id", TRUE, all.x=TRUE)
df$amount_paid_before_def <- ifelse(is.na(
  df$amount_paid_before_def),0,df$amount_paid_before_def)

# Compute Exposure at Default 
df$EAD <- df$total_amount - df$amount_paid_before_def

# Compute number and ratio of number of installments
df$ratio_passed_installments <-  (floor(
  ifelse(df$period==3,(as.Date(df$date_default) -90 - 
                         as.Date(df$pay_day))/30,
         ifelse(df$period==2,(as.Date(df$date_default) -90 - 
                                as.Date(df$pay_day))/14,
                (as.Date(df$date_default) -90  - 
                   as.Date(df$pay_day))/7)))) / df$installments

# Ratio last amount paid
df$ratio_last_amount_paid <- df$last_amount_paid / 
  df$installment_amount

# Calculate number of previous online, offline credits, 
# date difference and number of previous defaulted credits
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]

# Filter empty egn
df <- subset(df, !(df$egn=="" | is.na(df$egn)))

# Compute total credits and days of difference of last credit
df$credits <- 1
df$credits_cum <-0
df$default_cum <-0
df$credits_cum <-0
df$refinance_cum <-0
df$cession_cum <- 0
df$days_diff_last_credit <- NA

for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1] & df$status[i-1]==5) 
  {df$credits_cum[i] <- df$credits[i]+df$credits_cum[i-1]
  }
  else if (df$egn[i]==df$egn[i-1]) {
    df$credits_cum[i] <- df$credits_cum[i-1]
  }
  if (df$egn[i]==df$egn[i-1]){
    df$default_cum[i] <- 
      df$default_flag[i-1]+df$default_cum[i-1]
    df$days_diff_last_credit[i] <- as.numeric(
      as.Date(df$date[i]) - 
        as.Date(df$deactivated_at[i-1],format="%Y-%m-%d"))
    df$cession_cum[i] <- df$cession_cum[i-1] + 
      df$cession[i-1]
  }}

# Flag if previous is refinance or not 
df$refinance_previous <- 0
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_previous <- ifelse(abs(
  df$days_diff_last_credit)<=1,1,0)
df$refinance_previous <- ifelse(is.na(
  df$refinance_previous), 0, df$refinance_previous)

# Cumulative refinance
df$refinance_cum <- 0
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {
    df$refinance_cum[i] <- df$refinance_previous[i]+
      df$refinance_cum[i-1]
  }}

# Calculate max previous delay days
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$max_delay <- 0
for (i in 3:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1] & df$egn[i]!=df$egn[i-2]) {
    df$max_delay[i] <- df$days_delay[i-1]
  } 
  else if (df$egn[i]==df$egn[i-1] & 
           df$egn[i]==df$egn[i-2]) {
    if (df$days_delay[i-1]<df$max_delay[i-1]) {
      df$max_delay[i] <- df$max_delay[i-1]} else {
        df$max_delay[i] <- df$days_delay[i-1]}
  } else {
    df$max_delay[i]=NA} 
}

# Calculate if user has previous default credit (boolean) 
df$default_previous <- ifelse(df$default_cum>0,1,0)
df$cession_previous <- ifelse(df$cession_cum>0,1,0)

# Calculate paid time during default time
df$paid_time30 <- df$amount_paid_after_def / df$EAD
df$paid_time60 <- df$paid_time30
df$paid_time90 <- df$paid_time30
df$paid_time120 <- df$paid_time30
df$paid_time150 <- df$paid_time30
df$paid_time180 <- df$paid_time30
df$paid_time240 <- df$paid_time30
df$paid_time300 <- df$paid_time30

# Apply new time difference up to now
paids_last <- paids[rev(order(paids$object_id,
                              paids$pay_date)),]
paids_last <- paids_last[!duplicated(paids_last$object_id),]
names(paids_last)[3] <- "last_pay_date"
paids_last <- paids_last[,
                         c("object_id","last_pay_date")]
df <- df[ , -which(names(df) %in% c("last_pay_date"))]
df <- merge(df, paids_last, 
            by.x = "id", 
            by.y = "object_id", all.x = TRUE)
df$current_time_diff <-  difftime(
  date_provisions, df$last_pay_date, units="days")

# Remove duplicates by credit_number 
df <- df[!duplicated(df$credit_number), ]

# Remerge to only credits of interest
df <- merge(df, credits_nb_abs, 
            by.x = "credit_number", by.y = "credit_number", 
            all.y = TRUE)



#############################################
########## Work on payment behavior #########
#############################################

# Correct date default
plan$pay_day_det <- as.Date(plan$pay_day_det, 
                            format="%Y-%m-%d")

# Get detailed delays
df_cred <- df[,c("credit_number","id")]
plan <- subset(plan,!(is.na(plan$application_id)),)
plan  <- merge(df_cred, plan, by.x = "id", 
               by.y = "application_id", all.y = TRUE)

# Work on credits plan main (with days of delay)
plan <- plan[order(plan$credit_number,plan$pay_day_det),]
plan_nb_installments <- as.data.frame(
  aggregate(plan$installment_num, 
            by=list(credit_number=plan$credit_number),
            FUN=max))
names(plan_nb_installments)[ncol(plan_nb_installments)] <- 
  "nb_installments"
plan <- merge(plan, plan_nb_installments, 
              by.x = "credit_number", 
              by.y = "credit_number",
              all.x = TRUE)
rm(plan_nb_installments)

# Filter plan
plan <- subset(plan, plan$pay_day_det<
                 as.Date(date_provisions))

# Order by credit number and pay date
plan <- subset(plan, !is.na(plan$credit_number))
plan <- plan[order(plan$credit_number,plan$pay_day_det),]

# Create variables max DPD and ratio passed installments
plan$ratio_passed_installments <- plan$installment_num / 
  plan$nb_installments
plan <- plan[order(plan$credit_number,
                   plan$installment_num),]
plan$max_DPD_mov <- NA
plan$max_DPD_mov[1] <- plan$days_delay_curr[1]
plan$max_DPD_upto <- NA
plan$max_DPD_upto[2] <- plan$max_DPD_mov[1]
for (i in 2:nrow(plan)){
  if (plan$credit_number[i]==plan$credit_number[i-1]){
    if(plan$days_delay_curr[i]<plan$max_DPD_mov[i-1]){
      plan$max_DPD_mov[i] <- plan$max_DPD_mov[i-1]
    } else {
      plan$max_DPD_mov[i] <- plan$days_delay_curr[i]
    }
    plan$max_DPD_upto[i] <- plan$max_DPD_mov[i-1]
  } else {
    plan$max_DPD_mov[i] <- plan$days_delay_curr[i]
    plan$max_DPD_upto[i] <- NA
  }}


# Reorder conveniently
plan <- plan[order(plan$credit_number,
                   plan$installment_num),]

# Apply date format
plan$pay_day_det <- as.Date(plan$pay_day_det)

# Get last payment and number of payments per number of installments
plan_paid <- plan[,c("credit_number","installment_num")]
plan_paid <- merge(plan_paid, paids, 
                   by.x = "credit_number", 
                   by.y = "credit_number", 
                   all.x = TRUE)
plan_paid$last_payment <- difftime(date_provisions, 
                                   plan_paid$pay_date, units = c("days"))
plan_paid <- subset(plan_paid, plan_paid$last_payment>1)
plan_paid$count <- 1
agg <- aggregate(plan_paid$count, 
                 by=list(credit_number=plan_paid$credit_number,
                         installment_num=plan_paid$installment_num), FUN=sum)
agg$x <- agg$x / (agg$installment_num)
names(agg)[ncol(agg)] <- "payments_per_installments"
plan_paid <- merge(plan_paid, agg, 
                   by.x = c("credit_number","installment_num"),
                   by.y = c("credit_number","installment_num"), all.x = TRUE)
plan_paid <- plan_paid[order(plan_paid$credit_number, 
                             plan_paid$installment_num, 
                             plan_paid$last_payment),]
plan_paid <- plan_paid[!duplicated(plan_paid[c(1,2)]),]
plan_paid <- plan_paid[,
                       c("credit_number","installment_num",
                         "last_payment","payments_per_installments")]
plan_paid$last_payment <- 
  as.numeric(round(plan_paid$last_payment, 1))
plan_paid$payments_per_installments <- 
  as.numeric(round(plan_paid$payments_per_installments, 2))

# Remerge with main plan
plan <- merge(plan, plan_paid, 
              by.x = c("credit_number","installment_num"), 
              by.y = c("credit_number","installment_num"), 
              all.x = TRUE)

# Choose relative columns and output
plan$current_date <- current_date
plan$diff_time <- difftime(plan$current_date, 
                           plan$pay_day_det, units="days")
plan <- plan[order(plan$credit_number, plan$diff_time), ]
plan <- plan[!duplicated(plan$credit_number),]
plan <- plan[,c("credit_number","ratio_passed_installments",
                "max_DPD_upto","last_payment","installment_num",
                "payments_per_installments")]
names(plan)[2] <- "ratio_passed_installments_curr" 

# Remerge payment behavior to main dataframe
df <- merge(df, plan, 
            by.x = "credit_number", 
            by.y = "credit_number", all.x = TRUE)



##############################################################
# Redefine some parameters and load Rdatas for LGD and PD    #		
##############################################################

# Set output name		
output_name <-  paste("Provisions_", 
                      date_provisions,".xlsx", sep="")	

# Define constants		
days_def <- 90

# Define work directory
setwd(main_dir)

# Rejoin first office
df <- merge(df, first_office, 
            by.x = "credit_number", 
            by.y = "credit_number", all.x = TRUE)

# Load data to score
rm(credits)
credits <- read.xlsx(accounting_file_name)
credits <- as.data.frame(credits)
credits <- credits[,c(1,31,32,36,5,9,10,27,28,29,30,2)]
names(credits) <- c("credit_number","Saldo", 
                    "current_days_delay","collateral","office",
                    "product_name","credit_status",
                    "taksi","neustoiki","lihvi","glavnitsi","activated_at")
credits$Saldo <- ifelse(is.na(credits$Saldo), 0, credits$Saldo)

# Rearrange collateral
credits$collateral <- ifelse(is.na(credits$collateral), 
                             0, credits$collateral)

# Load Decision Tree Models for Loss Given Default 
#(for each time and return rate bucket)
ranges <- c(0,30,60,90,120,150,180,240,300)
ranges_pct <- c(0,5,10,20,30,40,50,60,70,80,90,100)
for (j in 1:length(ranges_pct)){
  setwd(paste(dir_LGD, ranges_pct[j], 'pct\\', sep=""))
  fname <- paste('Decision_Trees_',ranges,'_', 
                 ranges_pct[j],'pct.rdata', sep="")
  lapply(fname,load,.GlobalEnv)
  for (i in 1:length(ranges)){
    assign(paste0("dt",ranges[i], '_', ranges_pct[j], 'pct', sep=""), 
           get(noquote(paste("decision_tree_model_", ranges[i], sep=""))), 
           envir = .GlobalEnv)
  }
}
rm(list=ls(pattern="^decision_tree_model"))

# Load Decision Tree Models for Probability of Default (buckets on DPD)
for (i in 1:11){
  load(paste(dir_PD_buck,'PD_Bucket',i,'.rdata', sep=""))
}

# Select credits to calculate provisions
df <- merge(df , credits, 
            by.x="credit_number", 
            by.y="credit_number", TRUE, 
            all.x=TRUE)

# Take current days of delay
df$days_delay <- df$current_days_delay

# Rearrange some other fields
df$ratio_installment_income <- 
  as.numeric(df$ratio_installment_income)
df$age <- ifelse(df$age>90 | df$age<18, NA, df$age)

# Make back-up to use later for LGD
df_backup <- df

# Separate into several dataframe depending on days delay
df$difftime_rep <- difftime(as.Date(date_provisions), 
                            df$pay_day, units=c("days"))
df_notdef <- subset(df, df$days_delay<90)
df_notfirst <- subset(df_notdef, df_notdef$difftime_rep>1)
df_first <- subset(df_notdef, df_notdef$difftime_rep<=1)
df_def <- subset(df, df$days_delay>=90)



###########################################################
# Compute with Decision Trees for not first installments  #
###########################################################

# Treat max delay and ration passed installments
df_notfirst <- df_notfirst[ , 
   -which(names(df) %in% c("ratio_passed_installments"))]
colnames(df_notfirst)[colnames(df_notfirst)==
   "ratio_passed_installments_curr"] <- "ratio_passed_installments"
df_notfirst$ratio_passed_installments_curr <- NA 
df_notfirst$max_delay <- ifelse(is.na(
  df_notfirst$max_delay), -999, df_notfirst$max_delay)

# Convert to integers or doubles
df_notfirst$max_DPD_upto <- 
  as.integer(df_notfirst$max_DPD_upto)
df_notfirst$ratio_passed_installments <- 
  as.numeric(df_notfirst$ratio_passed_installments)
df_notfirst$last_payment <- 
  as.numeric(df_notfirst$last_payment)
df_notfirst$payments_per_installments <- 
  as.numeric(df_notfirst$payments_per_installments)
df_notfirst$ownership <- 
  as.integer(df_notfirst$ownership)
df_notfirst$education <- 
  as.integer(df_notfirst$education)
df_notfirst$gender <- 
  as.integer(df_notfirst$gender)
df_notfirst$household_children <- 
  as.integer(df_notfirst$household_children)
df_notfirst$on_address <- 
  as.integer(df_notfirst$on_address)
df_notfirst$experience_employer <- 
  as.integer(df_notfirst$experience_employer)
df_notfirst$marital_status <- 
  as.integer(df_notfirst$marital_status)
df_notfirst$max_delay <- 
  as.numeric(df_notfirst$max_delay)
df_notfirst$credits_cum <- 
  as.integer(df_notfirst$credits_cum)
df_notfirst$days_diff_last_credit <- 
  as.integer(df_notfirst$days_diff_last_credit)
df_notfirst$age <- 
  as.integer(df_notfirst$age)

# Make factor some variabeles
df_notfirst$purpose <- as.factor(df_notfirst$purpose)
df_notfirst$education <- as.factor(df_notfirst$education)
df_notfirst$ownership <- as.factor(df_notfirst$ownership)
df_notfirst$marital_status <- as.factor(df_notfirst$ownership)

# Apply Decision Tree models
df_notfirst$PD <- ifelse(df_notfirst$days_delay==0,
    predict(pd_dt_bucket1, df_notfirst),
    ifelse(df_notfirst$days_delay>=1 & df_notfirst$days_delay<=5,
    predict(pd_dt_bucket2, df_notfirst),
    ifelse(df_notfirst$days_delay>=6 & df_notfirst$days_delay<=9,
    predict(pd_dt_bucket3, df_notfirst),
    ifelse(df_notfirst$days_delay>=10 & df_notfirst$days_delay<=19,
    predict(pd_dt_bucket4, df_notfirst),
    ifelse(df_notfirst$days_delay>=20 & df_notfirst$days_delay<=29,
    predict(pd_dt_bucket5, df_notfirst),
    ifelse(df_notfirst$days_delay>=30 & df_notfirst$days_delay<=39,
    predict(pd_dt_bucket6, df_notfirst),
    ifelse(df_notfirst$days_delay>=40 & df_notfirst$days_delay<=49,
    predict(pd_dt_bucket7, df_notfirst),
    ifelse(df_notfirst$days_delay>=50 & df_notfirst$days_delay<=59,
    predict(pd_dt_bucket8, df_notfirst),
    ifelse(df_notfirst$days_delay>=60 & df_notfirst$days_delay<=69,
    predict(pd_dt_bucket9, df_notfirst),
    ifelse(df_notfirst$days_delay>=70 & df_notfirst$days_delay<=79,
    predict(pd_dt_bucket10, df_notfirst),
    predict(pd_dt_bucket11, df_notfirst)))))))))))



################################################################
# Assume a constant PD for first installments and for defaults #
################################################################
df_first$PD <-  mean(df_notfirst$PD)
df_def$PD <- 1



#######################################
# Rebind all PDs from all dataframes
#######################################
PD <- rbind(df_first, df_notfirst, df_def)
PD <- PD[,c("credit_number", "PD")]



#######################################
# Compute LGD (Loss Given Default)    #
#######################################

# Get back-uped dataframe
df <- df_backup

# Create fictive CKR and maturity (not used but required)
df$ckr_status <- 402
df$maturity <- 4

# Convert to integers
df$age <- as.integer(df$age)
df$education <- as.integer(df$education)
df$experience_employer <- 
  as.integer(df$experience_employer)
df$gender <- as.integer(df$gender)
df$period <- as.integer(df$period)
df$purpose <- as.integer(df$purpose)
df$credits_cum <- as.integer(df$credits_cum)
df$ownership <- as.integer(df$ownership)
df$max_delay <- as.integer(df$max_delay)
df$marital_status <- as.integer(df$marital_status)
df$on_address <- as.integer(df$on_address)
df$ckr_status <- as.integer(df$ckr_status)
df$household_children <- 
  as.integer(df$household_children)
df$refinance_previous <- 
  as.integer(df$refinance_previous)
df$default_previous <- 
  as.integer(df$default_previous)
df$default_cum <- 
  as.integer(df$default_cum)
df$refinance_cum <- 
  as.integer(df$refinance_cum)

# Convert to doubles
df$ratio_installment_income <- 
  as.numeric(df$ratio_installment_income)
df$days_diff_last_credit <- 
  as.numeric(df$days_diff_last_credit)
df$ratio_passed_installments <- 
  as.numeric(df$ratio_passed_installments)
df$maturity <- as.numeric(df$maturity)

# Bin days of delay
df$days_delay_bin <- cut(df$days_delay,
                         c(0,90,90+60,90+90,120+90,150+90,180+90,
                           240+90,300+90,100000))
options(scipen=999)

# Set correct EADs for discretized LGD buckets
df$EAD30 <- df$EAD
df$EAD60 <- df$EAD
df$EAD90 <- df$EAD
df$EAD120 <- df$EAD
df$EAD150 <- df$EAD
df$EAD180 <- df$EAD
df$EAD240 <- df$EAD
df$EAD300 <- df$EAD

# Define cutoffs for each bucket 
cut_0pct <- 
  c(0.5,0.5,0.4,0.4,0.35,0.3,0.2,0.15,0.1)
cut_5pct <- 
  c(0.6,0.45,0.4,0.3,0.3,0.25,0.2,0.15,0.1)
cut_10pct <- 
  c(0.5,0.45,0.35,0.3,0.3,0.25,0.2,0.1,0.075)
cut_20pct <- 
  c(0.5,0.35,0.3,0.275,0.25,0.2,0.2,0.1,0.075)
cut_30pct <- 
  c(0.4,0.35,0.3,0.25,0.2,0.2,0.15,0.1,0.075)
cut_40pct <- 
  c(0.4,0.35,0.3,0.25,0.2,0.15,0.125,0.1,0.075)
cut_50pct <- 
  c(0.4,0.35,0.3,0.25,0.2,0.15,0.15,0.1,0.075)
cut_60pct <- 
  c(0.35,0.3,0.25,0.2,0.2,0.15,0.15,0.1,0.075)
cut_70pct <- 
  c(0.35,0.3,0.25,0.2,0.2,0.15,0.125,0.1,0.075)
cut_80pct <- 
  c(0.35,0.3,0.25,0.2,0.15,0.15,0.1,0.075,0.075)
cut_90pct <- 
  c(0.3,0.3,0.25,0.2,0.2,0.15,0.15,0.1,0.075)
cut_100pct <- 
  c(0.3,0.25,0.25,0.225,0.2,0.175,0.1,0.1,0.075)

# Set payment diffs to current
df$last_payment_before_def <- 
  as.integer(df$current_time_diff)
df$last_payment_before_def <- 
  as.integer(df$last_payment_before_def)
df$payment_time_diff30 <- df$current_time_diff
df$payment_time_diff60 <- df$current_time_diff
df$payment_time_diff90 <- df$current_time_diff
df$payment_time_diff120 <- df$current_time_diff
df$payment_time_diff150 <- df$current_time_diff
df$payment_time_diff180 <- df$current_time_diff
df$payment_time_diff240 <- df$current_time_diff
df$payment_time_diff300 <- df$current_time_diff

# Make double
df$payment_time_diff30 <- 
  as.numeric(df$payment_time_diff30)
df$payment_time_diff60 <- 
  as.numeric(df$payment_time_diff60)
df$payment_time_diff90 <- 
  as.numeric(df$payment_time_diff90)
df$payment_time_diff120 <- 
  as.numeric(df$payment_time_diff120)
df$payment_time_diff150 <- 
  as.numeric(df$payment_time_diff150)
df$payment_time_diff180 <- 
  as.numeric(df$payment_time_diff180)
df$payment_time_diff240 <- 
  as.numeric(df$payment_time_diff240)
df$payment_time_diff300 <- 
  as.numeric(df$payment_time_diff300)

# Define function to calculate predicted return rates for each bucket
RR_fct <- function(cutoff_vect, mod1, mod2, mod3, 
                   mod4, mod5, mod6, mod7, mod8, mod9){
  return(ifelse(df$days_delay<(days_def + 30), 
         ifelse(predict(mod1, df)<=cutoff_vect[1],0,1),
         ifelse(df$days_delay<(days_def + 60), 
         ifelse(predict(mod2, df)<=cutoff_vect[2],0,1),
         ifelse(df$days_delay<(days_def + 90), 
         ifelse(predict(mod3, df)<=cutoff_vect[3],0,1),
         ifelse(df$days_delay<(days_def + 120), 
         ifelse(predict(mod4, df)<=cutoff_vect[4],0,1),
         ifelse(df$days_delay<(days_def + 150), 
         ifelse(predict(mod5, df)<=cutoff_vect[5],0,1),
         ifelse(df$days_delay<(days_def + 180), 
         ifelse(predict(mod6, df)<=cutoff_vect[6],0,1),
         ifelse(df$days_delay<(days_def + 240), 
         ifelse(predict(mod7, df)<=cutoff_vect[7],0,1),
         ifelse(df$days_delay<(days_def + 300), 
         ifelse(predict(mod8, df)<=cutoff_vect[8],0,1),
         ifelse(predict(mod9, df)<=cutoff_vect[9],0,1))))))))))
}

# Calculate return rate for each RR bucket
RR0pct <- RR_fct(cut_0pct,dt0_0pct,dt30_0pct,
                 dt60_0pct,dt90_0pct,
                 dt120_0pct,dt150_0pct,dt180_0pct,
                 dt240_0pct,dt300_0pct)
RR5pct <- RR_fct(cut_5pct,dt0_5pct,dt30_5pct,
                 dt60_5pct,dt90_5pct,
                 dt120_5pct,dt150_5pct,dt180_5pct,
                 dt240_5pct,dt300_5pct)
RR10pct <- RR_fct(cut_10pct,dt0_10pct,dt30_10pct,
                  dt60_10pct,dt90_10pct,
                  dt120_10pct,dt150_10pct,dt180_10pct,
                  dt240_10pct,dt300_10pct)
RR20pct <- RR_fct(cut_20pct,dt0_20pct,dt30_20pct,
                  dt60_20pct,dt90_20pct,
                  dt120_20pct,dt150_20pct,dt180_20pct,
                  dt240_20pct,dt300_20pct)
RR30pct <- RR_fct(cut_30pct,dt0_30pct,dt30_30pct,
                  dt60_30pct,dt90_30pct,
                  dt120_30pct,dt150_30pct,
                  dt180_30pct,dt240_30pct,dt300_30pct)
RR40pct <- RR_fct(cut_40pct,dt0_40pct,dt30_40pct,
                  dt60_40pct,dt90_40pct,
                  dt120_40pct,dt150_40pct,
                  dt180_40pct,dt240_40pct,dt300_40pct)
RR50pct <- RR_fct(cut_50pct,dt0_50pct,dt30_50pct,
                  dt60_50pct,dt90_50pct,
                  dt120_50pct,dt150_50pct,
                  dt180_50pct,dt240_50pct,dt300_50pct)
RR60pct <- RR_fct(cut_60pct,dt0_60pct,dt30_60pct,
                  dt60_60pct,dt90_60pct,
                  dt120_60pct,dt150_60pct,
                  dt180_60pct,dt240_60pct,dt300_60pct)
RR70pct <- RR_fct(cut_70pct,dt0_70pct,dt30_70pct,
                  dt60_70pct,dt90_70pct,
                  dt120_70pct,dt150_70pct,
                  dt180_70pct,dt240_70pct,dt300_70pct)
RR80pct <- RR_fct(cut_80pct,dt0_80pct,dt30_80pct,
                  dt60_80pct,dt90_80pct,
                  dt120_80pct,dt150_80pct,dt180_80pct,
                  dt240_80pct,dt300_80pct)
RR90pct <- RR_fct(cut_90pct,dt0_90pct,dt30_90pct,
                  dt60_90pct,dt90_90pct,
                  dt120_90pct,dt150_90pct,
                  dt180_90pct,dt240_90pct,dt300_90pct)
RR100pct <- RR_fct(cut_100pct,dt0_100pct,dt30_100pct,
                   dt60_100pct,dt90_100pct,
                   dt120_100pct,dt150_100pct,
                   dt180_100pct,dt240_100pct,dt300_100pct)

# Join to main data frame
df <- cbind(df, RR0pct, RR5pct, RR10pct, 
            RR20pct, RR30pct, RR40pct, 
            RR50pct, RR60pct, RR70pct, 
            RR80pct, RR90pct, RR100pct)

# Join binary return rates
df$RR1 <- ifelse(df$RR0pct==0, 0,
   ifelse(df$RR5pct==0, 0.025,
   ifelse(df$RR10pct==0, 0.075,
   ifelse(df$RR20pct==0, 0.15,
   ifelse(df$RR30pct==0, 0.25,
   ifelse(df$RR40pct==0, 0.35,
   ifelse(df$RR50pct==0, 0.45,
   ifelse(df$RR60pct==0, 0.55,
   ifelse(df$RR70pct==0, 0.65,
   ifelse(df$RR80pct==0, 0.75,
   ifelse(df$RR90pct==0, 0.85,
   ifelse(df$RR100pct==0, 0.95,1))))))))))))

df$RR2 <- ifelse(df$RR100pct==1, 1,
   ifelse(df$RR90pct==1, 0.95,
   ifelse(df$RR80pct==1, 0.85,
   ifelse(df$RR70pct==1, 0.75,
   ifelse(df$RR60pct==1, 0.65,
   ifelse(df$RR50pct==1, 0.55,
   ifelse(df$RR40pct==1, 0.45,
   ifelse(df$RR30pct==1, 0.35,
   ifelse(df$RR20pct==1, 0.25,
   ifelse(df$RR10pct==1, 0.15,
   ifelse(df$RR5pct==1, 0.075,
   ifelse(df$RR0pct==1, 0.025,0))))))))))))

# Treat predicted return rate to produce more accurate results
df$RR <- (df$RR1 + df$RR2) / 2



##############################
# Calculate Expected Loss    #
##############################

# Define days delay 2
df$days_delay_bin2 <- cut(df$days_delay,
                          c(89,179,269,359,10000))
df$days_delay_bin3 <- cut(df$days_delay,
                          c(-1,10,30,60,90,180,270,360,100000))
df$days_delay_bin4 <- cut(df$days_delay,
                          c(-1,10,30,60,90,180,270,270+90,100000))
df$days_delay_bin5 <- cut(df$days_delay,
                          c(-1,30,90,180,10000))

# Get collected amounts
df$collected <- ifelse(df$Saldo<0, 0, df$RR * df$Saldo)
sum(df$collected)/sum(df$Saldo)*100

# Compute LGD 
df$LGD <- 1 - df$RR

# Get Probabilty of Default
df <- merge(df, PD, 
            by.x = "credit_number", 
            by.y = "credit_number", 
            all.x = TRUE)

# Make hypothesis for non-default PDs
df$PD <- ifelse(df$days_delay>=90,1,
         ifelse(df$PD<=0.2,0.2,0.8))

# Set LGD for non-defaults
df$LGD <- ifelse(df$days_delay<90, 0.67, df$LGD)

# Discretize LGD by buckets
df$LGD <- ifelse(df$days_delay<90, df$LGD,
                 ifelse(df$LGD<=0.9, 0.9, 1))

# Apply some additional criteria for high DPDs
df$LGD <- ifelse(df$LGD<1 & df$days_delay>=180 & 
    is.na(df$last_pay_date), 1, df$LGD) 
df$LGD <- ifelse(df$LGD<1 & df$days_delay>=180 & 
    difftime(as.Date(date_provisions), 
    df$last_pay_date, units=c("days"))>=180, 1, df$LGD)

# Take into account collateral
df$Saldo_less_collateral <- ifelse(df$Saldo-df$collateral<=0, 
                                   0, df$Saldo-df$collateral)

# Get average PD per days delay bucket for non-defaults
non_def <- subset(df, df$days_delay<90)
aggregate(non_def$PD, by=list(days_delay=non_def$days_delay_bin2), 
          FUN=mean)

# Define Expected Loss
df$EL <- ifelse(df$Saldo_less_collateral<=0, 0, 
                df$Saldo_less_collateral * df$PD * df$LGD)

# Sum Expected Loss and Exposure
sum_EL_all <- sum(df$EL)
sum_exposure <- sum(df$Saldo_less_collateral)

# Sum Expected Loss for credirect
df$credirect_flag <- ifelse(substring(
  df$product_name,0,5)=="CreDi",1,0)
credirect <- subset(df, df$credirect_flag==1)
sum_EL_credirect <- sum(credirect$EL)
sum_exposure_credirect <- sum(credirect$Saldo_less_collateral)

# Make table with summed EL and exposure
table_sum <- as.data.frame(cbind(rbind(sum_EL_all,sum_exposure),
      rbind(sum_EL_credirect ,sum_exposure_credirect)))
names(table_sum) <- c("all","credirect")

# Define EL percentage
df$EL_Pct <- ifelse(df$Saldo==0, 0,
                    df$EL / df$Saldo_less_collateral)



##############################
# Finalize some other fields #
##############################

# Define month and year of activation
df$month_activation <- as.numeric(substring(df$date,6,7))
df$year_activation <- as.numeric(substring(df$date,1,4))

df$month_provisions <- as.numeric(substring(month_prov,6,7))
df$year_provisions <- as.numeric(substring(month_prov,1,4))

# Arrange office and zone
df$office_current <- as.character(df$office_current)
df$zone_current <- as.character(df$office_current)
Encoding(df$office_current) <- "UTF-8"
Encoding(df$zone_current) <- "UTF-8"

# Set office
df$office <- ifelse(is.na(df$first_office), df$office, df$first_office)

df_bu <- df

#########################################################
# Filter credits for cession with specified criteria    #
#########################################################

prev_provisions <- read.xlsx(paste(
  dir_prev_provisions,"\\Provisions_",
  date_prev_first_of_month,".xlsx", sep=""), 1)
prev_provisions <- subset(prev_provisions, prev_provisions$EL_Pct==1)

offices_to_remove <-  c("CreDirect","CreDirect-Creditour",
     "CreDirect-WebBroker","Загуба", "Преструктурирани кредити",
     "Каса Корпоративна Сигурност","Починали","Полиция",
     "СЪД-КРЕДИРЕКТ","СЪД-СИТИКЕШ",
     "Централен офис","ЦЕНТРАЛЕН ОФИС-КРЕДИРЕКТ","За продажба КРЕДИРЕКТ")

cession1 <- subset(df, df$days_delay>=180)
cession1 <- cession1[cession1$credit_number %in% 
                       prev_provisions$credit_number,]
cession1$offices_to_remove <- ifelse(cession1$office_current %in%
                                       offices_to_remove, 1, 0)
cession2 <- subset(cession1, cession1["offices_to_remove"]==0)
cession2$last_pay_tonow <- difftime(Sys.time(), 
                                    cession2$last_pay_date, units=c("days"))
cession2 <- subset(cession2, cession2$last_pay_tonow>=(6*30) | 
                     is.na(cession2$last_pay_tonow))
cession2 <- subset(cession2, 
                   cession2$office_current != "Финстарт Дискаунт" & 
                     cession2$office_current!="Финстарт")

last_padej <- plan_raw[,c("application_id", "pay_day_det")]
last_padej <- last_padej[last_padej$application_id %in% 
                           cession2$id,]
last_padej <- last_padej[order(last_padej$application_id),]
last_padej <- last_padej[rev(order(as.Date(
  last_padej$pay_day_det))),]
last_padej <- last_padej[!duplicated(last_padej$application_id),]
names(last_padej)[2] <- "max_pay_day"

cession2 <- merge(cession2, last_padej, 
                  by.x = "id", 
                  by.y = "application_id", all.x = TRUE)
cession2 <- subset(cession2, difftime(
  as.Date(cession2$max_pay_day), Sys.time(), units = "days")<0)

coorp_secu$coorporate_security <- 1
cession2 <- merge(cession2, coorp_secu, 
                  by.x = "credit_number", 
                  by.y = "credit_number", all.x = TRUE)
cession <- subset(cession2, is.na(cession2$coorporate_security))
cession$EL_Pct <- 1

cession <- cession[,c("credit_number","product_name",
   "days_delay","EL_Pct","office_current",
   "last_pay_date","max_pay_day","Saldo",
   "taksi","neustoiki","lihvi","glavnitsi","activated_at")]
names(cession) <- c("credit_number","product_name","DPD",
           "EL_Pct","office_current","last_pay_date",
           "last_installment_date","Салдо",
           "Такси","Неустойки","Лихви","Главница","activated_at")

# Get date of first tax creation of 20% - 25% and if third side date
cession <- merge(cession,
      df[,c("credit_number","tax_20pct_created","third_side_date")],
      by.x = "credit_number", by.y = "credit_number", all.x = TRUE)



################################
# Compute stats on provisions  #
################################

# Make dataframe for analysis with variables which enter models
analysis <- df[,c(1,14,21,35,23,29,27,28,34,
                  43,47,49,40,41,52:59,86:93,40,62,
                  63,66,64,113,112)]

# Aggregate LGD by DPD
aggregate(df$LGD, by=list(df$days_delay_bin2), 
          FUN=mean)

# Sum EL per product ID 
dpd_prod <- as.data.frame(aggregate(df$EL,
                                    by=list(id=df$product_name), FUN=sum))
dpd_prod <- dpd_prod[rev(order(dpd_prod$x)),]
names(dpd_prod) <- c("product_name","sum_EL")
dpd_prod$id <- seq(1,nrow(dpd_prod),1)
dpd_prod <- subset(dpd_prod, dpd_prod$id<=10)
piepercent <- round(100*dpd_prod$sum_EL/sum(dpd_prod$sum_EL), 1)
piepercent <- paste(dpd_prod$product_name," ",
                    piepercent,"%", sep="")
pic_path <- paste(dir_charts,"\\piechart.png", sep="")
png(filename=pic_path,width=1500,height=800)
pie(dpd_prod$sum_EL, 
    labels = piepercent, cex = 1.4, 
    col = rainbow(length(dpd_prod$id)))
dev.off()

# Sum EL by DPD bin
exposure <- aggregate(df$Saldo, 
         by=list(days_delay=df$days_delay_bin3), FUN=sum)
expected_loss <- aggregate(df$EL, 
         by=list(days_delay=df$days_delay_bin3), FUN=sum)
expected_loss_without_finstart <- aggregate(df$EL, 
         by=list(days_delay=df$days_delay_bin3), FUN=sum)
table_exp <- cbind(expected_loss, exposure)[,c(1,2,4)]
names(table_exp) <- 
  c("days_delay","EL_per_group","exposure_per_group")
exposure <- aggregate(df$Saldo, 
                      by=list(days_delay=df$days_delay_bin5), FUN=sum)
expected_loss <- aggregate(df$EL, 
                           by=list(days_delay=df$days_delay_bin5), FUN=sum)
expected_loss_without_finstart <- aggregate(
  df$EL[!df$product_name=="Финстарт Дискаунт"], 
  by=list(days_delay=df$days_delay_bin5[!df$product_name=="Финстарт Дискаунт"]), 
  FUN=sum)
table_exp2 <- cbind(expected_loss, expected_loss_without_finstart,
                    exposure)[,c(1,2,4,6)]
names(table_exp2) <-
  c("days_delay","EL_per_group","EL_per_group_without_finstart","
    exposure_per_group")

# Sum EL by defaulted or not 
def_tab <- aggregate(df$EL,by=list(id=df$default_flag), 
                     FUN=sum)
def_tab[,2] <- def_tab[,2]/sum(def_tab$x)*100
jpeg(file=paste(dir_charts,"\\Plot_Defaults_Sum_EL.png",sep=""), 
     width = 700, height = 700)
barplot(def_tab$x, names.arg = def_tab$id, 
        xlab="Defaults", 
        ylab="Sum of EL (% total)", ylim = c(0,100),
        cex.axis=1.5, cex.names=1.5, cex.lab = 1.5)
dev.off()

# Sum EL by DPD (more bins)
dpd <- aggregate(df$EL,
                 by=list(id=df$days_delay_bin4), FUN=sum)
dpd[,2] <- dpd[,2]/sum(dpd$x)*100
jpeg(file=paste(dir_charts,"\\Plot_DPD_Sum_EL.png",sep=""), 
     width = 700, height = 700)
barplot(dpd$x, names.arg = dpd$id, 
        xlab="Defaults", 
        ylab="Sum of EL (% total)", ylim = c(0,40), 
        cex.axis=1.5, cex.names=1.5, cex.lab = 1.5)
dev.off()

# Average EL by days past due
dpd_tot <- aggregate(df$EL,by=list(id=df$days_delay_bin4), 
                     FUN=sum)
dpd_count <- table(df$days_delay_bin4)
avg_dpd_rat <- dpd_tot[,2]/dpd_count
avg_dpd_rat <-as.data.frame(avg_dpd_rat)
jpeg(file=paste(dir_charts,"\\Plot_DPD_Average_EL.png",sep=""), 
     width = 700, height = 700)
barplot(avg_dpd_rat$Freq, 
        names.arg = avg_dpd_rat$Var1, 
        xlab="Days Past Due", ylab="Average Expected Loss", 
        ylim = c(0,1000), cex.axis=1.5, cex.names=1.5, 
        cex.lab = 1.5)
dev.off()




#############################################
# Write in external Excel file output data  #
#############################################

# Set main directory
setwd(main_dir)

# Set output dataframes
output <- df[,c("credit_number", 
                "product_name","office", 
                "days_delay","Saldo", 
                "EL", "EL_Pct",
                "month_activation", "year_activation",
                "month_provisions", "year_provisions")]
names(output)[3:5] <- c("office","dpd","saldo")
raw_data <- df[,c("credit_number",
                  "product_name","office", 
                  "days_delay","Saldo",
                  "LGD","EL","EL_Pct","PD","max_DPD_upto",
                  "ratio_passed_installments","last_payment",
                  "payments_per_installments","education",
                  "experience_employer","period","gender","age",
                  "ratio_installment_income","credits_cum",
                  "days_diff_last_credit","max_delay",
                  "ratio_last_amount_paid","last_payment_before_def",
                  "paid_time30","payment_time_diff30")]
names(raw_data)[(ncol(raw_data)-1):ncol(raw_data)] <- 
  c("paid_time","payment_time_diff")
raw_data$last_payment_before_def <- 
  raw_data$payment_time_diff

# Create worksheets
OUT <- createWorkbook()
addWorksheet(OUT, "Data")
addWorksheet(OUT, "Overall")
addWorksheet(OUT, "EL per DPD1")
addWorksheet(OUT, "EL per DPD2")
addWorksheet(OUT, "EL per product")
addWorksheet(OUT, "Cession")
addWorksheet(OUT, "Raw")

# Write to worsheets
writeData(OUT, sheet = "Data", x = output)
writeData(OUT, sheet = "Overall", 
          x = table_sum, rowNames=TRUE)
writeData(OUT, sheet = "EL per DPD1", x = table_exp)
writeData(OUT, sheet = "EL per DPD2", x = table_exp2)
writeData(OUT, sheet = "Cession", x = cession)
writeData(OUT, sheet = "Raw", x = raw_data)
insertImage(OUT, "EL per product",  
            paste(dir_charts,"\\piechart.png", sep=""))

# Save and close
saveWorkbook(OUT, output_name)



#######
# END #
#######



