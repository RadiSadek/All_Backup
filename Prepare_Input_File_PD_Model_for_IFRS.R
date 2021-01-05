

############## PD FOR PROVISIONS ###############

################################################
# PREPARE FILE FOR PD MODELLED ON DPD BUCKETS  #
################################################

# set the seed
set.seed(550)

# Load libraries
library(caTools)
library(ROCR)
library(Hmisc)
library(plyr)
library(binr)
library(gmodels)
library(cluster)
library(gmodels)
library(party)
library(nnet)
library(dplyr)
library(RMySQL)

# Function for taking only complete rows 
complete_fct <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
# Function to count number of missing values
count_empty <- function(var){
  return(sum(is.na(var)))
}
# Create function for ordering by date 
order_fct <- function(data){
  return (data[with(data, order(date)), ])
}



#####################################################
############ Make connection with SQL  ##############
#####################################################

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

data_sql2 <- suppressWarnings(dbSendQuery(con, 
"SELECT object_id, amount, pay_date FROM citycash_db.cash_flow
WHERE nomenclature_id in (90,100,101) AND deleted_at IS NULL 
AND object_type=4"))
data_sql2 <- fetch(data_sql2, n=-1)
paids <- merge(data_sql2,df[,c("id","egn","credit_number")],
               by.x = "object_id",by.y = "id",all.x = TRUE)
paids <- fetch(data_sql2, n=-1)
data_sql3 <- suppressWarnings(dbSendQuery(con, 
"SELECT application_id, installment_num, pay_day, days_delay, payed_at 
FROM citycash_db.credits_plan_main"))
plan <- fetch(data_sql3, n=-1)


#############################################
########## Work on main dataframe ###########
#############################################

# Main dataframe
df <- subset(df,df$sub_status==128)
names(plan)[3:4] <- c("pay_day_det","days_delay_curr")

# Make eventual tests
#df <- subset(df, substring(df$egn,0,2)=="83")

# Select date with ckr_status
df$date <- as.Date(df$date)
df <- subset(df, df$date>"2016-03-07")
df <- subset(df, df$date<="2018-03-31")
df <- subset(df,df$total_income<10000)

# Calculate number of previous online, offline credits, 
#date difference and number of previous defaulted and refinance credits
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]

df$online <- ifelse(df$online_offline=="online",1,0)
df$offline <- ifelse(df$online_offline=="offline",1,0)
df$offline_cum <-0
df$online_cum <-0
df$default_cum <-0
df$credits_cum <-0
df$refinance_cum <-0
df$cession_cum <- 0
df$days_diff_last_credit <- NA

df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1] & df$status[i-1]==5) 
  {df$offline_cum[i] <- df$offline[i]+df$offline_cum[i-1]
  df$online_cum[i] <- df$online[i]+df$online_cum[i-1]
  }
  else if (df$egn[i]==df$egn[i-1]) {
    df$offline_cum[i] <- df$offline_cum[i-1]
    df$online_cum[i] <- df$online_cum[i-1]
  }
  if (df$egn[i]==df$egn[i-1]){
    df$default_cum[i] <- df$default_flag[i-1]+df$default_cum[i-1]
    df$days_diff_last_credit[i] <- as.numeric(as.Date(df$date[i]) - 
       as.Date(df$deactivated_at[i-1],format="%Y-%m-%d"))
    df$cession_cum[i] <- df$cession_cum[i-1] + df$cession[i-1]
  }
  df$credits_cum[i] <- df$online_cum[i] + df$offline_cum[i]
}

# Flag if previous is refinance or not 
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_previous <- 0
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_previous <- ifelse(abs(df$days_diff_last_credit)<=1,1,0)
df$refinance_previous <- ifelse(is.na(df$refinance_previous), 0, 
                                df$refinance_previous)

# Cumulative refinance
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_cum <- 0
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {
    df$refinance_cum[i] <- df$refinance_previous[i]+df$refinance_cum[i-1]
  }
}

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
      df$max_delay[i] <- df$max_delay[i-1]
      } else {df$max_delay[i] <- df$days_delay[i-1]}
  } 
  else {df$max_delay[i]=NA} 
}

# Calculate if user has previous default credit (boolean) 
df$default_previous <- ifelse(df$default_cum>0,1,0)
df$cession_previous <- ifelse(df$cession_cum>0,1,0)

# Correct_fields and filter wrong data
df$age <- as.numeric(ifelse(nchar(df$egn, type = "chars", 
  allowNA = FALSE, keepNA = NA)==8,
  as.numeric(substring(df$date_entry,1,4))-2000, df$age))
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
df <- subset(df,!is.na(df$ratio_installment_income))

# Correct some fields
df$days_diff_last_credit <- ifelse(df$days_diff_last_credit<0,0,
                                   df$days_diff_last_credit)
df$default_cum <- ifelse(df$credits_cum==0,NA,df$default_cum)


# Give name to selected products
df$product_name <- ifelse(df$product_id==1,"City Month",
                   ifelse(df$product_id %in% c(5,11,23,24),"City Week",
                   ifelse(df$product_id==6,"City 2-Week",
                   ifelse(df$product_id==7,"Pensioner",
                   ifelse(df$product_id==8,"VIP Week",
                   ifelse(df$product_id %in% c(30,31,32,33,35),"0%",
                   ifelse(df$product_id %in% c(9,25,26,27,28,36,37),"Credirect",
                      "Other")))))))


#############################################
########## Work on payment behavior #########
#############################################

# Read paids
paids_raw <- paids

# Correct date default
df$date_default <- as.Date(df$date_default, format="%Y-%m-%d")
plan$pay_day_det <- as.Date(plan$pay_day_det, format="%Y-%m-%d")

# Get detailed delays
df_cred <- df[,c("credit_number","id")]
plan  <- merge(df_cred, plan, by.x = "id", by.y = "application_id", all.y = TRUE)

# Work on credits plan main (with days of delay)
plan <- plan[order(plan$credit_number,plan$pay_day_det),]
plan_nb_installments <- as.data.frame(aggregate(plan$installment_num, 
    by=list(credit_number=plan$credit_number),FUN=max))
names(plan_nb_installments)[ncol(plan_nb_installments)] <- "nb_installments"
plan <- merge(plan, plan_nb_installments, 
              by.x = "credit_number", by.y = "credit_number",
              all.x = TRUE)
rm(plan_nb_installments)

# Order by credit number and pay date
plan <- subset(plan, !is.na(plan$credit_number))
plan <- plan[order(plan$credit_number,plan$pay_day_det),]

# Create variables max DPD, average DPD and ratio passed installments
plan$ratio_passed_installments <- plan$installment_num / plan$nb_installments
plan <- plan[order(plan$credit_number,plan$installment_num),]

plan$max_DPD_mov <- NA
plan$max_DPD_mov[1] <- plan$days_delay_curr[1]
plan$DPD_avg <- NA
plan$DPD_avg[1] <- plan$days_delay_curr[1]
plan$max_DPD_upto <- NA
plan$max_DPD_upto[2] <- plan$max_DPD_mov[1]
plan$DPD_avg_upto <- NA
plan$DPD_avg_upto[2] <- plan$days_delay_curr[1]

for (i in 2:nrow(plan)){
  if (plan$credit_number[i]==plan$credit_number[i-1]){
    plan$DPD_avg[i] <- plan$days_delay_curr[i]+plan$DPD_avg[i-1]
    if(plan$days_delay_curr[i]<plan$max_DPD_mov[i-1]){
      plan$max_DPD_mov[i] <- plan$max_DPD_mov[i-1]
    }
    else{
      plan$max_DPD_mov[i] <- plan$days_delay_curr[i]
    }
    plan$max_DPD_upto[i] <- plan$max_DPD_mov[i-1]
    plan$DPD_avg_upto[i] <- plan$DPD_avg[i-1]
  }
  else{
    plan$max_DPD_mov[i] <- plan$days_delay_curr[i]
    plan$DPD_avg[i] <- plan$days_delay_curr[i]
    plan$max_DPD_upto[i] <- NA
    plan$DPD_avg_upto[i] <- NA
  }
}

plan$DPD_avg <- plan$DPD_avg/plan$installment_num 
plan$DPD_avg_upto <- plan$DPD_avg_upto / (plan$installment_num-1)

# Make DPD max AFTER the current installment
plan <- plan[order(plan$credit_number,-plan$installment_num),]
plan$max_DPD_rev <- NA
plan$max_DPD_rev[1] <- plan$days_delay_curr[1]
plan$max_DPD_rev_upto <- NA
plan$max_DPD_rev_upto[2] <- plan$days_delay_curr[1]
for (i in 2:nrow(plan)){
  if (plan$credit_number[i]==plan$credit_number[i-1]){
    if(plan$days_delay_curr[i]>plan$days_delay_curr[i-1]){
      if(plan$days_delay_curr[i]<plan$max_DPD_rev[i-1]){
        plan$max_DPD_rev[i] <- plan$max_DPD_rev[i-1]
      }
      else{
        plan$max_DPD_rev[i] <- plan$days_delay_curr[i]
      }
    }
    else{
      plan$max_DPD_rev[i] <- plan$max_DPD_rev[i-1]
    }
    plan$max_DPD_rev_upto[i] <- plan$max_DPD_rev[i-1]
  }
  else {
    plan$max_DPD_rev[i] <- plan$days_delay_curr[i]
  }}


# Reorder conveniently
plan <- plan[order(plan$credit_number,plan$installment_num),]

# Apply date format
plan$pay_day_det <- as.Date(plan$pay_day_det)

# Get how much paid up-to 
plan$date_curr <- as.Date(plan$pay_day_det) + plan$days_delay_curr

# Filter paid in advance
plan <- subset(plan, !(plan$days_delay_curr==0 & is.na(payed_at)))

# Choose relative columns and output
all_plan <- plan[,c("credit_number","days_delay_curr","installment_num",
                    "ratio_passed_installments","max_DPD_mov",
                    "max_DPD_rev",
                    "DPD_avg","max_DPD_upto","DPD_avg_upto")]
all_plan <- all_plan[order(all_plan$credit_number,all_plan$installment_num),]
#all_plan <- subset(all_plan, all_plan$ratio_passed_installments!=1)
all_plan$default_next <- ifelse(all_plan$max_DPD_rev>90,1,0)

#  Make DPD columns
all_plan$dpd_1 <- 1
all_plan$dpd_2 <- ifelse(all_plan$days_delay_curr<1, 0, 1)
all_plan$dpd_3 <- ifelse(all_plan$days_delay_curr<6, 0, 1)
all_plan$dpd_4 <- ifelse(all_plan$days_delay_curr<10, 0, 1)
all_plan$dpd_5 <- ifelse(all_plan$days_delay_curr<20, 0, 1)
all_plan$dpd_6 <- ifelse(all_plan$days_delay_curr<30, 0, 1)
all_plan$dpd_7 <- ifelse(all_plan$days_delay_curr<40, 0, 1)
all_plan$dpd_8 <- ifelse(all_plan$days_delay_curr<50, 0, 1)
all_plan$dpd_9 <- ifelse(all_plan$days_delay_curr<60, 0, 1)
all_plan$dpd_10 <- ifelse(all_plan$days_delay_curr<70, 0 ,1)
all_plan$dpd_11 <- ifelse(all_plan$days_delay_curr<80, 0,1)
 
# Apply to several buckets of DPD
all_plan_merge <- merge(df, all_plan, by.x = "credit_number", 
                        by.y = "credit_number", all.y = TRUE)

# Choose relevant columns
output_df <- all_plan_merge[,c("credit_number","age","maturity","ckr_status",
"marital_status","gender","ownership","education",
"household_children","purpose","experience_employer","on_address",
"ratio_installment_income","total_income","max_delay",
"status_work","credits_cum","days_diff_last_credit","default_next",
"days_delay_curr","installment_num",
"ratio_passed_installments","max_DPD_upto","DPD_avg_upto",
"dpd_1","dpd_2","dpd_3","dpd_4","dpd_5","dpd_6","dpd_7","dpd_8",
"dpd_9","dpd_10","dpd_11")]

# Check results
output_df <- output_df[
  order(output_df$credit_number,output_df$installment_num),]
View(output_df[,c(1,15:27)])

# Create function 
get_df <- function(DPD_span){
  all_plan_DPD <- subset(output_df, output_df[,DPD_span]==1)
  return(ddply(all_plan_DPD ,.(credit_number), 
  function(x) x[sample(nrow(x),1),]))
}

# Discretize into buckets of DPD
plan1 <- get_df(25)
plan2 <- get_df(26)
plan3 <- get_df(27)
plan4 <- get_df(28)
plan5 <- get_df(29)
plan6 <- get_df(30)
plan7 <- get_df(31)
plan8 <- get_df(32)
plan9 <- get_df(33)
plan10 <- get_df(34)
plan11 <- get_df(35)


# Function for calc. individual last payment and number of payments/installments
plan_paid_fct <- function(dff, bucket_dpd, bucket_dpd_up){
  plan_paid <- plan[,c("credit_number","installment_num",
                       "pay_day_det","days_delay_curr","date_curr")]
  plan_paid <- merge(plan_paid, paids, by.x = "credit_number", 
                     by.y = "credit_number", all.x = TRUE)
  plan_paid$date_curr <- as.Date(ifelse(
    plan_paid$days_delay_curr<=(bucket_dpd_up), as.Date(plan_paid$date_curr),
    as.Date(as.Date(plan_paid$pay_day_det) + 
    round(runif(1, bucket_dpd, bucket_dpd_up)))))
  plan_paid$last_payment <- difftime(plan_paid$date_curr, 
    plan_paid$pay_date, units = c("days"))
  plan_paid <- subset(plan_paid, plan_paid$last_payment>1)
  plan_paid$count <- 1
  agg <- aggregate(plan_paid$count, by=list(
    credit_number=plan_paid$credit_number,
    installment_num=plan_paid$installment_num), FUN=sum)
  agg$x <- agg$x / (agg$installment_num)
  names(agg)[ncol(agg)] <- "payments_per_installments"
  plan_paid <- merge(plan_paid, agg, 
    by.x = c("credit_number","installment_num"),
    by.y = c("credit_number","installment_num"), all.x = TRUE)
  plan_paid <- plan_paid[order(plan_paid$credit_number, 
                               plan_paid$installment_num, plan_paid$last_payment),]
  plan_paid <- plan_paid[!duplicated(plan_paid[c(1,2)]),]
  plan_paid <- plan_paid[,c("credit_number","installment_num",
                            "last_payment","payments_per_installments")]
  plan_paid$last_payment <- as.numeric(round(plan_paid$last_payment, 1))
  plan_paid$payments_per_installments <- as.numeric(round(
    plan_paid$payments_per_installments, 2))
  dfff <- merge(dff, plan_paid, 
                by.x = c("credit_number","installment_num"), 
                by.y = c("credit_number","installment_num"),
                all.x=TRUE)
  return(dfff)

}

# Compute last payment and number of payments/installments
plan1b <- plan_paid_fct(plan1, 0, 0)
plan2b <- plan_paid_fct(plan2, 1, 5)
plan3b <- plan_paid_fct(plan3, 6, 9)
plan4b <- plan_paid_fct(plan4, 10, 19)
plan5b <- plan_paid_fct(plan5, 20, 29)
plan6b <- plan_paid_fct(plan6, 30, 39)
plan7b <- plan_paid_fct(plan7, 40, 49)
plan8b <- plan_paid_fct(plan8, 50, 59)
plan9b <- plan_paid_fct(plan9, 60, 69)
plan10b <- plan_paid_fct(plan10, 70, 79)
plan11b <- plan_paid_fct(plan11, 80, 89)



##########################################
###### Output bucket files (.csv) ########
##########################################

write.csv(plan1b,"buckets\\2nd_method\\Bucket1.csv")
write.csv(plan2b,"buckets\\2nd_method\\Bucket2.csv")
write.csv(plan3b,"buckets\\2nd_method\\Bucket3.csv")
write.csv(plan4b,"buckets\\2nd_method\\Bucket4.csv")
write.csv(plan5b,"buckets\\2nd_method\\Bucket5.csv")
write.csv(plan6b,"buckets\\2nd_method\\Bucket6.csv")
write.csv(plan7b,"buckets\\2nd_method\\Bucket7.csv")
write.csv(plan8b,"buckets\\2nd_method\\Bucket8.csv")
write.csv(plan9b,"buckets\\2nd_method\\Bucket9.csv")
write.csv(plan10b,"buckets\\2nd_method\\Bucket10.csv")
write.csv(plan11b,"buckets\\2nd_method\\Bucket11.csv")



####################
###### END #########
####################

