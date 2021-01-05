
#############################################################################
# Prepare input file for Loss Given Default Model - November 2018           # 
#      This file outputs the data for applying and for building the model   #
#           Make buckets for several day interval                           #
#############################################################################



##########################################
######## Libraries and functions #########
##########################################

# Call libraries 
library(dplyr)
library(RMySQL)

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



################################
########## Read data ###########
################################

# Main dataframe
setwd("C:\\Projects\\LGD_model\\data\\")
df <- subset(df,df$status %in% c(4,5))

# Read paids
paids_raw <- paids

# Compute date default
data_sql3 <- suppressWarnings(dbSendQuery(con, 
"SELECT MIN(application_id) AS appl_id, pay_day, days_delay, 
DATE_ADD(pay_day, INTERVAL 90 DAY) AS date_default 
FROM citycash_db.credits_plan_main 
WHERE days_delay>=90 
GROUP BY application_id"))
df_def <- fetch(data_sql3, n=-1)
df <- merge(df,df_def,by.x = "id",by.y = "appl_id",all.x = TRUE)
df$date_default <- as.Date(df$date_default, format="%Y-%m-%d")



####################################################
### Build data for applying and building model   ###
####################################################

########## Calculate Exposure at Default and Return Rate at default ###########

# Compute amount paid before and after default (in a 1-year window span)
df_def_date <- df[,c("id","date_default")]
paids <- merge(paids , df_def_date, by.x="object_id", by.y="id", TRUE)
paids$date_default_plus1year <- as.Date(paids$date_default) + 360
paids$amount_paid_after_def <- 
  ifelse(as.Date(paids$pay_date)<=as.Date(paids$date_default_plus1year) & 
         as.Date(paids$pay_date)>=as.Date((as.Date(paids$date_default))),
         paids$amount,0)
paids$amount_paid_before_def <- 
  ifelse(as.Date(paids$pay_date)<as.Date((as.Date(paids$date_default))),
         paids$amount,0)

paids_agg <- aggregate(paids$amount_paid_after_def, 
         by=list(id=paids$object_id), FUN=sum)
names(paids_agg)[ncol(paids_agg)] <- "amount_paid_after_def"
df <- merge(df , paids_agg, by.x="id", 
         by.y="id", TRUE, all.x=TRUE)

df$amount_paid_after_def <- ifelse(
  is.na(df$amount_paid_after_def),0,df$amount_paid_after_def)

paids_agg_bef <- aggregate(paids$amount_paid_before_def, 
         by=list(id=paids$object_id), FUN=sum)
names(paids_agg_bef)[ncol(paids_agg_bef)] <- "amount_paid_before_def"
df <- merge(df , paids_agg_bef, 
          by.x="id", by.y="id", TRUE, all.x=TRUE)
df$amount_paid_before_def <- ifelse(
         is.na(df$amount_paid_before_def),0,df$amount_paid_before_def)

# Compute Exposure at Default 
df$EAD <- df$total_amount - df$amount_paid_before_def

# Compute Return Rate
df$return_rate <- df$amount_paid_after_def/df$EAD


########## Compute other interesting fields ###########

# Compute 1-year window span
df$time_window <- as.Date(df$date_default) + 360

# Compute number and ratio of number of installments
df$flag_paid_installments <- ifelse(floor(df$amount_paid_before_def/
  (df$total_amount/df$installments))>1,1,0)
df$ratio_paid_installments <- floor(df$amount_paid_before_def/
   (df$total_amount/df$installments)) / df$installments
df$ratio_passed_installments <-  (floor(ifelse(df$period==3,
   (as.Date(df$date_default) -90 - as.Date(df$pay_day))/30,
   ifelse(df$period==2,(as.Date(df$date_default) -90 - as.Date(df$pay_day))/14,
   (as.Date(df$date_default) -90  - as.Date(df$pay_day))/7)))) / df$installments

# Last amount paid
paids_last <- paids
paids_last$flag_before_def <- ifelse(
  as.Date(paids$pay_date)<=as.Date(paids$date_default),1,0)
paids_last_def <- subset(paids_last,paids_last$flag_before_def==1)
paids_last_max <- aggregate(as.Date(paids_last_def$pay_date), 
  by=list(id=paids_last_def$object_id), FUN=max)
names(paids_last_max) <- c("object_id","max_pay_date")
paids_last_max$max_pay_date <- as.Date(paids_last_max$max_pay_date)
paids_last_max$flag <- 1
paids_last$pay_date <- as.Date(paids_last$pay_date)
paids_last <- merge(paids_last, paids_last_max, 
      by.x = c("object_id","pay_date"), 
      by.y = c("object_id", "max_pay_date"), all.x=TRUE)
paids_last <- subset(paids_last, paids_last$flag==1)
paids_last_agg <- aggregate(paids_last$amount, 
     by=list(id=paids_last$object_id), FUN=sum)
names(paids_last_agg)[ncol(paids_last_agg)] <- "last_amount_paid"
df <- merge(df, paids_last_agg, by.x="id", by.y="id", all.x=TRUE)
df$ratio_last_amount_paid <- df$last_amount_paid / df$installment_amount

# Days since last paid before def
paids_last <- paids_last[,c("object_id","pay_date")]
names(paids_last)[ncol(paids_last)] <- "last_pay_date_before_def"
paids_last <- paids_last[!duplicated(paids_last$object_id), ]
df <- merge(df, paids_last, by.x="id", by.y="object_id", all.x=TRUE)
df$last_payment_before_def <- as.numeric(
  as.Date(df$date_default) - as.Date(df$last_pay_date_before_def))

# Remove unnecessary dataframes
rm(paids_last_max, paids_last_agg, paids_agg_bef, paids_agg, 
   paids_last, paids_last_def)
  
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
df$refinance_previous <- 0
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_previous <- ifelse(abs(df$days_diff_last_credit)<=1,1,0)
df$refinance_previous <- ifelse(is.na(df$refinance_previous), 0, 
                                df$refinance_previous)

# Cumulative refinance
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


####### Calculate EAD and RR at Default Date + N days (FUNCTION)  ########

# Make a function to calculate bucket-span variables
return_rate_fct <- function(time_span){
       local_df <- df
       local_df <- local_df[ , -which(names(local_df) %in% 
          c("amount_paid_before_def","amount_paid_after_def"))]
       df_def_date <- df[,c("id","date_default")]
       df_def_date$date_default <- as.Date(df_def_date$date_default) + time_span
       paids <- paids_raw
       paids <- merge(paids , df_def_date, by.x="object_id", by.y="id", TRUE)
       paids$date_default_plus1year <- as.Date(paids$date_default) + 360
       paids$amount_paid_after_def <- ifelse(
         as.Date(paids$pay_date)<=as.Date(paids$date_default_plus1year) & 
         as.Date(paids$pay_date)>=as.Date(paids$date_default),paids$amount,0)
       paids$amount_paid_before_def <- ifelse(
         as.Date(paids$pay_date)<as.Date(paids$date_default),paids$amount,0)
       paids_agg <- aggregate(paids$amount_paid_after_def, 
         by=list(id=paids$object_id), FUN=sum)
       names(paids_agg)[ncol(paids_agg)] <- "amount_paid_after_def"
       local_df <- merge(local_df , paids_agg, 
         by.x="id", by.y="id", TRUE, all.x=TRUE)
       local_df$amount_paid_after_def <- ifelse(
         is.na(local_df$amount_paid_after_def),0,local_df$amount_paid_after_def)
       paids_agg_bef <- aggregate(paids$amount_paid_before_def, 
         by=list(id=paids$object_id), FUN=sum)
       names(paids_agg_bef)[ncol(paids_agg_bef)] <- "amount_paid_before_def"
       
       paids$payment_time_diff <- difftime(paids$date_default, 
         paids$pay_date, units = "days")
       paids_last_date <- subset(paids,!(paids$payment_time_diff<0))
       paids_last_date <- paids_last_date[
         order(paids_last_date$payment_time_diff), ]
       paids_last_date <- paids_last_date[
         order(paids_last_date$object_id), ]
       paids_last_date <- paids_last_date[
         !duplicated(paids_last_date$object_id),]
       paids_last_date <- paids_last_date[,c("object_id","payment_time_diff")]

       local_df <- merge(local_df , paids_agg_bef, 
         by.x="id", by.y="id", TRUE, all.x=TRUE)
       local_df <- merge(local_df , paids_last_date, 
         by.x="id", by.y="object_id", TRUE, all.x=TRUE)
       local_df$amount_paid_before_def <- ifelse(
         is.na(local_df$amount_paid_before_def),0,
         local_df$amount_paid_before_def)
       local_df$EAD <- local_df$total_amount - 
         local_df$amount_paid_before_def
       local_df$ratio_passed_installments <- (floor(ifelse(local_df$period==3,
        (as.Date(local_df$date_default) - 90 + time_span 
        - as.Date(local_df$pay_day))/30,
        ifelse(local_df$period==2,(as.Date(local_df$date_default) - 90 + 
        time_span  
        - as.Date(local_df$pay_day))/14,
        (as.Date(local_df$date_default) - 90 + time_span
        - as.Date(local_df$pay_day))/7))))/ local_df$installments
       return_rate_df <- as.data.frame(cbind(local_df$credit_number, 
        local_df$EAD, local_df$amount_paid_after_def/local_df$EAD, 
        local_df$ratio_passed_installments, local_df$payment_time_diff))
       names(return_rate_df) <- c("credit_number", 
        paste("EAD", time_span, sep=""), 
        paste("return_rate", time_span, sep=""),
        paste("ratio_passed_installments", time_span, sep=""), 
        paste("payment_time_diff", time_span, sep=""))
       return (return_rate_df)
}

# Call functions to make time-span buckets  
df30 <- return_rate_fct(30)
df60 <- return_rate_fct(60)
df90 <- return_rate_fct(90)
df120 <- return_rate_fct(120)
df150 <- return_rate_fct(150)
df180 <- return_rate_fct(180)
df240 <- return_rate_fct(240)
df300 <- return_rate_fct(300)

df <- merge(df, df30, by.x="credit_number", 
            by.y="credit_number", TRUE, all.x=TRUE)
df <- merge(df, df60, by.x="credit_number", 
            by.y="credit_number", TRUE, all.x=TRUE)
df <- merge(df, df90, by.x="credit_number", 
            by.y="credit_number", TRUE, all.x=TRUE)
df <- merge(df, df120, by.x="credit_number", 
            by.y="credit_number", TRUE, all.x=TRUE)
df <- merge(df, df150, by.x="credit_number", 
            by.y="credit_number", TRUE, all.x=TRUE)
df <- merge(df, df180, by.x="credit_number", 
            by.y="credit_number", TRUE, all.x=TRUE)
df <- merge(df, df240, by.x="credit_number", 
            by.y="credit_number", TRUE, all.x=TRUE)
df <- merge(df, df300, by.x="credit_number",
            by.y="credit_number", TRUE, all.x=TRUE)

df$paid_time30 <- (df$EAD - df$EAD30) / df$EAD
df$paid_time60 <- (df$EAD - df$EAD60) / df$EAD
df$paid_time90 <- (df$EAD - df$EAD90) / df$EAD
df$paid_time120 <- (df$EAD - df$EAD120) / df$EAD
df$paid_time150 <- (df$EAD - df$EAD150) / df$EAD
df$paid_time180 <- (df$EAD - df$EAD180) / df$EAD
df$paid_time240 <- (df$EAD - df$EAD240) / df$EAD
df$paid_time300 <- (df$EAD - df$EAD300) / df$EAD

# Compute specific time window 
df$returned_after <- ifelse(df$return_rate>=1,0,
                     ifelse(df$return_rate30>=1,30,
                     ifelse(df$return_rate60>=1,60,
                     ifelse(df$return_rate90>=1,90,
                     ifelse(df$return_rate120>=1,120,
                     ifelse(df$return_rate150>=1,150,
                     ifelse(df$return_rate180>=1,180,
                     ifelse(df$return_rate240>=1,240,
                     ifelse(df$return_rate300>=1,300,360)))))))))

df$time_window_correct <- as.Date(df$time_window)+df$returned_after

# Apply new time difference up to now
df$current_time_diff <-  difftime(Sys.time(), df$last_pay_date, units="days")

# Remove duplicates by credit_number (shouldn't be, but hey, let's make a check)
df <- df[!duplicated(df$credit_number), ]

# Output all rows
setwd("C:\\Projects\\LGD_model\\Apply_LGD\\")



############################################
### Build data for building model ONLY   ###
############################################


########## Filter and correct data and analyze ###########

# Filter only defaults 
df <- subset(df,df$default_flag==1)
df_raw <- df

# Filter fields
df <- subset(df,df$time_window_correct<"2018-12-31" & 
               !is.na(df$time_window_correct))

# Correct_fields and filter wrong data
df <- subset(df,df$sub_status!=126)
df$amount_paid <- as.numeric(ifelse(
  is.na(df$amount_paid),0,df$amount_paid))
df$age <- as.numeric(ifelse(nchar(df$egn, type = "chars", 
  allowNA = FALSE, keepNA = NA)==8,as.numeric(
  substring(df$date_entry,1,4))-2000, df$age))
df$age <- ifelse(df$age>90 | df$age<18,NA,df$age)
df$total_income <- as.numeric(df$total_income)
df$total_income <- ifelse(is.na(df$total_income),NA,
  ifelse(df$total_income<100,NA,df$total_income))
df$ratio_installment_income <- as.numeric(df$ratio_installment_income)
df$ratio_installment_income <- ifelse(is.na(df$total_income),NA,
  ifelse(df$ratio_installment_income>4,NA,df$ratio_installment_income))
df$default_previous <- ifelse(is.na(df$max_delay),NA,df$default_previous)
df$refinance_previous <- ifelse(is.na(df$max_delay),NA,df$refinance_previous)
df$refinance_cum <- ifelse(is.na(df$max_delay),NA,df$refinance_previous)
df <- subset(df,!is.na(df$age))
df <- subset(df,!is.na(df$ratio_installment_income))

# Give name to selected products
df$product_name <- ifelse(df$product_id==1,"City Month",
                   ifelse(df$product_id %in% c(5,11,23,24),"City Week",
                   ifelse(df$product_id==6,"City 2-Week",
                   ifelse(df$product_id==7,"Pensioner",
                   ifelse(df$product_id==8,"VIP Week",
                   ifelse(df$product_id %in% c(30,31,32,33,35),"0%",
                   ifelse(df$product_id %in% c(9,25,26,27,28,36,37),"Credirect",
                   "Other")))))))

df_output <- df[ , -which(names(df) %in% c("id","signed_at","deactivated_at",
 "sub_status","updated_at","date_entry","office_id",
 "request_number","source","ckr_count","ckr_units","ckr_delay_sum", 
 "default_flag","ckr_left","ckr_amount","income","additional_income",
 "pay_day", "online_cum","offline_cum","income_corr","additional_income_corr",
 "office_current","office_id", "cession", "cession_cum","zone_id",
 "default_abs","zone_current","return_rate_cut","time_window",
 "last_pay_date_before_def","offline","online"))]



#############################
########## Output ###########
#############################

setwd("C:\\Projects\\LGD_model\\data\\")
write.csv(df_output,"all_data_buckets_v4.csv")



########################################
#### Analysis on return rate ###########
########################################

bins <- c(-1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99,1)
return_rate_binned <- cut(df_output$return_rate,bins)
table(return_rate_binned)


#########################
######### END ###########
#########################
