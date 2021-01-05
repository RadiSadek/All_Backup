

######################################################################
# GET CUSTOMER LIFETIME VALUE BASED ON MONTH OF FIRST CREDIT LENDING #
######################################################################


# Choose end date of study (not included)
end_date <- "2020-05-01"

# Load libraries
library(openxlsx)
library(RMySQL)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Load data
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
                 host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

data_sql2 <- suppressWarnings(dbSendQuery(con, 
"SELECT object_id, amount, pay_date 
FROM citycash_db.cash_flow
WHERE nomenclature_id in (90,100,101) 
AND deleted_at IS NULL AND object_type=4"))
df_raw <- df
paid <- fetch(data_sql2, n=-1)
df3 <- df[,c("id","egn","credit_number","status")]

# Merge to final dataframe
paid <- merge(paid, df3, by.x = "object_id", 
              by.y = "id",
              all.x = TRUE)
plan_main_sql <- suppressWarnings(dbSendQuery(con, "SELECT * FROM 
              citycash_db.credits_plan_main"))
plan_main <- fetch(plan_main_sql, n=-1)

# Get only Credirect and set other fields
df <- subset(df, df$online_offline=="online")
df <- subset(df, df$date<end_date)
paid <- subset(paid, paid$pay_date<end_date)
plan_main <- plan_main[plan_main$application_id  %in% df$id,]

# Compute fields
df$month <- substring(df$date,1,7)

# Get first EGN per client
df <- df[order(df$month),]
df <- df[order(df$egn),]
egn  <- df[!duplicated(df$egn),][,c("egn","month")]
names(egn)[ncol(egn)] <- "month_entry"

# Get all possible months and set paids correctly
paid <- paid[paid$credit_number %in% df$credit_number,]
names <- t(names(table(df$month)))
paid$pay_month <- substring(paid$pay_date,1,7)
paid_agg <- aggregate(paid$amount, by=list(paid$egn, paid$pay_month), FUN=sum)
paid_agg$id <- 1:nrow(paid_agg)
df_dates <- data.frame(matrix(ncol=ncol(names), nrow=nrow(paid_agg)))
colnames(df_dates) <- names
paid_agg <- cbind(paid_agg,df_dates)
names(paid_agg)[1:4] <- c("egn","pay_month","amount","id")

# Distribute payment into subsequent month
for(i in (5:(ncol(paid_agg)))){
  paid_agg[,i] <- ifelse(names(paid_agg)[i]==paid_agg$pay_month[paid_agg$id], 
                         paid_agg$amount[paid_agg$id], 0)
}

# Aggregate for each unique EGN
for (i in 5:ncol(paid_agg)){
  if (i==5){
    final_agg <- merge(egn, paid_agg[paid_agg[,c(names(paid_agg)[i])]>0,
                       c("egn",names(paid_agg)[i])], 
                       by.x = "egn", by.y = "egn", all.x = TRUE)} 
  else{
    final_agg <- merge(final_agg, paid_agg[paid_agg[,c(names(paid_agg)[i])]>0, 
                       c("egn",names(paid_agg)[i])], 
                       by.x = "egn", by.y = "egn", all.x = TRUE)}
}
final_agg[, 3:ncol(final_agg)][is.na(final_agg[, 3:ncol(final_agg)])] <- 0

# Set Final CLTV column
final_agg$sum <- rowSums(final_agg[,c(3:ncol(final_agg))])

# Get aggregate of total amount lended per EGN
total_amount <- aggregate(df$amount, by=list(df$egn), FUN=sum)
names(total_amount) <- c("egn","total_amount")
final_agg <- merge(final_agg, total_amount, 
                   by.x = "egn", by.y = "egn", all.x = TRUE)

# Get aggregate of total amount with installments which has passed
plan_main <- subset(plan_main, plan_main$pay_day<end_date)
plan_main <- merge(plan_main, df[,c("id","egn")], 
                   by.x = "application_id", by.y = "id", 
                   all.x = TRUE)
total_amount_passed <- aggregate(plan_main$principal, 
                                 by=list(plan_main$egn), FUN=sum)
names(total_amount_passed) <- c("egn","total_amount_passed")
final_agg <- merge(final_agg, total_amount_passed, by.x = "egn", 
                   by.y = "egn", all.x = TRUE)

# Make final adjustments
final_agg$total_amount_passed <- ifelse(
    is.na(final_agg$total_amount_passed), 0, 
    final_agg$total_amount_passed)

# Make function to compute cltv up to a certain period
cltv_Ndays <- function(df, period, days){
  for (i in 1:nrow(df)){
    df$col_number[i] <- which(colnames(df)==df$month_entry[i])
    df$days_end[i] <- df$col_number[i] + period
    df$days_end[i] <- ifelse(df$days_end[i]>(length(names)+2), 
                             (length(names)+2), 
                             df$days_end[i])
    df[[paste("paid_",days, sep="")]][i] <- ifelse(
      df$days_end[i]==df$col_number[i], 
      df[i,df$col_number[i]],
      rowSums(df[i,c(df$col_number[i]:df$days_end[i])]))
    
    plan_main_subs <- subset(plan_main, plan_main$egn==df$egn[i] & 
        plan_main$pay_day<=paste(names(final_agg)[df$days_end[i]],"-31", 
                                 sep=""))
    
    df[[paste("passed_principal",days, sep="")]][i] <- 
      sum(plan_main_subs$principal)
  }
  return(df)
}

# Get amount paid during several
final_agg <- cltv_Ndays(final_agg, 1, 30)
final_agg <- cltv_Ndays(final_agg, 2, 60)
final_agg <- cltv_Ndays(final_agg, 3, 90)
final_agg <- cltv_Ndays(final_agg, 4, 120)
final_agg <- cltv_Ndays(final_agg, 6, 180)
final_agg <- cltv_Ndays(final_agg, 8, 240)
final_agg <- cltv_Ndays(final_agg, 10, 300)
final_agg <- cltv_Ndays(final_agg, 12, 360)

# Compute total cltv
final_agg$cltv_total30 <- final_agg$paid_30 - final_agg$passed_principal30
final_agg$cltv_total60 <- final_agg$paid_60 - final_agg$passed_principal60
final_agg$cltv_total90 <- final_agg$paid_90 - final_agg$passed_principal90
final_agg$cltv_total120 <- final_agg$paid_120 - final_agg$passed_principal120
final_agg$cltv_total180 <- final_agg$paid_180 - final_agg$passed_principal180
final_agg$cltv_total240 <- final_agg$paid_240 - final_agg$passed_principal240
final_agg$cltv_total300 <- final_agg$paid_300 - final_agg$passed_principal300
final_agg$cltv_total360 <- final_agg$paid_360 - final_agg$passed_principal360

# Append column with type of first product
df <- df_raw
df <- subset(df, df$online_offline=="online")
df <- subset(df, df$status %in% c(4,5))
df <- df[order(df$date),]
df <- df[order(df$egn),]
df <- df[!duplicated(df$egn),]
final_agg <- merge(final_agg,df[,c("egn","product_name")],
                   by.x = "egn",by.y = "egn",all.x = TRUE)

# Output results
write.xlsx(final_agg, "C:\\Projects\\Customer_LTV\\results\\results.xlsx")






