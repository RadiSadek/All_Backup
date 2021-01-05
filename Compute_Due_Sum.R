

#############################################
### BUILD LINEAR MODEL BETWEEN PROVISIONS ###
###                 AND                   ###
###       INCREASE IN SUM OF DELAYS       ###
#############################################



############## READ DATA ####################

# Read libraries
suppressWarnings(suppressMessages(library(openxlsx)))
suppressWarnings(suppressMessages(library(vars)))
suppressWarnings(suppressMessages(library(RMySQL)))

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)

# Define useful functions
agg_month_receiv <- function(input,var,beg,end,name){
  result <- as.data.frame(
    aggregate(var$total_receivables_end[var$days_delay>beg & 
      var$days_delay<=end], 
       by=list(var$month[var$days_delay>beg & var$days_delay<=end]),FUN=sum))
  names(result) <- c("month",name)
  return(merge(input,result, by.x = "month", by.y = "month",
               all.x = TRUE))
}
agg_month_distr_pop <- function(input,var,dpd){
  result <- merge(input, var[var$dpd_bin==dpd,],
                  by.x = "month", by.y = "month", all.x = TRUE)
  names(result)[ncol(result)] <- c(paste("dpd_",dpd,sep=""))
  result <- result[,-c(ncol(result)-1)]
  return(result)
}

# Read past accounting files
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
info <- df[,c("credit_number","online_offline","id","deactivated_at",
            "total_amount","date","amount","sub_status")]
load(paste("C:\\Projects\\Ad_Hoc\\Budget_Previsionning\\",
           "Relation_Provisions_DPDs\\input\\",
           "all_accountings_2016_to_2020.rdata",sep=""))
df <- final
df <- df[,-which(names(df) %in% c("amount","deactivated_at"))]
df <- merge(df, info, by.x = "credit_number", by.y = "credit_number",
            all.x = TRUE)

# Read correct offices
offices <- read.xlsx(paste(
  "C:\\Projects\\Ad_Hoc\\Budget_Previsionning\\",
  "Relation_Provisions_DPDs\\input\\correct_offices.xlsx",sep=""))
offices$office <-  gsub('"', '', offices$office)

# Work on City Cash solely for now
df <- subset(df,df$online_offline=="offline")
df$office <-  gsub('"', '', df$office)

# Get office id
offices_id <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id AS office_id, name AS office_name
FROM citycash_db.structure_offices")), n=-1)
Encoding(offices_id$office_name) <- "UTF-8"
offices_id$office_name <-  gsub('"', '', offices_id$office_name)
df <- merge(df,offices_id,by.x = "office",by.y = "office_name",
            all.x = TRUE)

# Check office transfers
office_transfers <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM citycash_db.credits_applications_transfers")), n=-1)



############# COMPUTE REPAYMENTS

# Join slujebni pogashenia
repay <- read.xlsx(paste("C:\\Projects\\Ad_Hoc\\Budget_Previsionning\\",
  "Relation_Provisions_DPDs\\input\\income_expenses.xlsx",sep=""))
repay <- merge(repay,info[,c("credit_number","online_offline")],
               by.x = "credit_number",by.y = "credit_number",all.x = TRUE)
repay <- repay[repay$online_offline=="offline",]
repay$repay_amount <- as.numeric(repay$repay_amount)

agg_repay <- aggregate(repay$repay_amount,by=list(repay$month),FUN=sum)
names(agg_repay) <- c("month","repaid_amount")



############# READ DATA FROM DB

# Get credits plan main
main <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id,application_id,pay_day 
FROM citycash_db.credits_plan_daily;")), n=-1)

# Get maturing tases
maturing_takes <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM citycash_db.credits_plan_balance_maturing_takes")), n=-1)

# Merge everything
main <- main[main$application_id %in% df$id,]
main_big <- merge(main,maturing_takes,by.x = "id",by.y = "daily_id",
                  all.x = TRUE)
main_big$delayed_sum <- main_big$principal + main_big$interest + 
  main_big$penalty

# Remove dataframes to free up memory
rm(main)
rm(maturing_takes)

# Free up main big
main_big <- main_big[main_big$application_id %in% df$id,]



############# CREATE FUNCTIONS FOR COMPUTING DELAYED SUMS

gen_delayed_sum <- function(input,date){
  main_local <- main_big[main_big$application_id %in% input$id,]
  main_local <- subset(main_local,main_local$pay_day<date)
  main_local <- main_local[rev(order(main_local$pay_day)),]
  main_local <- main_local[order(main_local$application_id),]
  main_local <- main_local[!duplicated(main_local$application_id),]
  #return(sum(main_local$delayed_sum,na.rm = TRUE))
  return(main_local)
}

gen_office_at_time <- function(input,date){
  office_transfers_local <- subset(office_transfers,
      office_transfers$created_at<date)
  
  office_transfers_local <- office_transfers_local[
    rev(order(office_transfers_local$created_at)),]
  office_transfers_local <- office_transfers_local[
    order(office_transfers_local$application_id),]
  office_transfers_local_last <- office_transfers_local[
   !duplicated(office_transfers_local$application_id),]
  
  office_transfers_all <- office_transfers
  office_transfers_all <- office_transfers_all[
    order(office_transfers_all$created_at),]
  office_transfers_all <- office_transfers_all[
    order(office_transfers_all$application_id),]
  office_transfers_all_first <- office_transfers_all[
    !duplicated(office_transfers_all$application_id),]
  
  input_here <- merge(input,
     office_transfers_local_last[,c("application_id","new_office_id")],
     by.x = "id",by.y = "application_id",all.x = TRUE)
  input_here <- merge(input_here,
     office_transfers_all_first[,c("application_id","old_office_id")],
     by.x = "id",by.y = "application_id",all.x = TRUE)
  input_here$real_office_id <- 
    ifelse(!is.na(input_here$new_office_id),input_here$new_office_id,
    ifelse(!is.na(input_here$old_office_id),input_here$old_office_id,
           input_here$office_id))
  return(input_here)
}



############# COMPUTE DELAYED SUMS PER MONTH

all_months <- as.data.frame(unique(sort(df$month)))
names(all_months) <- c("month")
all_months <- merge(all_months,aggregate(as.numeric(substring(df$date,9,10)),
  by=list(substring(df$date,1,7)),FUN=max),by.x = "month",by.y = "Group.1",
  all.x = TRUE)
names(all_months)[2] <- c("days_in_month")
all_months$delayed_sum <- NA
last_row <- as.data.frame(cbind("2020-10","31",NA))
names(last_row) <- names(all_months)
all_months <- rbind(all_months,last_row)

for(i in 2:(nrow(all_months))){
  
  first <- all_months$month[i-1]
  second <- all_months$month[i]

  date_first <- paste(all_months$month[i],"01",sep = "-")  
  date_second <- paste(all_months$month[i+1],"01",sep = "-")
  
  input1 <- df[df$month==first,]
  input2 <- df[df$month==second,]
  
  input1 <- gen_office_at_time(input1,date_first)
  input2 <- gen_office_at_time(input2,date_second)
  
  input2 <- input2[input2$real_office_id %in% offices$office_id,]
  input1 <- input1[input1$credit_number %in% input2$credit_number,]
  
  all_months$delayed_sum[i] <- gen_delayed_sum(input2,date_second) - 
   gen_delayed_sum(input1,date_first)
}

# Join monthly repayments
all_months$delayed_sum <- round(as.numeric(all_months$delayed_sum),2)




############# OUTPUT RESULT

all_months <- all_months[,c(1,3)]
write.xlsx(all_months,paste("C:\\Projects\\Provisions_Modeled\\data\\",
  "provisions_and_delayed_sums\\Delayed_Sums.xlsx",sep=""))






