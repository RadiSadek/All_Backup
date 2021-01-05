
########## GENERATE VINTAGE REPORT ############


# Choose end date of study (not included) for profit analysis
end_date <- "2020-11-01"


# Get libraries
library(openxlsx)
library(RMySQL)
library(lubridate)
library(zoo)


# Database specifications
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306


# Define SQL queries
sql_query1 <- "SELECT credit_number, id, 
product_id, maturity 
FROM test.data_final WHERE status IN (4,5)"
sql_query2 <- "SELECT id, name FROM citycash_db.products"
sql_query3 <- "SELECT 
citycash_db.credits_applications.credit_number,
citycash_db.credits_applications.product_id, 
citycash_db.credits_applications.date,
citycash_db.credits_applications_transfers.old_office_id,
citycash_db.credits_applications_transfers.created_at,
citycash_db.structure_offices.name,
citycash_db.structure_zones.name as zone_name
FROM citycash_db.credits_applications
INNER JOIN citycash_db.credits_applications_transfers 
ON citycash_db.credits_applications.id = 
citycash_db.credits_applications_transfers.application_id
INNER JOIN citycash_db.structure_offices 
ON citycash_db.credits_applications_transfers.old_office_id = 
citycash_db.structure_offices.id
LEFT JOIN citycash_db.structure_zones 
ON citycash_db.structure_offices.zone_id = 
citycash_db.structure_zones.id"
sql_query4 <- "SELECT application_id,score,amount,period 
FROM citycash_db.credits_applications_scoring"


# Read and make connection
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, 
                 host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
data <- df
data_raw <- data
data <- data[ , -which(names(data) %in% c("id"))]
data_sql2 <- suppressWarnings(dbSendQuery(con, 'set character set "utf8"'))
data_sql2 <- suppressWarnings(dbSendQuery(con, sql_query2))
products <- fetch(data_sql2, n=-1)
Encoding(products$name) <- "UTF-8" 


# Select product_name
data <- merge(data, products, by.x = "product_id", by.y = "id", all.x = TRUE)


# Select first office if movement
data_sql3 <- suppressWarnings(dbSendQuery(con, sql_query3))
first_office <- fetch(data_sql3, n=-1)
Encoding(first_office$name) <- "UTF-8"
Encoding(first_office$zone_name) <- "UTF-8"
first_office <- first_office[order(first_office$created_at),]
first_office <- first_office[order(first_office$credit_number),]
first_office <- first_office[!duplicated(first_office$credit_number),]


# Define functions
gen_last_day_month <- function(date){
  year <- substring(date,7,10)
  month <- substring(date,4,5)
  end_day <- ifelse(month=="02","28",
  ifelse(month %in% 
  c("01","03","05","07","08","10","12"),"31","30"))
  end_day <- ifelse(month=="02" & year %in% 
  c("2004","2008","2012","2016","2020","2024","2028","2032"), "29", end_day)
  return(paste(year,month,end_day, sep="-"))
}

gen_field <- function(i,days,flag){
  if(flag==0){
    result <- ifelse(as.Date(
  gen_last_day_month(v_int$Дата.на.отпускане))+days<=
    as.Date(first_month), v_int[,i], 0)} 
  else {
    result <- ifelse(
  as.Date(gen_last_day_month(v_int$Дата.на.отпускане))+
    days<as.Date(first_month), v_int[,i], 0)}
  return(result)}


# Read data
setwd("C:\\Regular\\R\\Vintage\\")
v_int <- read.xlsx("vintage_input.xlsx")
zones <- read.xlsx("zones.xlsx") 


# set first of month
first_month <- floor_date(as.Date(Sys.time()), "month")


# Filter data with irrelevant date
v_int$date <- as.Date(paste(substring(v_int$Дата.на.отпускане,7,10),
    substring(v_int$Дата.на.отпускане,4,5),
    substring(v_int$Дата.на.отпускане,1,2), sep="-"))
v_int <- subset(v_int, v_int$date<first_month)
v_int <- v_int[ , -which(names(v_int) %in% c("date"))]


# Compute columns
v_int$month <- as.numeric(substring(v_int[,2],4,5))
v_int$year <- as.numeric(substring(v_int[,2],7,10))

v_int$m30 <- gen_field(8,29,0)
v_int$r30 <- gen_field(9,29,0)
v_int$m60 <- gen_field(10,59,0)
v_int$r60 <- gen_field(11,59,0)
v_int$m90 <- gen_field(12,89,0)
v_int$r90 <- gen_field(13,89,0)
v_int$m120 <- gen_field(14,119,0)
v_int$r120 <- gen_field(15,119,0)
v_int$m180 <- gen_field(16,179,0)
v_int$r180 <- gen_field(17,179,0)
v_int$m210 <- gen_field(18,209,0)
v_int$r210 <- gen_field(19,209,0)
v_int$m360 <- gen_field(20,359,0)
v_int$r360 <- gen_field(21,359,0)
v_int$m_o_360 <- gen_field(22,359,1)
v_int$r_o_360  <- gen_field(23,359,1)
v_int$empty <- NA


# Rearrange offices and zones
v_int <- merge(v_int, first_office[,c("credit_number","name")], 
               by.x = "Кредит.№", by.y = "credit_number", 
               all.x = TRUE)
v_int$Офис <- ifelse(is.na(v_int$name), v_int$Офис, v_int$name)
v_int <- v_int[ , -which(names(v_int) %in% c("name"))]
v_int <- merge(v_int, zones, by.x = "Офис", by.y = "office", 
               all.x = TRUE)
v_int <- merge(v_int, data[,c("credit_number","maturity","name")], 
               by.x = "Кредит.№", 
               by.y = "credit_number", all.x = TRUE)


# Rearrange fields 
v_int <- v_int[,c(1,3,4,5,2,6,7,8:41,42,43,45,44)]
names(v_int)[ncol(v_int)] <- "Матуритет"
names(v_int)[ncol(v_int)-1] <- "Тип продукт"
v_int$Матуритет <- round(v_int$Матуритет,0)


# Get self approval offices 
# self_offices <- read.xlsx("self_approval_offices.xlsx")
# v_int$date <- paste(v_int$year,v_int$month,sep="-")
# v_int <- merge(v_int,self_offices,by.x = c("Офис","date"),
#                  by.y = c("office","date"), all.x = TRUE)
# v_int <- v_int[!duplicated(v_int$`Кредит.№`),]
# v_int <- v_int[,c(3,4,5,6,1,7:ncol(v_int))]
# names(v_int)[ncol(v_int)] <- "Офис_самоодобрение"
# v_int$Офис_самоодобрение <- ifelse(is.na(v_int$Офис_самоодобрение),"Не","Да")


# Get scoring
scoring <- fetch(suppressWarnings(dbSendQuery(con,sql_query4)), n=-1)
df_score <- df[df$status %in% c(4,5),
    c("id","amount","installments","credit_number")]
df_score <- merge(df_score,scoring,by.x = c("id","amount","installments"),
    by.y = c("application_id","amount","period"),all.x = TRUE)
v_int <- merge(v_int,df_score,
               by.x = "Кредит.№", by.y = "credit_number",all.x = TRUE)
v_int <- v_int[ , -which(names(v_int) %in% c("id","amount","installments"))]


# Compute repeat customers for new company or new brand
df <- subset(df,df$status %in% c(4,5))
df <- subset(df,is.na(df$sub_status) | !(df$sub_status %in% c(129)))
df <- df[order(df$date),]
df <- df[order(df$egn),]
for(i in 2:nrow(df)){
  if(df$egn[i]==df$egn[i-1]){
    df$has_prev_cred[i] <- 1
  }
}
df_credirect <- subset(df, df$online_offline=="online")
df_credirect <- df_credirect[order(df_credirect$date),]
df_credirect <- df_credirect[order(df_credirect$egn),]
df_credirect$has_prev_cred_company <- 0
for(i in 2:nrow(df_credirect)){
  if(df_credirect$egn[i]==df_credirect$egn[i-1]){
    df_credirect$has_prev_cred_company[i] <- 1
  }
}
df_citycash <- subset(df, df$online_offline=="offline")
df_citycash <- df_citycash[order(df_citycash$date),]
df_citycash <- df_citycash[order(df_citycash$egn),]
df_citycash$has_prev_cred_company <- 0
for(i in 2:nrow(df_citycash)){
  if(df_citycash$egn[i]==df_citycash$egn[i-1]){
    df_citycash$has_prev_cred_company[i] <- 1
  }
}
df <- rbind(df_credirect, df_citycash)
df <- df[,c("credit_number","id",
            "online_offline","date",
            "has_prev_cred","has_prev_cred_company")]
df$Пореден_Марка <- ifelse(df$has_prev_cred_company==1,"Да","Не")
df$Пореден_Компания <- ifelse(df$has_prev_cred==1,"Да","Не")
v_int <- merge(v_int,df[,c("credit_number","Пореден_Марка","Пореден_Компания")],
  by.x = "Кредит.№",by.y = "credit_number",all.x = TRUE)


# Get passed padejirala suma
plan_main_sql <- suppressWarnings(dbSendQuery(con, "SELECT * FROM 
citycash_db.credits_plan_main"))
plan_main <- fetch(plan_main_sql, n=-1)
plan_main <- subset(plan_main, plan_main$pay_day<end_date)
plan_main <- merge(plan_main, df[,c("id","credit_number")], 
by.x = "application_id", by.y = "id", 
all.x = TRUE)
total_amount_passed <- aggregate(plan_main$principal, 
by=list(plan_main$credit_number), FUN=sum)
names(total_amount_passed) <- c("credit_number","Падежирала_сума")
v_int <- merge(v_int,total_amount_passed,
    by.x = "Кредит.№",by.y = "credit_number",all.x = TRUE)
v_int$Падежирала_сума <- ifelse(is.na(v_int$Падежирала_сума),0,
    v_int$Падежирала_сума)


# Output data 
write.xlsx(v_int, "vintage_output.xlsx")

