
# Library
library(RMySQL)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

gen_big_dataframe <- function(db_user,db_password,db_name,db_host,df_port){
  
  con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                   dbname=db_name, host=db_host, port = df_port)
  
  df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT 
  id, client_id, SUBSTRING(signed_at,1,10) AS date, credit_number, 
  has_prev_credits, has_prev_applications, has_prev_company_credits, 
  product_id, status, sub_status, 
  signed_at, deactivated_at, updated_at, created_at 
  AS date_entry, office_id, number AS request_number, source, 
  IF(product_id IN (55,58,57,56,9,48,25,36,41,43,50,28,26,37,42,44,49,27),
  'online', 'offline') AS online_offline
  FROM citycash_db.credits_applications a 
  WHERE status IN (1,2,3,4,5);")), n=-1)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, amount, installments
  FROM citycash_db.credits_plan_contract")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, egn, ownership, education, marital_status, 
  household_children, household_total, on_address
  FROM citycash_db.credits_applications_clients")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, SUM(amount) AS income 
  FROM citycash_db.credits_applications_clients_money_income 
  WHERE type=1 AND sub_type=1 AND deleted_at IS NULL 
  GROUP BY application_id")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, SUM(amount) AS additional_income 
  FROM citycash_db.credits_applications_clients_money_income 
  WHERE deleted_at IS NULL AND type<>1 OR sub_type<>1
  GROUP BY application_id")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, MAX(days_delay) AS days_delay
  FROM citycash_db.credits_plan_main 
  GROUP BY application_id")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT id, period
  FROM citycash_db.products")), n=-1)
  df <- merge(df,next_df,by.x = "product_id",by.y = "id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT object_id, SUM(amount) AS amount_paid 
  FROM citycash_db.cash_flow
  WHERE nomenclature_id in (90,100,101,102) 
  AND deleted_at IS NULL AND object_type=4
  GROUP BY object_id")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "object_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, MAX(pmt_final) AS installment_amount 
  FROM citycash_db.credits_plan_main
  GROUP BY application_id")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, experience, 
  experience_employer, status AS status_work
  FROM citycash_db.credits_applications_clients_work")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, purpose
  FROM citycash_db.credits_applications_data_other")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT application_id, max(pay_day) AS max_pay_day 
  FROM citycash_db.credits_plan_main 
  GROUP BY application_id")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- dbSendQuery(con, 'set character set "utf8"')
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT id, name AS office_current, zone_id 
  FROM citycash_db.structure_offices"),), n=-1,)
  df <- merge(df,next_df,by.x = "office_id",by.y = "id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT id, name AS zone_current
  FROM citycash_db.structure_zones")), n=-1)
  Encoding(next_df$zone_current) <- "UTF-8"
  df <- merge(df,next_df,by.x = "zone_id",by.y = "id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT final_credit_amount AS total_amount, application_id 
   FROM citycash_db.credits_plan_contract")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT object_id, max(pay_date) AS last_pay_date 
  FROM citycash_db.cash_flow
  WHERE nomenclature_id IN (90,100,101) 
  AND deleted_at IS NULL AND object_type = 4
  GROUP BY object_id")), n=-1)
  df <- merge(df,next_df,by.x = "id",by.y = "object_id",all.x = TRUE)
  
  next_df <- fetch(suppressWarnings(dbSendQuery(con, 
  "SELECT id, name AS product_name
  FROM citycash_db.products")), n=-1)
  df <- merge(df,next_df,by.x = "product_id",by.y = "id",all.x = TRUE)
  
  Encoding(df$office_current) <- "UTF-8"
  Encoding(df$zone_current) <- "UTF-8" 
  Encoding(df$product_name) <- "UTF-8"
  
  df$income <- ifelse(is.na(df$income),0,df$income)
  df$additional_income <- ifelse(is.na(df$additional_income),0,
                                 df$additional_income)
  df$total_income <- df$income + df$additional_income
  df$ratio_installment_income <- ifelse(df$total_income==0,NA,
        ifelse(df$period==3, df$installment_amount/df$total_income,
        ifelse(df$period==2, df$installment_amount/(df$total_income*14/30),
               df$installment_amount/(df$total_income*7/30))))
  df$maturity <- ifelse(df$period==1, df$installments*7/30,
                 ifelse(df$period==2, df$installments*14/30, 
                        df$installments))
  df$default_flag <- ifelse(df$days_delay>=90, 1, 0)
  df$gender <- ifelse(substring(df$egn,9,9) %in% c(0,2,4,6,8), 1, 0)
  
  if(!(substring(df$egn,3,3) %in% c("5","4")) & 
     (substring(df$egn,1,2) %in% c("00","01","02","03","04","05","06"))){
    df$dob <- NA} else {
    df$dob <- 
        ifelse(as.character(substring(df$egn,1,2)) %in% 
                 c("00","01","02","03","04","05","06"),
        paste("20",substring(df$egn,1,2),"-",
              (as.numeric(substring(df$egn,3,3))-4),
              substring(df$egn,4,4),"-",
              substring(df$egn,5,6),sep=""),
        ifelse(as.character(substring(df$egn,1,2)) %in% c("10","11"),
               NA,
        paste("19",substring(df$egn,1,2),"-",
              substring(df$egn,3,4),"-",
              substring(df$egn,5,6), sep="")))
    }
   
  df$dob <- as.Date(df$dob)  
  df$date_entry <- as.Date(df$date_entry) 
  df$age <- ifelse(is.na(df$dob), 18, 
                   floor(as.numeric(difftime(df$date_entry, df$dob, 
                   units=c("days"))/365.242)))

  return(df)
  
}

df <- suppressWarnings(gen_big_dataframe(db_user,db_password,db_name,
                                         db_host,df_port))
save(df,
  file="C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

