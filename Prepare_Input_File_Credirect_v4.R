


######## Libraries and functions #########

# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)

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
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
                 host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
data_encoding <- suppressWarnings(dbSendQuery(con,'set character set "utf8"'))

data_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT egn, email 
FROM citycash_db.clients")))
internet <- fetch(data_sql2, n=-1)

data_ckr_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, client_id, created_at FROM citycash_db.clients_ckr_files")))
data_ckr <- fetch(data_ckr_sql, n=-1)

data_ckr_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT * FROM citycash_db.clients_ckr_files_data")))
data_ckr2 <- fetch(data_ckr_sql2, n=-1)

ckr <- suppressWarnings(merge(data_ckr, data_ckr2, by.x = "id", 
            by.y = "file_id", all.x = TRUE))

df <- merge(df, internet, by.x = "egn", by.y = "egn", 
            all.x = TRUE)




########## Read data ###########

# Main dataframe
setwd("C:\\Projects\\Application_Scoring\\Credirect_v4\\data\\")
dir_old_phone <- "C:\\Projects\\Application_Scoring\\Credirect_v3\\data\\"

# Subset some more
df <- subset(df,df$status %in% c(4,5))
df <- subset(df,df$online_offline=="online")
df <- subset(df,df$has_prev_credits==0)

# Correct some fields
df$month <- substring(df$date, 1, 7)
df$amount_paid <- ifelse(is.na(df$amount_paid), 0, df$amount_paid)

# Amount paid as percentage
df$profit <- df$amount_paid/df$amount

# Product category 
df$product_cat <- ifelse(df$product_id %in% c(9,48) ,
             "Credirect_User", "Credirect_Flex")



########## Choose time window filter ###########

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
df <- subset(df, df$date>"2018-04-10")
df <- subset(df, df$difftime>=180)
df <- subset(df, df$sub_status!=129 | is.na(df$sub_status))
df <- subset(df, df$date<"2020-09-30")



########## Set filds and variables ###########

# Correct age, total_income, ratio_installment_income
df$total_income <- ifelse(df$total_income<100, NA, df$total_income)
df$total_income <- ifelse(df$total_income>8000, 8000, df$total_income)
df$ratio_installment_income <- ifelse(df$ratio_installment_income>4, 4, 
                                      df$ratio_installment_income)


# Work on CKR
data_small <- df[,c("id","date", "client_id")]
names(data_small)[1] <- "application_id"

# CKR financial
ckr_financial <- subset(ckr, ckr$type==2)
data_ckr_fin <- merge(data_small, ckr_financial, 
                      by.x = "client_id", by.y = "client_id")
data_ckr_fin$diff_time_ckr <- abs(difftime(data_ckr_fin$date, 
      data_ckr_fin$created_at, units=c("days")))
data_ckr_fin <- data_ckr_fin[with(data_ckr_fin, order(diff_time_ckr)), ]
data_ckr_fin <- data_ckr_fin[with(data_ckr_fin, order(application_id)), ]
data_ckr_fin <- data_ckr_fin[,c("application_id","cred_count",
        "source_entity_count","amount_drawn","monthly_installment",
        "current_status_active","status_active","status_finished",
        "outstanding_performing_principal","outstanding_overdue_principal")]
names(data_ckr_fin) <- c("application_id","cred_count_financial",
        "source_entity_count_financial","amount_drawn_financial",
        "monthly_installment_financial","current_status_active_financial",
        "status_active_financial","status_finished_financial",
        "outs_principal_financial","outs_overdue_financial")
data_ckr_fin <- data_ckr_fin[!duplicated(data_ckr_fin$application_id),]

# CKR bank
ckr_bank <- subset(ckr, ckr$type==1)
data_ckr_bank <- merge(data_small, ckr_bank, 
                       by.x = "client_id", by.y = "client_id")
data_ckr_bank$diff_time_ckr <- abs(difftime(data_ckr_bank$date, 
        data_ckr_bank$created_at, units=c("days")))
data_ckr_bank <- data_ckr_bank[with(data_ckr_bank, order(diff_time_ckr)), ]
data_ckr_bank <- data_ckr_bank[with(data_ckr_bank, order(application_id)), ]
data_ckr_bank <- data_ckr_bank[,c("application_id","cred_count",
        "source_entity_count","amount_drawn","monthly_installment",
        "current_status_active","status_active","status_finished",
        "outstanding_performing_principal","outstanding_overdue_principal")]
names(data_ckr_bank) <- c("application_id","cred_count_bank",
        "source_entity_count_bank","amount_drawn_bank",
        "monthly_installment_bank","current_status_active_bank",
        "status_active_bank","status_finished_bank",
        "outs_principal_bank","outs_overdue_bank")
data_ckr_bank <- data_ckr_bank[!duplicated(data_ckr_bank$application_id),]


# Merge CKR data
df <- merge(df, data_ckr_fin, by.x = "id", 
            by.y = "application_id", all.x = TRUE)
df <- merge(df, data_ckr_bank, by.x = "id", 
            by.y = "application_id", all.x = TRUE)

# Re-work CKR
df$status_active_financial <- ifelse(df$cred_count_financial==0, -1, 
                                     df$status_active_financial)
df$status_active_bank <- ifelse(df$cred_count_bank==0, -1, 
                                df$status_active_bank)
df$outs_overdue_ratio_bank <- ifelse(
  df$outs_overdue_bank==0 & df$outs_principal_bank==0, -999,
  df$outs_overdue_bank/df$outs_principal_bank)                 
df$outs_overdue_ratio_financial <- ifelse(
  df$outs_overdue_financial==0 & df$outs_principal_financial==0, -999,
  df$outs_overdue_financial/df$outs_principal_financial)

df$outs_overdue_ratio_bank <- ifelse(
  df$outs_overdue_ratio_bank>1, 1, df$outs_overdue_ratio_bank)
df$outs_overdue_ratio_financial <- ifelse(
  df$outs_overdue_ratio_financial>1, 1, df$outs_overdue_ratio_financial)

df$source_entity_count_total <- df$source_entity_count_bank + 
  df$source_entity_count_financial
df$amount_drawn_total <- df$amount_drawn_bank + 
  df$amount_drawn_financial
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
df$outs_overdue_ratio_total <- ifelse(
  df$outs_overdue_ratio_bank>df$outs_overdue_ratio_financial, 
  df$outs_overdue_ratio_bank, df$outs_overdue_ratio_financial)

# Order by date
df <- df[order(df$date),]




#### Get SEON MAIL variables

seon_mails <- read.xlsx(paste("C:\\Projects\\Application_Scoring\\",
  "Credirect_v4\\data\\SEON_after_assessment_FINAL_v2.xlsx",sep=""))
df <- merge(df,seon_mails,by.x = "egn",by.y = "egn",all.x = TRUE)



#### Get SEON PHONE variables

viber_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, 
paste("SELECT a.application_id, b.registered AS viber
FROM ",db_name,".seon_requests a
JOIN ",db_name,".seon_requests_accounts b
ON a.id=b.requests_id
WHERE a.type=1 AND b.type=7",sep=""))))
viber <- fetch(viber_sql,n=-1)

whatsapp_sql <- suppressMessages(suppressWarnings(dbSendQuery(con, 
paste("SELECT a.application_id, b.registered AS whatsapp
FROM ",db_name,".seon_requests a
JOIN ",db_name,".seon_requests_accounts b
ON a.id=b.requests_id
WHERE a.type=1 AND b.type=8",sep=""))))
whatsapp <- fetch(whatsapp_sql,n=-1)

df <- merge(df,viber,by.x = "id",by.y = "application_id",all.x = TRUE)
df <- merge(df,whatsapp,by.x = "id",by.y = "application_id",all.x = TRUE)







#### Get SAME CITY OF OFFICIAL AND CURRENT ADDRESS

# Read addresses
address <- suppressWarnings(fetch(dbSendQuery(con, 
"SELECT * FROM addresses
WHERE addressable_type='App\\\\Models\\\\Clients\\\\Client'"), 
n=-1))
cities <- suppressWarnings(fetch(dbSendQuery(con, "SELECT * FROM cities"), 
n=-1))
address <- merge(address, cities[,c("id","name","zip","municipality_id")], 
   by.x = "city_id", 
   by.y = "id", all.x = TRUE)
address <- address[,c("addressable_id","type","city_id","neighborhood_id",
   "name","zip","municipality_id","street", 
   "neighborhood_text",
   "block","number","entrance","floor","apartment")]
Encoding(address$name) <- "UTF-8"
Encoding(address$street) <- "UTF-8"
Encoding(address$neighborhood_text) <- "UTF-8"
Encoding(address$block) <- "UTF-8"
Encoding(address$number) <- "UTF-8"
Encoding(address$entrance) <- "UTF-8"

# Merge with neighborhood text
neighborhoods <- suppressWarnings(fetch(dbSendQuery(con, 
"SELECT id, name AS neighborhood_name FROM citycash_db.neighborhoods"),n=-1))
Encoding(neighborhoods$neighborhood_name) <- "UTF-8"
address <- merge(address, neighborhoods[,c("id","neighborhood_name")], 
  by.x = "neighborhood_id", 
  by.y = "id", all.x = TRUE)
address$neighborhood_text <- ifelse(is.na(address$neighborhood_text),
  address$neighborhood_name,address$neighborhood_text)

# Get area and municipality
areas <- suppressWarnings(fetch(dbSendQuery(con, 
                                            "SELECT id,name
FROM citycash_db.municipalities"),  n=-1))
municipalities <- suppressWarnings(fetch(dbSendQuery(con, 
"SELECT id,area_id,name  
FROM citycash_db.municipalities"),  n=-1))

address <- merge(address,municipalities,
  by.x = "municipality_id", by.y = "id",
  all.x = TRUE)
names(address)[ncol(address)] <- "municipality"
Encoding(address$municipality) <- "UTF-8"
address <- merge(address,areas,
                 by.x = "area_id", by.y = "id",
                 all.x = TRUE)
names(address)[ncol(address)] <- "area"
Encoding(address$area) <- "UTF-8"
colnames(address)[which(names(address) == "name.x")] <- "city"


# Make nice address format
address[address==""] <- NA
address$address_format <- paste(
  address$city,
  ifelse(is.na(address$neighborhood_text),
         "",
         paste(", ж.к. ",address$neighborhood_text)),
  ifelse(is.na(address$neighborhood_text),
         ifelse(is.na(address$street),"",paste(", ул. ",address$street)),
         ifelse(is.na(address$block),"",paste(", бл. ",address$block))),
  ifelse(is.na(address$neighborhood_text),
         ifelse(is.na(address$number),"",paste(", №",address$number)),
         ""),
  sep=""
)
address$address_format2 <- paste(
  ifelse(is.na(address$number) & is.na(address$block),"",
         ifelse(is.na(address$block),address$number,address$block)),
  ifelse(is.na(address$street) & is.na(address$neighborhood_text),"",
         ifelse(is.na(address$street),address$neighborhood_text,
                address$street)),
  address$city,
  sep=" "
)
address$address_format3 <- address$city
address <- address

# Merge addresses (only with current address)
df <- merge(df, 
    address[address$type==1,c("addressable_id","city")], 
    by.x = "client_id", 
    by.y = "addressable_id", all.x = TRUE)
names(df)[ncol(df)] <- "city_official"
df <- merge(df, 
    address[address$type==2,c("addressable_id","city")], 
    by.x = "client_id", 
    by.y = "addressable_id", all.x = TRUE)
names(df)[ncol(df)] <- "city_current"
df$same_city <- ifelse(df$city_current==df$city_official,1,0)





#### GET GEO SEGMENTATION 

# Join Risky location
risky_loc <- read.xlsx(
  "C:\\Projects\\Geo_Segmentation\\result_final\\EGN_and_flag_risk_location.xlsx")
df <- merge(df,risky_loc,by.x = "client_id",by.y = "client_id",all.x = TRUE)
names(df)[ncol(df)] <- "flag_location_curr"
risky_loc <- read.xlsx(paste(
  "C:\\Projects\\Geo_Segmentation\\result_final\\",
  "EGN_and_flag_risk_location_Official.xlsx",sep=""))
df <- merge(df,risky_loc,by.x = "client_id",by.y = "client_id",all.x = TRUE)
names(df)[ncol(df)] <- "flag_location_offi"
distance_loc <- read.xlsx(paste(
  "C:\\Projects\\Geo_Segmentation\\result_final\\",
  "EGN_and_distance_addresses.xlsx",sep=""))
df <- merge(df,distance_loc,by.x = "client_id",by.y = "client_id",all.x = TRUE)
names(df)[ncol(df)] <- "distance_curr_offi"



##### Get phone numbers for older time window  

# Merge variables
phone_result1 <- read.csv(paste(dir_old_phone,
 "SEON_phones_v1.csv",sep=""),sep=",")
phone_result2 <- read.csv(paste(dir_old_phone,
 "SEON_phones_v2.csv",sep=""),sep=",")
phone_result2 <- phone_result2[,-which(names(phone_result2) %in%
  c("telegram_registered"))]
phone_result <- rbind(phone_result1,phone_result2)
phone_result <- phone_result[!duplicated(phone_result$phone_number),]
phone_result <- phone_result[ , -which(names(phone_result) %in% 
   c("applied_rules"))]
phone_result$whatsapp.photo <- 
  ifelse(phone_result$whatsapp.photo=="",0,1)
phone_result$viber.photo <- 
  ifelse(phone_result$viber.photo=="",0,1)
phone_result$whatsapp.last_seen <- as.Date(
  as.POSIXct(phone_result$whatsapp.last_seen, origin="1970-01-01"))
phone_result$viber.last_seen <- as.Date(
  as.POSIXct(phone_result$whatsapp.last_seen, origin="1970-01-01"))
phone_result <- phone_result[ , -which(names(phone_result) %in% 
                                         c("country","success","type","valid"))]
phone_result$digits <- nchar(phone_result$phone_number)
phone_result$phone_number_corr <- paste("0",
    substring(phone_result$phone_number,4,12),sep="")
phone_numbers <- read.xlsx("all_phone_numbers.xlsx")
df <- merge(df,phone_numbers[,c("credit_number","pers_number_1")],
            by.x = "credit_number", by.y = "credit_number",
            all.x = TRUE)
df <- merge(df,phone_result[,c("phone_number_corr","whatsapp_registered",
   "viber_registered","facebook_registered","instagram_registered",
   "twitter_registered","google_registered")],
   by.x = "pers_number_1", by.y = "phone_number_corr",
   all.x = TRUE)

# Treat variables
df$viber_registered_bin <- ifelse(df$viber_registered=="True",1,
    ifelse(df$viber_registered=="False",0,NA))
df$viber <- ifelse(!is.na(df$viber),df$viber,df$viber_registered_bin)

df$whatsapp_registered_bin <- ifelse(df$whatsapp_registered=="True",1,
   ifelse(df$whatsapp_registered=="False",0,NA))
df$whatsapp <- ifelse(!is.na(df$whatsapp),df$whatsapp,
                      df$whatsapp_registered_bin)





########## Output ###########

# Make final checks
df <- df[!duplicated(df$credit_number),]

# Select only those after creation of scorecard
df <- subset(df,df$date>="2019-07-01")

# Get those with empty phone
empty_phones <- subset(df,is.na(df$facebook_registered))
not_empty <- subset(df,!is.na(df$facebook_registered))
empty_phones <- empty_phones[ , -which(names(empty_phones) %in% 
     c("pers_number_1"))]
empty_phones <- merge(empty_phones,
     phone_numbers[,c("credit_number","pers_number_1")],
     by.x = "credit_number", by.y = "credit_number",
     all.x = TRUE)
empty_phones <- empty_phones[,c("credit_number","pers_number_1")]
empty_phones <- empty_phones[!(empty_phones$pers_number_1 %in% 
    not_empty$pers_number_1),]
empty_phones <- empty_phones[!duplicated(empty_phones$pers_number_1),]
write.xlsx(empty_phones,"empty_phones.xlsx")


# Select output columns
all_cols <- c(
  
  "egn","credit_number","client_id","date","default_flag","product_cat",
  "amount_paid","amount",
  
  "age","maturity","gender","ratio_installment_income",
  "ownership","education","household_children","household_total","on_address",
  "experience_employer","marital_status", "purpose", "status_work",
  
  "status_active_total","status_finished_total","monthly_installment_total",
  "source_entity_count_total","amount_drawn_total","cred_count_total",
  "outs_overdue_ratio_total",
  names(df)[c(90:120)])
df <- df[,c(all_cols)]
df <- df[ , -which(names(df) %in% c("city_official","city_current",
  "whatsapp_registered","whatsapp_registered_bin",
  "viber_registered","viber_registered_bin"))]

# Output results\
write.csv(df,"input_current_v3.csv")


