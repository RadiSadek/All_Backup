

##################################################
## PREPARE INPUT FILE FOR CITY CASH APPLICATION ##
##################################################


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
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
data_encoding <- suppressWarnings(dbSendQuery(con,'set character set "utf8"'))

data_ckr <- fetch(suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, client_id, created_at
FROM citycash_db.clients_ckr_files"))), n=-1)

data_ckr2 <- fetch(suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT * 
FROM citycash_db.clients_ckr_files_data"))), n=-1)

ckr <- suppressWarnings(merge(data_ckr, data_ckr2, by.x = "id", 
  by.y = "file_id", all.x = TRUE))

data_app <- fetch(suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT * 
FROM citycash_db.credits_applications"))), n=-1)

first_office <- fetch(suppressWarnings(dbSendQuery(con, "SELECT 
citycash_db.credits_applications.id,
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
citycash_db.structure_zones.id")), n=-1)
Encoding(first_office$name) <- "UTF-8"
Encoding(first_office$zone_name) <- "UTF-8"

offices <- fetch(suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, name AS office_name, city_id, latitude, longitude, self_approve
FROM citycash_db.structure_offices"))), n=-1)
Encoding(offices$office_name) <- "UTF-8"

cities <- fetch(suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, name, municipality_id AS city_office, population
FROM citycash_db.cities"))), n=-1)
Encoding(cities$name) <- "UTF-8"

offices <- merge(offices,cities,by.x = "city_id",by.y ="id",all.x = TRUE) 

municipalities <- fetch(suppressMessages(suppressWarnings(dbSendQuery(con, 
"SELECT id, area_id, name AS municipality_name
FROM citycash_db.municipalities"))), n=-1)
Encoding(municipalities$municipality_name) <- "UTF-8"
municipalities$municipality_name <- ifelse(
  municipalities$municipality_name=="????" & municipalities$area_id==3,
  "????_?????",municipalities$municipality_name)


########## Read data and set fields ###########

# Main dataframe
setwd("C:\\Projects\\Application_Scoring\\CityCash_v4\\data\\")

# Subset some more
df <- subset(df,df$status %in% c(4,5))
df <- subset(df,df$online_offline=="offline")
df <- subset(df,df$has_prev_credits==0)

# Correct some fields
df$month <- substring(df$date, 1, 7)
df$amount_paid <- ifelse(is.na(df$amount_paid), 0, df$amount_paid)

# Amount paid as percentage
df$profit <- df$amount_paid/df$amount

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
df <- subset(df, df$sub_status!=122 | is.na(df$sub_status))
df <- subset(df, df$date<"2020-10-06" & df$date>"2018-04-10")

# Correct age, total_income, ratio_installment_income
df$total_income <- ifelse(df$total_income<100, NA, df$total_income)
df$total_income <- ifelse(df$total_income>8000, 8000, df$total_income)
df$ratio_installment_income <- ifelse(df$ratio_installment_income>4, 4, 
                                      df$ratio_installment_income)




########## Work on CKR data ###########

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




########## Work on other types of application variables ###########

data_sql <- suppressWarnings(dbSendQuery(con, 
"SELECT application_id, old_office_id, new_office_id, created_at
FROM citycash_db.credits_applications_transfers
"))
office <- fetch(data_sql, n=-1)
office_old <- subset(office,!is.na(office$old_office_id))
office_new <- subset(office,!is.na(office$new_office_id))
office_old <- office_old[order(office_old$created_at),]
office_old <- office_old[order(office_old$application_id),]
office_old <- office_old[!duplicated(office_old$application_id),]
office_new <- office_new[order(office_new$created_at),]
office_new <- office_new[order(office_new$application_id),]
office_new <- office_new[!duplicated(office_new$application_id),]
office_all <- merge(office_old,office_new,by.x = "application_id",
    by.y = "application_id",all.x = TRUE)
office_all$first_office_id <- ifelse(is.na(office_all$old_office_id.x),
   office_all$new_office_id.y,office_all$old_office_id.x)
df <- merge(df,office_all[,c("application_id","first_office_id")],
   by.x = "id",by.y = "application_id",all.x = TRUE)
df$first_office_id <- ifelse(is.na(df$first_office_id),df$office_id,
   df$first_office_id)
data_sql2 <- suppressWarnings(dbSendQuery(con, 
"SELECT id, name AS first_office_name
FROM citycash_db.structure_offices"))
structure_offices <- fetch(data_sql2, n=-1)
Encoding(structure_offices$first_office_name) <- "UTF-8"
df <- merge(df,structure_offices,by.x = "first_office_id",by.y = "id",
            all.x = TRUE)

# Get if self approval
df$month2 <- paste(substring(df$created_at,1,4),
                   as.numeric(substring(df$created_at,6,7)),sep="-")
self_approvals <- read.xlsx(
  "C:\\Regular\\R\\Vintage\\self_approval_offices.xlsx")
names(self_approvals)[3] <- "self_approval"
df <- merge(df,self_approvals,by.x = c("first_office_name","month2"),
            by.y = c("office","date"),all.x = TRUE)
df$self_approval <- ifelse(is.na(df$self_approval),0,df$self_approval)
 
# Join Risky location
risky_loc <- read.xlsx(
"C:\\Projects\\Geo_Segmentation\\result_final\\EGN_and_flag_risk_location.xlsx")
df <- merge(df,risky_loc,by.x = "client_id",by.y = "client_id",all.x = TRUE)
names(df)[ncol(df)] <- "flag_location_curr"
risky_loc <- read.xlsx(paste(
  "C:\\Projects\\Geo_Segmentation\\result_final\\",
  "EGN_and_flag_risk_location_Official_Address.xlsx",sep=""))
df <- merge(df,risky_loc,by.x = "client_id",by.y = "client_id",all.x = TRUE)
names(df)[ncol(df)] <- "flag_location_offi"
distance_loc <- read.xlsx(paste(
  "C:\\Projects\\Geo_Segmentation\\result_final\\",
  "EGN_and_distance_addresses.xlsx",sep=""))
df <- merge(df,distance_loc,by.x = "client_id",by.y = "client_id",all.x = TRUE)
names(df)[ncol(df)] <- "distance_curr_offi"






#### Get SAME CITY OF OFFICIAL AND CURRENT ADDRESS

# Read addresses
address <- suppressWarnings(fetch(dbSendQuery(con, 
"SELECT * FROM addresses
WHERE addressable_type='App\\\\Models\\\\Credits\\\\Applications\\\\Application'"), 
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
            by.x = "id", 
            by.y = "addressable_id", all.x = TRUE)
names(df)[ncol(df)] <- "city_official"
df <- merge(df, 
            address[address$type==2,c("addressable_id","city")], 
            by.x = "id", 
            by.y = "addressable_id", all.x = TRUE)
names(df)[ncol(df)] <- "city_current"
df$same_city <- ifelse(df$city_current==df$city_official,1,0)






########## Output data ###########

# Order by date
df <- df[order(df$date),]

# Output data 
setwd("C:\\Projects\\Application_Scoring\\CityCash_v4\\data\\")
write.csv(df,"input.csv")



