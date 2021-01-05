
#################################################
######### Get all possible addresses ############
############## Mind the filters #################
#################################################


# Library
library(RMySQL)
library(openxlsx)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Read data
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
                 host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")


# Set query for first office
sql_query3 <- "SELECT 
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
ON citycash_db.structure_offices.zone_id = citycash_db.structure_zones.id;"
data_sql3 <- suppressWarnings(dbSendQuery(con, sql_query3))
first_office <- fetch(data_sql3, n=-1)
Encoding(first_office$name) <- "UTF-8"
Encoding(first_office$zone_name) <- "UTF-8"
first_office <- first_office[order(first_office$created_at),]
first_office <- first_office[order(first_office$id),]
first_office <- first_office[!duplicated(first_office$id),]
df <- merge(df, first_office[,c("id","name")], 
            by.x = "id", by.y = "id", all.x = TRUE)
names(df)[names(df) == "name"] <- "office_first"

# Subset data
df <- subset(df, df$online_offline=="offline" & df$status %in% c(4,5))
df <- subset(df, df$date_entry<="2019-10-22 23:59:59" & 
                 df$date_entry>="2019-04-22 00:00:00")

# Read addresses
address <- suppressWarnings(fetch(dbSendQuery(con, "SELECT * FROM addresses
WHERE addressable_type='App\\\\Models\\\\Clients\\\\Client'"), n=-1))
cities <- suppressWarnings(fetch(dbSendQuery(con, "SELECT * FROM cities"), n=-1))
address <- merge(address, cities[,c("id","name")], 
                 by.x = "city_id", by.y = "id", all.x = TRUE)
address <- address[,c("addressable_id","type","name","street",
                      "block","number")]
Encoding(address$name) <- "UTF-8"
Encoding(address$street) <- "UTF-8"
Encoding(address$block) <- "UTF-8"
Encoding(address$number) <- "UTF-8"

# Remerge with main df
df <- merge(df, address[address$type==1,], by.x = "client_id", 
            by.y = "addressable_id", all.x = TRUE)
names(df)[names(df) == "type"] <- "type_1"
names(df)[names(df) == "name"] <- "city_li4na_karta"
names(df)[names(df) == "street"] <- "street_li4na_karta"
names(df)[names(df) == "block"] <- "block_li4na_karta"
names(df)[names(df) == "number"] <- "number_li4na_karta"
df <- merge(df, address[address$type==2,], by.x = "client_id", 
            by.y = "addressable_id", all.x = TRUE)
names(df)[names(df) == "type"] <- "type_2"
names(df)[names(df) == "name"] <- "city_current"
names(df)[names(df) == "street"] <- "street_current"
names(df)[names(df) == "block"] <- "block_current"
names(df)[names(df) == "number"] <- "number_current"

# Choose fields
df$office <- ifelse(is.na(df$office_first), df$office_current, df$office_first)
df <- df[,c("id","client_id","egn","status","product_name",
 "city_li4na_karta","street_li4na_karta","block_li4na_karta",
 "number_li4na_karta","city_current","street_current","block_current",
 "number_current","office")]
Encoding(df$product_name) <- "UTF-8"
Encoding(df$office) <- "UTF-8"

# Set status
df$status <- ifelse(df$status==1,"Чака решение",
             ifelse(df$status==2,"Отказан",
             ifelse(df$status==3,"Одобрен",
             ifelse(df$status==4,"Активен","Приключен"))))

# Remove duplicated client_id
df <- df[!duplicated(df$client_id),]

# Make whole address field
df$full_address_li4na_karta <- paste(
  df$city_li4na_karta,df$street_li4na_karta,df$block_li4na_karta,sep=";")
df$full_address_current <- paste(
  df$city_current,df$street_current,df$block_current,sep=";")

# Get city of office
setwd("C:\\Users\\rsadek.CC\\Desktop\\Generic\\")
office_city <- read.xlsx("office_and_city.xlsx")
df <- merge(df, office_city, by.x = "office", by.y = "office", all.x = TRUE)

# Make criteria
df$criteria <- ifelse(df$full_address_current==df$full_address_li4na_karta, 
     "same_address",
     ifelse(df$city_current==df$city_li4na_karta,"same_city", 
     "different_address"))
df$office_criteria_current <- ifelse(df$city_current==df$city, 
     "office_in_city_of_current_address","different")
df$office_criteria_li4na <- ifelse(df$city_li4na_karta==df$city, 
     "office_in_city_of_current_address","different")

View(df[,c("id","full_address_li4na_karta","full_address_current","criteria",
"office","city","office_criteria_current","office_criteria_li4na")])

# Output
setwd("C:\\Users\\rsadek.CC\\Desktop\\")
write.xlsx(df, "addresses.xlsx")


