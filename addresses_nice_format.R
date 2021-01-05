
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
sql <- paste("SELECT * 
             FROM test.data_final",sep="")
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

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
address$address_format <- paste(
  "пк. ",address$zip,
  ", обл. ",address$area,
  ", гр. ",address$city,
  ifelse(is.na(address$neighborhood_text),
         "",
         paste(", ж.к. ",address$neighborhood_text)),
  ifelse(is.na(address$neighborhood_text),
         ifelse(is.na(address$street),"",paste(", ул. ",address$street)),
         ifelse(is.na(address$block),"",paste(", бл. ",address$block))),
  ifelse(is.na(address$neighborhood_text),
         ifelse(is.na(address$number),"",paste(", №",address$number)),
         ""),
  ifelse(is.na(address$entrance),"",paste(", вх. ",address$entrance)),
  ifelse(is.na(address$floor),"",paste(", ет. ",address$floor)),
  ifelse(is.na(address$apartment),"",paste(", ап. ",address$apartment)),
  sep=""
)

# Merge addresses
df <- merge(df[,c("credit_number","client_id","egn")], 
            address[address$type==2,c("addressable_id","address_format")], 
            by.x = "client_id", 
            by.y = "addressable_id", all.x = TRUE)

# Output results
# Output
setwd("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output")
write.xlsx(df,"all_address_nice_format.xlsx")



