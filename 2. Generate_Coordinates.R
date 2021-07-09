

##########################################
#            GEO SEGMENTATION            #
#   GENERATE COORDINATES OF ADDRESS      #
#       USING OPEN STREE MAP API         #
##########################################



####### CHOOSE IF OFFICAL ADDRESS (TYPE=1) OR CURRENT ADDRESS (TYPE=2)

type_address <- 1




####### DEFINE FUNCTIONS

# Function to get coordinates
nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}




######## MAKE DB CONNECTION AND READ DATA 

# Read libraries
suppressWarnings(suppressMessages(library(openxlsx)))
suppressWarnings(suppressMessages(library(vars)))
suppressWarnings(suppressMessages(library(RMySQL)))
suppressWarnings(suppressMessages(library(plyr)))
library(dummies)

# Make DB connection
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
                 host=db_host, port = df_port)

# Read whole database from City Cash
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
df_raw <- df
data_encoding <- suppressWarnings(dbSendQuery(con,'set character set "utf8"'))

# Subset only clients with credits
df <- subset(df,df$status %in% c(4,5))




######## GET ADDRESS WITH NICE READABLE FORMAT 

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

# Merge addresses (only with current address)
df <- merge(df[,c("credit_number","client_id","egn")], 
      address[address$type==type_address,c("addressable_id","address_format",
      "address_format2","address_format3")], 
      by.x = "client_id", 
      by.y = "addressable_id", all.x = TRUE)

# Remove duplicates and other
df <- df[!duplicated(df$client_id),]
rows <- sample(nrow(df))
df <- df[rows, ]
df <- subset(df,!is.na(df$address_format))




######## APPLY COORDINATES WITH OPEN STREET MAP API

# Apply coordinates
df$lon <- NA
df$lat <- NA
df$precision <- NA
for(i in 1:nrow(df)){
  
  if(!is.na(df$address_format[i])){
    tryCatch({result <- nominatim_osm(df$address_format2[i])}, 
             error=function(e){})
    df$lon[i] <- ifelse(length(result$lon)==0,NA,
                        result$lon)
    df$lat[i] <- ifelse(length(result$lat)==0,NA,
                        result$lat)
  }  
  if(is.na(df$lon[i])){
    tryCatch({result <- nominatim_osm(df$address_format3[i])}, 
             error=function(e){})
    df$lon[i] <- ifelse(length(result$lon)==0,NA,
                        result$lon)
    df$lat[i] <- ifelse(length(result$lat)==0,NA,
                        result$lat)
    df$precision[i] <- 3
  } else {
    df$precision[i] <- 2
  }
}




######## OUTPUT RESULTS

write.xlsx(df,paste("C:\\Projects\\Geo_Segmentation\\result\\",
                    "Main_Dataframe_OfficialAddresses.xlsx",sep=""))




######## END


