

##########################################
#       COMPUTE DISTANCE BETWEEN         #
#     OFFICIAL AND CURRENT ADDRESS       #
##########################################



######## MAKE DB CONNECTION AND READ DATA 

# Read libraries
suppressWarnings(suppressMessages(library(openxlsx)))
suppressWarnings(suppressMessages(library(vars)))
suppressWarnings(suppressMessages(library(RMySQL)))
suppressWarnings(suppressMessages(library(plyr)))


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
df <- subset(df,df$status %in% c(4,5))




######## READ ADDRESSES 

current <- read.csv(paste("C:\\Projects\\Geo_Segmentation\\result\\",
 "All_Clients_Coordinates.csv",sep=""))
official <- read.csv(paste("C:\\Projects\\Geo_Segmentation\\result\\",
 "All_Clients_Coordinates_Official.csv",sep=""))
final <- merge(current,
 official[,c("client_id","address_format2","address_format3","precision",
             "node_lon", "node_lat")],
 by.x = "client_id",by.y = "client_id",all.x = TRUE)

names(final)[c(6,7,10:ncol(final))] <- 
  c("address_format2_curr","address_format3_curr","precision_curr",
    "node_lon_curr","node_lat_curr",
    "address_format2_offi","address_format3_offi","precision_offi",
    "node_lon_offi","node_lat_offi")


######## COMPUTE DISTANCE BETWEEN OFFICIAL AND CURRENT ADDRESSES

# Compute distance
final$const_a <- sin(0.5*(final$node_lat_offi-final$node_lat_curr) * pi/180)^2 + 
  cos(final$node_lat_offi * pi/180) * cos(final$node_lat_curr * pi/180) * 
  sin(0.5*(final$node_lon_offi-final$node_lon_curr) * pi/180)^2
final$distance <- round(6371e3* 2* atan2(sqrt(final$const_a),
  sqrt(1-final$const_a))/1000,3)
  
final <- final[ , -which(names(final) %in% c("const_a"))]

  


####### OUTPUT

write.xlsx(final[,c("client_id","distance")],
  paste("C:\\Projects\\Geo_Segmentation\\result_final\\",
         "EGN_and_distance_addresses.xlsx",sep = ""))


