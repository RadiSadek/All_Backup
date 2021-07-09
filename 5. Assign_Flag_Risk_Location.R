

##########################################
#       ASSIGN FLAG RISK LOCATION        #
#               TO EVERY EGN             #
##########################################



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
df <- subset(df,df$status %in% c(4,5))




######## READ ADDRESSES 

nodes <- read.xlsx(paste("C:\\Projects\\Geo_Segmentation\\result\\",
  "Defaults_per_Node_CityCash_highest.xlsx",sep = ""))
nodes$flag_location <- 1

egn <- read.csv(
"C:\\Projects\\Geo_Segmentation\\result\\All_Clients_Coordinates.csv")

egn <- merge(egn,nodes[,c("node_lon","node_lat","flag_location")],
              by.x = c("node_lon","node_lat"),
              by.y = c("node_lon","node_lat"),all.x = TRUE)
egn$flag_location <- ifelse(is.na(egn$flag_location),0,egn$flag_location)




####### OUTPUT

output <- egn[,c("client_id","flag_location")]
write.xlsx(output,
  paste("C:\\Projects\\Geo_Segmentation\\result_final\\",
         "EGN_and_flag_risk_location_v2.xlsx",sep = ""))


