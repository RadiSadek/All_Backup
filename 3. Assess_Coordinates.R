
##########################################
#            GEO SEGMENTATION            #
#   REASSESS AND REARRANGE COORDINATES   #
#       DISCRETIZE AND AGGREGATE         #
##########################################


####### Choose modeling parameters

resolution <- 0.0025





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

# Read coordinated addresses
df_plot <- read.xlsx(
 "C:\\Projects\\Geo_Segmentation\\result\\Main_Dataframe_OfficialAddresses.xlsx")

# Select non-empty coordinates
df_plot <- subset(df_plot,!is.na(df_plot$lon))
df_plot$lon <- as.numeric(round(df_plot$lon,5))
df_plot$lat <- as.numeric(round(df_plot$lat,5))

# Take only precision 2
df_plot <- subset(df_plot,df_plot$precision==2)

# Correct certain points
# Pernik
df_plot$coordinates <- paste(df_plot$lon,df_plot$lat,sep="-")
df_plot$lat <- ifelse(df_plot$coordinates=="22.8825-42.62803",42.60542,
                       df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="22.8825-42.62803",23.03151,
                       df_plot$lon)

# Blagoevgrad
df_plot$lat <- ifelse(df_plot$coordinates=="23.48705-41.75022",42.01686,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="23.48705-41.75022",23.09398,
                      df_plot$lon)

# Vratsa
df_plot$lat <- ifelse(df_plot$coordinates=="23.71572-43.39882",43.20937,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="23.71572-43.39882",23.55262,
                      df_plot$lon)

# Smolian
df_plot$lat <- ifelse(df_plot$coordinates=="24.59012-41.6218",41.57635,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="24.59012-41.6218",24.70324,
                      df_plot$lon)

# Tchepelare
df_plot$lat <- ifelse(df_plot$coordinates=="24.65008-41.77940",41.72407,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="24.65008-41.77940",24.68464,
                      df_plot$lon)

# Dobrich
df_plot$lat <- ifelse(df_plot$coordinates=="27.90018-43.66363",43.57257,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="27.90018-43.66363",27.830402,
                      df_plot$lon)

# Samokov
df_plot$lat <- ifelse(df_plot$coordinates=="23.27808-42.46825",42.34569,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="23.27808-42.46825",23.56034,
                      df_plot$lon)

# Dobrich 
df_plot$lat <- ifelse(df_plot$coordinates=="27.58339-43.80135",43.57394,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="27.58339-43.80135",27.84881,
                      df_plot$lon)

# Yambol
df_plot$lat <- ifelse(df_plot$coordinates=="26.47976-42.53186",42.47261,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="26.47976-42.53186",26.52233,
                      df_plot$lon)

# Sofia - Levski
df_plot$lat <- ifelse(df_plot$coordinates=="23.41422-42.69557",42.70837,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="23.41422-42.69557",23.38370,
                      df_plot$lon)


# Sofia - Obelia
df_plot$lat <- ifelse(df_plot$coordinates=="23.2761-42.74069",42.73834,
                      df_plot$lat)
df_plot$lon <- ifelse(df_plot$coordinates=="23.2761-42.74069",23.27082,
                      df_plot$lon)



df_plot <- df_plot[ , -which(names(df_plot) %in% c("coordinates"))]






######## AGGREGGATE PER DISCRETIZED NODE

# Discritezie map of Bulgaria by a certain resolution
lon_bg <- seq(22.4,28.6,resolution)
lat_bg <- seq(41.25,44.25,resolution)
map_bg <- as.data.frame(expand.grid(lon_bg,lat_bg))
names(map_bg) <- c("lon","lat")

# Get closer node
for(i in 1:nrow(df_plot)){
  df_plot$node_lon[i] <- map_bg$lon[which.min(abs(map_bg$lon - df_plot$lon[i]))]
  df_plot$node_lat[i] <- map_bg$lat[which.min(abs(map_bg$lat - df_plot$lat[i]))]
}

# Plot discretization of Bulgaria
clients_map <- as.data.frame(table(df_plot$node_lon,df_plot$node_lat))
names(clients_map) <- c("node_lon","node_lat","counts")
map_bg[is.na(map_bg)] <- 0
map_bg <- merge(map_bg,clients_map,by.x = c("lon","lat"),
                by.y = c("node_lon","node_lat"),all.x = TRUE)
map_bg[is.na(map_bg)] <- 0

# Remove obsolete points
map_bg <- subset(map_bg,map_bg$lat<=44.22 & map_bg$lat>=41.33 & 
                        map_bg$lon>=22.43 & map_bg$lon<=28.58)

# Output results 
write.csv(df_plot,
 "C:\\Projects\\Geo_Segmentation\\result\\All_Clients_Coordinates_Official.csv")
write.csv(map_bg,
 "C:\\Projects\\Geo_Segmentation\\result\\Counts_per_Node_Final_Official.csv")


####### END 