
##########################################
#            GEO SEGMENTATION            #
#   REASSESS AND REARRANGE COORDINATES   #
#       DISCRETIZE AND AGGREGATE         #
##########################################


####### Choose modeling parameters

resolution <- 0.0025
minimal_default_rate_pct <- 65
min_counts <- 50




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

df_plot <- read.csv(paste(
  "C:\\Projects\\Geo_Segmentation\\result\\",
  "All_Clients_Coordinates_Official.csv",sep=""))
map_bg <- read.csv(paste(
  "C:\\Projects\\Geo_Segmentation\\result\\",
  "Counts_per_Node_Final_Official.csv",sep=""))





########## SELECT ONLY DISCRETIZED NODES >=50 AND AGGREGATE PAYMENTS

# Select with enough counts
map_select <- subset(map_bg,map_bg$counts>=1)
map_select$coordinates <- paste(map_select$lon,map_select$lat,sep="-")
df_plot$coordinates <- paste(df_plot$node_lon,df_plot$node_lat,sep="-")
df_select <- df_plot[df_plot$coordinates %in% map_select$coordinates,]


# Get defaulted by egn
egn_def <- aggregate(df$default_flag,by=list(df$egn),FUN=sum)
names(egn_def) <- c("egn","default")
egn_def$default <- ifelse(egn_def$default>1,1,egn_def$default)

# Get first entry and subset subsequently 
last_entry <- df[rev(order(df$signed_at)),]
last_entry <- last_entry[order(last_entry$egn),]
last_entry <- last_entry[!duplicated(last_entry$egn),]
egn_def <- merge(egn_def,last_entry[,c("egn","signed_at")],
                 by.x = "egn",by.y = "egn",all.x = TRUE)
names(egn_def)[ncol(egn_def)] <- "last_entry"
egn_def <- subset(egn_def,egn_def$last_entry>="2017-01-01")

# Get last entry and subset subsequently 
first_entry <- df[order(df$signed_at),]
first_entry <- first_entry[order(first_entry$egn),]
first_entry <- first_entry[!duplicated(first_entry$egn),]
egn_def <- merge(egn_def,first_entry[,c("egn","signed_at")],
                 by.x = "egn",by.y = "egn",all.x = TRUE)
names(egn_def)[ncol(egn_def)] <- "first_entry"
egn_def <- subset(egn_def,egn_def$first_entry<="2020-10-31")

# Merge to main dataframe 
df_select <- merge(df_select,egn_def[,c("egn","default")],
                   by.x = "egn",by.y = "egn",all.x = TRUE)
df_select$count <- 1
df_select <- subset(df_select,!is.na(df_select$default))

# Get defaults by Company
df_credirect <- subset(df,df$online_offline=="online")
df_citycash <- subset(df,df$online_offline=="offline")

egn_def_credirect <- aggregate(df_credirect$default_flag,
   by=list(df_credirect$egn),FUN=sum)
names(egn_def_credirect) <- c("egn","default_credirect")
egn_def_credirect$default_credirect <- ifelse(egn_def_credirect$default>1,1,
   egn_def_credirect$default)

egn_def_citycash <- aggregate(df_citycash$default_flag,
    by=list(df_citycash$egn),FUN=sum)
names(egn_def_citycash) <- c("egn","default_citycash")
egn_def_citycash$default_citycash <- ifelse(egn_def_citycash$default>1,1,
   egn_def_citycash$default)

df_select <- merge(df_select,egn_def_credirect,
                   by.x = "egn",by.y = "egn",all.x = TRUE)
df_select <- merge(df_select,egn_def_citycash,
                   by.x = "egn",by.y = "egn",all.x = TRUE)

df_select$count_credirect <- ifelse(is.na(df_select$default_credirect),0,1)
df_select$count_citycash <- ifelse(is.na(df_select$default_citycash),0,1)

df_select$default_credirect <- ifelse(is.na(df_select$default_credirect),0,
                                      df_select$default_credirect)
df_select$default_citycash <- ifelse(is.na(df_select$default_citycash),0,
                                      df_select$default_citycash)


# Make dataframe for defaults
node_def <- merge(
  aggregate(df_select$default,by=list(df_select$coordinates),FUN=sum),
  aggregate(df_select$count,by=list(df_select$coordinates),FUN=sum),
  by.x = "Group.1",by.y = "Group.1",all.x = TRUE)
names(node_def) <- c("coordinates","defaults","counts")
node_def$def_pct <- round(node_def$defaults / node_def$counts,4)*100

node_def <- merge(node_def,
  merge(
  aggregate(df_select$default_credirect,by=list(df_select$coordinates),FUN=sum),
  aggregate(df_select$count_credirect,by=list(df_select$coordinates),FUN=sum),
  by.x = "Group.1",by.y = "Group.1",all.x = TRUE),
  by.x = "coordinates",by.y = "Group.1",all.x = TRUE)

node_def <- merge(node_def,
  merge(
  aggregate(df_select$default_citycash,by=list(df_select$coordinates),FUN=sum),
  aggregate(df_select$count_citycash,by=list(df_select$coordinates),FUN=sum),
  by.x = "Group.1",by.y = "Group.1",all.x = TRUE),
  by.x = "coordinates",by.y = "Group.1",all.x = TRUE)

names(node_def)[5:8] <- c("defaults_credirect","counts_credirect",
                          "defaults_citycash","counts_citycash")
node_def$def_pct_credirect <- round(node_def$defaults_credirect / 
  node_def$counts_credirect,4)*100
node_def$def_pct_citycash <- round(node_def$defaults_citycash / 
  node_def$counts_citycash,4)*100

# Join real coordinates
df_select_unique <- df_select[!duplicated(df_select$coordinates),]
node_def <- merge(node_def,
                  df_select_unique[,c("coordinates","node_lon","node_lat")],
                  by.x = "coordinates",by.y = "coordinates",all.x = TRUE)




########## AGGRGATE WITH POTENTIAL NEIGHBORS

# Aggregate by neigbors
node_def$defaults_credirect_agg <- NA
node_def$counts_credirect_agg <- NA
node_def$credirect_agg <- NA
for(i in 1:nrow(node_def)){
  
  here_lon <- node_def$node_lon[i]
  here_lat <- node_def$node_lat[i]
  here <- subset(node_def,
    as.numeric(abs(node_def$node_lon-here_lon))<=(resolution+0.005) & 
    as.numeric(abs(node_def$node_lat-here_lat))<=(resolution+0.005))
  
  node_def$credirect_agg[i] <- nrow(here)
  node_def$defaults_credirect_agg[i] <- sum(here$defaults_credirect)
  node_def$counts_credirect_agg[i] <- sum(here$counts_credirect)
}

node_def$defaults_credirect <- node_def$defaults_credirect_agg
node_def$counts_credirect <- node_def$counts_credirect_agg
node_def$def_pct_credirect <- round(node_def$defaults_credirect / 
                                   node_def$counts_credirect,4)*100






########## FINALIZE DATAFRAME AND OUTPUT RESULTS 

# Compute final dataframe
node_def <- node_def[,c(11,12,6,9)]
 
# Create subsets
node_def_credirect <- subset(node_def,node_def$counts_credirect>=min_counts)

# Remove certain obsolete nodes
node_def_credirect <- subset(node_def_credirect,
  !(node_def_credirect$node_lat==42.6925 & node_def_credirect$node_lon==23.32))
node_def_credirect <- subset(node_def_credirect,
  !(node_def_credirect$node_lat==42.4175 & 
      node_def_credirect$node_lon==25.6125))


# Output results for GIS visualization 
write.csv(node_def_credirect,paste("C:\\Projects\\Geo_Segmentation\\result\\",
  "Defaults_per_Node_Credirect_Official.csv",sep=""
))


# Output nodes with Default Rate >= 65%
write.xlsx(node_def_credirect[
  node_def_credirect$def_pct_credirect>=minimal_default_rate_pct,],
  paste("C:\\Projects\\Geo_Segmentation\\result\\",
  "Defaults_per_Node_Credirect_highest_Official.xlsx",sep = ""))




######### END



