
############################################################
############# Zadarjane Klienti (Pridobiti)   ##############
#############          Version 6.0            ##############
############################################################

# Load library
library(Hmisc)
library(plyr)
library(dplyr)
library(binr)
library(openxlsx)
library(gtools)
library(RMySQL)
library(lubridate)
library(dummies)

# Define work directory
main_dir <- "C:\\Regular\\R\\Zadarjane_Klienti_Pridobiti\\"
setwd(main_dir)

# Set functions
rename_products <- function(var){
  return(ifelse(substring(var,0,6)=="City W","City Week",
         ifelse(substring(var,0,6)=="City M","City Month",
         ifelse(substring(var,0,6)=="City 2","City 2-Week",
         ifelse(substring(var,0,6)=="City 2","City 2-Week",
         ifelse(substring(var,0,11)=="CreDirect П","Credirect Потребителски",
         ifelse(substring(var,0,11)=="CreDirect14" | 
                substring(var,0,11)=="CreDirect30" | 
                substring(var,0,11)=="CreDirect F", "CrediRect_Flex",
         ifelse(substring(var,0,5)=="Пенси","Пенсионер",
         ifelse(substring(var,0,3)=="Big","BigFin",
         ifelse(substring(var,0,3)=="Fin","FinMag","Друг"))))))))))
}

# Database specifications
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Make connection with SQL
sql_query2 <- "SELECT * FROM citycash_db.products"
sql_query3 <- "SELECT 
citycash_db.credits_applications.credit_number,
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
ON citycash_db.structure_offices.zone_id = citycash_db.structure_zones.id;
"
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
                 host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

# Select first office if movement
data_sql3 <- suppressWarnings(dbSendQuery(con, 'set character set "utf8"'))
data_sql3 <- suppressWarnings(dbSendQuery(con, sql_query3))
first_office <- fetch(data_sql3, n=-1)
Encoding(first_office$name) <- "UTF-8"
Encoding(first_office$zone_name) <- "UTF-8"
first_office <- first_office[order(first_office$created_at),]
first_office <- first_office[order(first_office$credit_number),]
first_office <- first_office[!duplicated(first_office$credit_number),]

# Read data_final, call it df, and set some fields
df <- subset(df,df$status==4 | df$status==5)
df$short_date <- substring(df$date,1,7)
df$short_date_exit <- substring(df$deactivated_at,1,7)
Encoding(df$office_current) <- "UTF-8"
Encoding(df$zone_current) <- "UTF-8"

# Set encoding of product name
Encoding(df$product_name) <- "UTF-8"

# Make back-up dataframe for later
df_raw_raw <- df

# Choosing relative columns
df <- df[,c("egn","credit_number","date","short_date","short_date_exit",
            "product_name")]

# Get all available months from data_final and make them into columns
names <- t(names(table(df$short_date)))
df_dates <- data.frame(matrix(ncol=ncol(names), nrow=nrow(df)))
colnames(df_dates) <- names
df <- cbind(df,df_dates)

# Set raw data
df_raw <- df[,c("egn","credit_number","date","short_date","short_date_exit",
                "product_name")]

# Calculate for each month, 1 if credit, 0 if not #
df$id <-1:nrow(df)
for(i in (7:(ncol(df)-1))){
  df[,i] <- ifelse(names(df)[i]>=df$short_date[df$id] & 
                     is.na(df$short_date_exit[df$id]), 1,
            ifelse(names(df)[i]>=df$short_date[df$id] & 
                     names(df)[i]<=df$short_date_exit[df$id],
                          1,0))
}
df <- df[ , -which(names(df) %in% c("id"))]

df_agg <- aggregate(list(df[,7:ncol(df)]), by = list(df$egn), sum)
colnames(df_agg) <- c("egn",names)
df <- merge(df_agg, df_raw, by.x="egn", by.y="egn", all.x=TRUE)
df <- df[,c(1:(ncol(df)-5))]
df <- df[!duplicated(df$egn), ]

# Get first and last credit for each EGN
last <- df_raw[order(df_raw$egn, rev(order(as.Date(df_raw$date)))),]
last <- last[,c("egn","credit_number","date")]
last <- last[!duplicated(last$egn), ]
names(last) <- c("egn","credit_number_last","date_last_credit")
first <- df_raw[order(df_raw$egn, df_raw$date),]
first <- first [,c("egn","credit_number","date","product_name")]
first <- first[!duplicated(first$egn), ]
names(first) <- c("egn","credit_number_first","date_first_credit",
                  "first_product")

# Join first and last credit of each EGN to all dataframe
df <- merge(df , last, by.x="egn", by.y="egn", all.x=TRUE)
df  <- merge(df , first, by.x="egn", by.y="egn", all.x=TRUE)

# Join first and last office for each EGN to all dataframe
df_raw_last <- df_raw_raw[,c("credit_number","zone_current","office_current")]
names(df_raw_last) <- c("credit_number","Зона.последен","Офис.Последен")
df <- merge(df , df_raw_last, by.x="credit_number_last", by.y="credit_number", 
            all.x=TRUE)
df_raw_first <- df_raw_raw[,c("credit_number","zone_current","office_current")]
names(df_raw_first) <- c("credit_number","Зона.първи","Офис.Първи")
df <- merge(df, df_raw_first, by.x="credit_number_first", by.y="credit_number", 
            all.x=TRUE)
df <- merge(df, first_office, by.x="credit_number_first", by.y="credit_number", 
            all.x=TRUE)
df$Офис.Първи <- ifelse(!is.na(df$name), df$name, df$Офис.Първи)
df$Зона.първи <- ifelse(!is.na(df$name), df$zone_name, df$Зона.първи)
df <- df[,-c((ncol(df)-4):ncol(df))]

# Reorder fields
df <- df[,c(3,(ncol(df)-5),(ncol(df)-4),(ncol(df)-3):ncol(df),4:(ncol(df)-7))]
df$date_first_credit <- substring(df$date_first_credit,1,7)

# Make binary total credits of EGN per month and put into relevant offices
df[,4] <- ifelse(df[,5] %in% c("Централен офис","Просрочени ЦО","Вечен дом", 
    "Полиция", "За Продажба",
    "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
    "KRONOS RECOVERY", "KRONOS RECOVERY ",
    "МТЕЛ","Починали", "Съдебен отдел","Каса Корпоративна Сигурност"),
    "Централен офис",df[,4])
df[,5] <- ifelse(df[,5] %in% c("Централен офис","Просрочени ЦО","Вечен дом", 
    "Полиция", "За Продажба", "МТЕЛ", "Починали",
    "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
    "KRONOS RECOVERY", "KRONOS RECOVERY ", 
    "Починали,Съдебен отдел","Каса Корпоративна Сигурност"),
    "Централен офис",df[,5])
df[,6] <- ifelse(df[,7] %in% c("Централен офис","Просрочени ЦО","Вечен дом", 
    "Полиция", "За Продажба", "МТЕЛ", "Починали",
    "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
    "KRONOS RECOVERY", "KRONOS RECOVERY ",
    "Починали,Съдебен отдел","Каса Корпоративна Сигурност"),
    "Централен офис",df[,6])
df[,7] <- ifelse(df[,7] %in% c("Централен офис","Просрочени ЦО","Вечен дом",
     "Полиция", "За Продажба", "МТЕЛ", "Починали",
     "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
     "KRONOS RECOVERY", "KRONOS RECOVERY ",
     "Починали,Съдебен отдел","Каса Корпоративна Сигурност"),
     "Централен офис",df[,7])

df$Зона.последен <- ifelse(df$Офис.Последен %in% c("CreDirect",
      "CreDirect-Creditour","CreDirect-WebBroker",
      "ЦЕНТРАЛЕН ОФИС-КРЕДИРЕКТ", "За продажба КРЕДИРЕКТ"),
      "CreDirect",df$Зона.последен)
df$Зона.първи <- ifelse(df$Офис.Първи %in% c("CreDirect","CreDirect-Creditour",
      "CreDirect-WebBroker",
      "ЦЕНТРАЛЕН ОФИС-КРЕДИРЕКТ", "За продажба КРЕДИРЕКТ"),
      "CreDirect",df$Зона.първи)
df$Зона.последен <- ifelse(df$Офис.Последен %in% c("Ипотеки","Финстарт"),
      "Ипотеки",df$Зона.последен)
df$Зона.първи <- ifelse(df$Офис.Първи %in% c("Ипотеки","Финстарт"),
      "Ипотеки",df$Зона.първи)
df$Зона.последен <- ifelse(df$Офис.Последен=="СЪД-КРЕДИРЕКТ","СЪД-КРЕДИРЕКТ",
                           df$Зона.последен)
df$Зона.първи <- ifelse(df$Офис.Първи=="СЪД-КРЕДИРЕКТ","СЪД-КРЕДИРЕКТ",
                        df$Зона.първи)

df$id <-1:nrow(df)
for(i in (8:(ncol(df)-1))){
  
  df[df$id,i] <- as.numeric(df[df$id,i])
  df[df$id,i] <- ifelse(df[df$id,i]>=1,1,0)
  
}
df <- df[ , -which(names(df) %in% c("id"))]

# Remove potential duplicates
df <- df[!duplicated(df$egn),]

# Change name of column 
names(df)[3] <- "Първи.продукт"
df$Първи.продукт <- apply(df[,c("Първи.продукт"),drop=F], 1, rename_products)

# Save output and Rdata 
save(df, file = "Dataframe_saved_non_filtered.RData")
wb <- loadWorkbook("output.xlsx")
writeData(wb, sheet = "Data_not_filtered", df, colNames = T)
saveWorkbook(wb,"output.xlsx", overwrite = T)

# Make back-up dataframe for not filtered results
df_result_not_filtered <- df



############ Redo the same thing but filtered with 90 days #############

# Read back-up dataframe
df <- df_raw_raw 

# Aggregate those who have defaulted and subset
defaulted_ever <- aggregate(df$default_flag, by=list(df$egn), FUN=sum)
names(defaulted_ever) <- c("egn","defaulted_ever")
df <- merge(df, defaulted_ever, by.x = "egn", by.y = "egn", all.x = TRUE)
df <- subset(df, df$defaulted_ever==0)

# Choosing relative columns
df <- df[,c("egn","credit_number","date","deactivated_at","short_date",
            "short_date_exit",
            "cession","days_delay","date_default","product_name")]
df <- df[,c("egn","credit_number","date","short_date","short_date_exit",
            "product_name")]

# Get all available months from data_final and make them into columns
names <- t(names(table(df$short_date)))
df_dates <- data.frame(matrix(ncol=ncol(names), nrow=nrow(df)))
colnames(df_dates) <- names
df <- cbind(df,df_dates)

# Set raw data
df_raw <- df[,c("egn","credit_number","date","short_date","short_date_exit",
                "product_name")]

# Calculate for each month, 1 if credit, 0 if not #
df$id <-1:nrow(df)
for(i in (7:(ncol(df)-1))){
  df[,i] <- ifelse(names(df)[i]>=df$short_date[df$id] & 
                    is.na(df$short_date_exit[df$id]), 1,
            ifelse(names(df)[i]>=df$short_date[df$id] & 
                    names(df)[i]<=df$short_date_exit[df$id], 1,0))
}
df <- df[ , -which(names(df) %in% c("id"))]

df_agg <- aggregate(list(df[,7:ncol(df)]), by = list(df$egn), sum)
colnames(df_agg) <- c("egn",names)
df <- merge(df_agg, df_raw, by.x="egn", by.y="egn", all.x=TRUE)
df <- df[,c(1:(ncol(df)-5))]
df <- df[!duplicated(df$egn), ]

# Get first and last credit for each EGN
last<- df_raw[order(df_raw$egn, rev(order(as.Date(df_raw$date)))),]
last <- last[,c("egn","credit_number","date")]
last <- last[!duplicated(last$egn), ]
names(last) <- c("egn","credit_number_last","date_last_credit")
first <- df_raw[order(df_raw$egn, df_raw$date),]
first <- first [,c("egn","credit_number","date","product_name")]
first <- first[!duplicated(first$egn), ]
names(first) <- c("egn","credit_number_first","date_first_credit",
                  "first_product")

# Join first and last credit of each EGN to all dataframe
df <- merge(df , last, by.x="egn", by.y="egn", all.x=TRUE)
df  <- merge(df , first, by.x="egn", by.y="egn", all.x=TRUE)

# Join first and last office for each EGN to all dataframe
df_raw_last <- df_raw_raw[,c("credit_number","zone_current","office_current")]
names(df_raw_last) <- c("credit_number","Зона.последен","Офис.Последен")
df <- merge(df , df_raw_last, by.x="credit_number_last", by.y="credit_number", 
            all.x=TRUE)
df_raw_first <- df_raw_raw[,c("credit_number","zone_current","office_current")]
names(df_raw_first) <- c("credit_number","Зона.първи","Офис.Първи")
df <- merge(df , df_raw_first, by.x="credit_number_first", by.y="credit_number", 
            all.x=TRUE)
df <- merge(df, first_office, by.x="credit_number_first", by.y="credit_number", 
            all.x=TRUE)
df$Офис.Първи <- ifelse(!is.na(df$name), df$name, df$Офис.Първи)
df$Зона.първи <- ifelse(!is.na(df$name), df$zone_name, df$Зона.първи)
df <- df[,-c((ncol(df)-4):ncol(df))]

# Reorder fields
df <- df[,c(3,(ncol(df)-5),(ncol(df)-4),(ncol(df)-3):ncol(df),4:(ncol(df)-7))]
df$date_first_credit <- substring(df$date_first_credit,1,7)

# Make binary total credits of EGN per month and put into relevant offices
df[,4] <- ifelse(df[,5] %in% c("Централен офис","Просрочени ЦО","Вечен дом", 
      "Полиция", "За Продажба",
      "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
      "KRONOS RECOVERY", "KRONOS RECOVERY ",
      "МТЕЛ","Починали", "Съдебен отдел","Каса Корпоративна Сигурност"),
      "Централен офис",df[,4])
df[,5] <- ifelse(df[,5] %in% c("Централен офис","Просрочени ЦО","Вечен дом", 
      "Полиция", "За Продажба", "МТЕЛ", "Починали",
      "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
      "KRONOS RECOVERY", "KRONOS RECOVERY ", 
      "Починали,Съдебен отдел","Каса Корпоративна Сигурност"),
      "Централен офис",df[,5])
df[,6] <- ifelse(df[,7] %in% c("Централен офис","Просрочени ЦО","Вечен дом",
      "Полиция", "За Продажба", "МТЕЛ", "Починали",
      "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
      "KRONOS RECOVERY", "KRONOS RECOVERY ",
      "Починали,Съдебен отдел","Каса Корпоративна Сигурност"),
      "Централен офис",df[,6])
df[,7] <- ifelse(df[,7] %in% c("Централен офис","Просрочени ЦО","Вечен дом",
      "Полиция", "За Продажба", "МТЕЛ", "Починали",
      "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
      "KRONOS RECOVERY", "KRONOS RECOVERY ",
      "Починали,Съдебен отдел","Каса Корпоративна Сигурност"),
      "Централен офис",df[,7])

df$Зона.последен <- ifelse(df$Офис.Последен %in% c("CreDirect",
     "CreDirect-Creditour","CreDirect-WebBroker",
     "ЦЕНТРАЛЕН ОФИС-КРЕДИРЕКТ", "За продажба КРЕДИРЕКТ"),
     "CreDirect",df$Зона.последен)
df$Зона.първи <- ifelse(df$Офис.Първи %in% c("CreDirect",
     "CreDirect-Creditour","CreDirect-WebBroker",
     "ЦЕНТРАЛЕН ОФИС-КРЕДИРЕКТ", "За продажба КРЕДИРЕКТ"),
     "CreDirect",df$Зона.първи)
df$Зона.последен <- ifelse(df$Офис.Последен %in% c("Ипотеки","Финстарт"),
      "Ипотеки",df$Зона.последен)
df$Зона.първи <- ifelse(df$Офис.Първи %in% c("Ипотеки","Финстарт"),
     "Ипотеки",df$Зона.първи)
df$Зона.последен <- ifelse(df$Офис.Последен=="СЪД-КРЕДИРЕКТ","СЪД-КРЕДИРЕКТ",
     df$Зона.последен)
df$Зона.първи <- ifelse(df$Офис.Първи=="СЪД-КРЕДИРЕКТ","СЪД-КРЕДИРЕКТ",
     df$Зона.първи)


df$id <-1:nrow(df)
for(i in (8:(ncol(df)-1))){
  df[df$id,i] <- as.numeric(df[df$id,i])
  df[df$id,i] <- ifelse(df[df$id,i]>=1,1,0)
}
df <- df[ , -which(names(df) %in% c("id"))]

# Remove potential duplicates
df <- df[!duplicated(df$egn),]

# Change name of column 
names(df)[3] <- "Първи.продукт"
df$Първи.продукт <- apply(df[,c("Първи.продукт"),drop=F], 1, rename_products)

# Make back-up dataframe for filtered results
df_result_filtered <- df

# Save output and Rdata 
save(df, file = "Dataframe_saved_filtered.RData")
wb <- loadWorkbook("output.xlsx")
writeData(wb, sheet = "Data_filtered", df, colNames = T)
saveWorkbook(wb,"output.xlsx", overwrite = T)



############ Calculate first and current product type #############

# Read back-up datafranme
df <- df_raw_raw 

# Choosing relative columns
df <- df[,c("egn","status","credit_number","date","short_date",
            "short_date_exit","product_name")]

# Set raw data
df_raw <- df[,c("egn","credit_number","status","date","short_date",
                "short_date_exit","product_name")]

# Get first and last credit for each EGN
last<- df_raw[order(df_raw$egn, rev(order(as.Date(df_raw$date)))),]
last <- last[,c("egn","credit_number","date")]
last <- last[!duplicated(last$egn), ]
names(last) <- c("egn","credit_number_last","date_last_credit")
first <- df_raw[order(df_raw$egn, df_raw$date),]
first <- first [,c("egn","credit_number","date","product_name")]
first <- first[!duplicated(first$egn), ]
names(first) <- c("egn","credit_number_first","date_first_credit",
                  "first_product")

# Join first and last credit of each EGN to all dataframe
df <- merge(df , last, by.x="egn", by.y="egn", all.x=TRUE)
df <- merge(df , first, by.x="egn", by.y="egn", all.x=TRUE)

# Rename products
df$Първи_Продукт <- apply(df[,c("first_product"),drop=F], 1, rename_products)
df$Текущ_Продукт <- apply(df[,c("product_name"),drop=F], 1, rename_products)

# Reorder columns
df$Първи_Продукт <- as.character(df$Първи_Продукт)
df$Текущ_Продукт <- as.character(df$Текущ_Продукт)

# Select relevant columns
df_agg <- df[,c("egn","Първи_Продукт","Текущ_Продукт","status")]
#df_agg <- subset(df, df$credit_number!=df$credit_number_last)
df <- df[,c("egn","Първи_Продукт","Текущ_Продукт","status")]

# Take only credits with 1 finished credit
df <- subset(df, df$status==5)
df <- df[,c("egn","Първи_Продукт","Текущ_Продукт")]

# Select only active at this moment
df_agg <- subset(df_agg, df_agg$status==4)
df_agg <- df_agg[,c("egn","Първи_Продукт","Текущ_Продукт")]

# Make dummy and aggregate number of credits per egn
Текущ_Продукт <- dummy.data.frame(as.data.frame(df_agg$Текущ_Продукт), 
                                  sep = "_")
df_agg <- cbind(df_agg, Текущ_Продукт)
names(df_agg)[4:11] <- c("BigFin","City_2_Week","City_Month", "City_Week",
                         "Credirect_Потребителски","CrediRect_Flex",
                         "Друг","Пенсионер")
c_BigFin <- aggregate(list("BigFin"= df_agg$BigFin), 
                      by=list(egn=df_agg$egn), FUN=sum)
c_City_2_Week <- aggregate(list("City_2_Week"= df_agg$City_2_Week),
                           by=list(egn=df_agg$egn), FUN=sum)
c_City_Month <- aggregate(list("City_Month"= df_agg$City_Month), 
                          by=list(egn=df_agg$egn), FUN=sum)
c_City_Week <- aggregate(list("City_Week"= df_agg$City_Week), 
                         by=list(egn=df_agg$egn), FUN=sum)
c_Credirect_Потребителски <- aggregate(
  list("Credirect_Потребителски"= df_agg$Credirect_Потребителски), 
  by=list(egn=df_agg$egn), FUN=sum)
c_CrediRect_Flex <- aggregate(list("CrediRect_Flex"= df_agg$CrediRect_Flex), 
                              by=list(egn=df_agg$egn), FUN=sum)
c_Друг <- aggregate(list("Друг"= df_agg$Друг), 
                    by=list(egn=df_agg$egn), FUN=sum)
c_Пенсионер <- aggregate(list("Пенсионер"= df_agg$Пенсионер), 
                         by=list(egn=df_agg$egn), FUN=sum)

# Select important variables
df <- df[,c(1,2)]

# Merge with main dataframe
df <- merge(df, c_BigFin, by.x="egn", by.y="egn", all.x=TRUE)
df <- merge(df, c_City_2_Week, by.x="egn", by.y="egn", all.x=TRUE)
df <- merge(df, c_City_Month, by.x="egn", by.y="egn", all.x=TRUE)
df <- merge(df, c_City_Week, by.x="egn", by.y="egn", all.x=TRUE)
df <- merge(df, c_Credirect_Потребителски, by.x="egn", by.y="egn", all.x=TRUE)
df <- merge(df, c_CrediRect_Flex, by.x="egn", by.y="egn", all.x=TRUE)
df <- merge(df, c_Друг, by.x="egn", by.y="egn", all.x=TRUE)
df <- merge(df, c_Пенсионер, by.x="egn", by.y="egn", all.x=TRUE)

# Remove duplicates
df <- df[!duplicated(df$egn), ]

# Put all NAs to 0
df[is.na(df)] <- 0

# Sum all citycash
df$CityCash <- df$City_2_Week+df$City_Month+df$City_Week+df$Друг+
  df$Пенсионер+df$BigFin
df$Credirect <- df$Credirect_Потребителски+df$CrediRect_Flex

# Recorrect fields if user has double actives
df$CityCash_and_Credirect <- ifelse(df$Credirect>=1 & df$CityCash>=1, 
                                    df$Credirect + df$CityCash, 0)
for (i in (1:nrow(df))) {
  for(j in (3:12)) {
    if(df$CityCash_and_Credirect[i]>=1){
      df[i,j] <- 0
    }
  }}

# See if client has no credits
df$no_credit <- ifelse((df$BigFin+df$City_2_Week+df$City_Month+df$City_Week+
                          df$Credirect_Потребителски+
                          df$CrediRect_Flex+df$CityCash_and_Credirect+
                          df$Друг+df$Пенсионер)==0, 1, 0
)

# Output result
wb <- loadWorkbook("output.xlsx")
writeData(wb, sheet = "First_Product", df, colNames = T)
saveWorkbook(wb,"output.xlsx", overwrite = T)



############ Filter when client defaults #############

# Read back-up dataframe
df <- df_raw_raw 

# Get date of first default per EGN
plan_sql <- "SELECT application_id, pay_day, days_delay FROM credits_plan_main"
plan <- suppressWarnings(fetch(dbSendQuery(con, plan_sql), n=-1))
plan$default_flag <- ifelse(plan$days_delay>=90, 1, 0)
plan <- subset(plan, plan$default_flag==1)
plan <- plan[order(plan$pay_day),]
plan <- plan[order(plan$application_id),]
plan <- plan[!duplicated(plan$application_id),]
plan <- plan[,c("application_id","pay_day")]
names(plan) <- c("application_id","first_def_date")
df <- merge(df, plan, by.x="id", by.y="application_id", all.x=TRUE)
df$first_def_date_cor <- as.Date(df$first_def_date) + 90

df_def <- subset(df, !is.na(df$first_def_date_cor))
agg_def <- aggregate(df_def$first_def_date_cor, by=list(df_def$egn), FUN=min)
names(agg_def) <- c("egn","first_day_def")
agg_def$first_month_def <- substring(agg_def$first_day_def,1,7)

# Remove 1s whenever client defaults
df2 <- df_result_not_filtered
df2 <- merge(df2, agg_def[,c("egn","first_month_def")], by.x = "egn", 
             by.y = "egn", all.x = TRUE)

df2$id <-1:nrow(df2)
for(i in (8:(ncol(df2)-2))){
  df2[df2$id,i] <- ifelse(is.na(df2$first_month_def[df2$id]), df2[df2$id,i],
                          ifelse(df2$first_month_def[df2$id]<=names(df2)[i], 0, 
                                 df2[df2$id,i]))
}
df2 <- df2[ , -which(names(df2) %in% c("id"))]

# Remove potential duplicates
df2 <- df2[!duplicated(df2$egn),]



############ Compute retention rate #############

# Set/get back-up
df_backup <- df2

# Create dataframe and set fields
df_retention <- data.frame(matrix(ncol = 11 , nrow = (ncol(df2)-8)))
names(df_retention) <- c("month","all",names(table(df2$Първи.продукт)))
df_retention$month <- names(df2)[8:(ncol(df2)-1)]
df_retention$all <- NA
df_new <- df_result_not_filtered
df_new <- merge(df_new, df2[,c("egn","first_month_def")], by.x="egn", 
                by.y="egn", all.x=TRUE)

# Compute retention rate
for (i in 19:nrow(df_retention)){
  first <- i-15
  last <- i-4
  
  # Compute retention rate for all
  df_local <- subset(df_new, is.na(df_new$first_month_def) | 
      df_new$first_month_def>names(table(df_retention$month))[i])
  df_retention$all[i] <- sum(df2[[names(table(df_retention$month))[i]]]
      [df2$date_first_credit %in% 
          c(names(table(df_retention$month))[first:last])]) / 
    sum(df_local$date_first_credit %in% 
          names(table(df_retention$month))[first:last])
  
  # Compute retention rate specific to each product type
  for(j in 3:length(names(df_retention))){
    df_retention[i,j] <- sum(df2[[names(table(df_retention$month))[i]]]
      [df2$date_first_credit %in% 
          c(names(table(df_retention$month))[first:last]) & 
          df2$Първи.продукт==names(df_retention)[j]]) / 
      sum(df_local$date_first_credit[
        df_local$Първи.продукт==names(df_retention)[j]]
          %in% names(table(df_retention$month))[first:last])
  }
}

# Reselect relevant months (early months are deleted)
df_retention <- subset(df_retention, df_retention$month>="2015-01")



############ Оutpur results #############

wb <- loadWorkbook("output.xlsx")
writeData(wb, sheet = "Retention Rate table", df_retention , colNames = T)
saveWorkbook(wb,"output.xlsx", overwrite = T)


#################################
##### The End of all things #####
#################################
