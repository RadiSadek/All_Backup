

# Get libraries
library(openxlsx)
library(RMySQL)
library(lubridate)
library(zoo)

# Database specifications
db_user <- "root"
db_password <- "123456"
db_name <- "romania"
db_host <- "127.0.0.1"
df_port <- 3306

# Define functions
gen_last_day_month <- function(date){
  year <- substring(date,1,4)
  month <- substring(date,6,7)
  end_day <- ifelse(month=="02","28",
     ifelse(month %in% 
     c("01","03","05","07","08","10","12"),"31","30"))
  end_day <- ifelse(month=="02" & year %in% 
     c("2004","2008","2012","2016","2020","2024","2028","2032"), "29", end_day)
  return(paste(year,month,end_day, sep="-"))
}

gen_field <- function(i,days,flag){
  if(flag==0){
    result <- ifelse(as.Date(
      gen_last_day_month(v_int$Activated.at))+days<=
        as.Date(first_month), v_int[,i], 0)} 
  else {
    result <- ifelse(
      as.Date(gen_last_day_month(v_int$Activated.at))+
        days<as.Date(first_month), v_int[,i], 0)}
  return(result)}

# Read data
setwd("C:\\Regular_Romania\\Vintage_Romania\\")
v_int <- read.xlsx("vintage_input.xlsx")

# set first of month
first_month <- floor_date(as.Date(Sys.time()), "month")

# Filter data with irrelevant date
v_int$Activated.at <- as.Date(v_int$Activated.at, origin = "1899-12-30")
v_int <- subset(v_int, v_int$Activated.at<first_month)

# Compute columns
v_int$month <- as.numeric(substring(v_int[,2],6,7))
v_int$year <- as.numeric(substring(v_int[,2],1,4))

v_int$m30 <- gen_field(8,29,0)
v_int$r30 <- gen_field(9,29,0)
v_int$m60 <- gen_field(10,59,0)
v_int$r60 <- gen_field(11,59,0)
v_int$m90 <- gen_field(12,89,0)
v_int$r90 <- gen_field(13,89,0)
v_int$m120 <- gen_field(14,119,0)
v_int$r120 <- gen_field(15,119,0)
v_int$m180 <- gen_field(16,179,0)
v_int$r180 <- gen_field(17,179,0)
v_int$m210 <- gen_field(18,209,0)
v_int$r210 <- gen_field(19,209,0)
v_int$m360 <- gen_field(20,359,0)
v_int$r360 <- gen_field(21,359,0)
v_int$m_o_360 <- gen_field(22,359,1)
v_int$r_o_360  <- gen_field(23,359,1)

# Join product names
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
   dbname=db_name, host=db_host, port = df_port)
data_encoding <- suppressWarnings(dbSendQuery(con, 'set character set "utf8"'))
products <- products <- fetch(suppressWarnings(dbSendQuery(con, paste("
SELECT a.credit_number, a.master_client_id AS client_id, b.name AS Product 
FROM ",db_name,".loans a JOIN ",db_name,".products b ON a.product_id=b.id",
sep=""))), n=-1)
v_int <- merge(v_int, products, by.x = "Credit.number",
               by.y = "credit_number", all.x = TRUE)

# Compute if repeat or not
v_int <- v_int[order(v_int$Activated.at),]
v_int <- v_int[order(v_int$client_id),]
v_int$Repeat_Customer <- 0
for(i in 2:nrow(v_int)){
  if(v_int$client_id[i]==v_int$client_id[i-1]){
    v_int$Repeat_Customer[i] <- 1
  }
}
v_int$Repeat_Customer <- ifelse(v_int$Repeat_Customer==1,"Yes","No")
v_int <- v_int[ , -which(names(v_int) %in% c("client_id"))]

# Output data 
v_int <- subset(v_int,v_int$Activated.at>="2019-09-01")
write.xlsx(v_int, "vintage_output.xlsx")

