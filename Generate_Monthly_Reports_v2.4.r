

#########################################
#########################################
#####                               #####
#####    Generate monthly reports   #####
#####                               #####
#########################################
#########################################


####################
# Manual Settings  #
####################
slujebni_pog <- 10038.18
month_index <- 60


############################
# Make connection with SQL #
############################

# Load libraries - part 1
library(RMySQL)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Connect to MySQL
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
data_encoding <- suppressWarnings(dbSendQuery(con, 'set character set "utf8"'))
data_sql <- dbSendQuery(con, "select * from test.data_max_pay_day")
max_pay_day <- fetch(data_sql, n=-1)
data_sql2 <- dbSendQuery(con, "select * from test.data_last_pay_day")
last_pay_day <- fetch(data_sql2, n=-1)
data_sql3 <- dbSendQuery(con, "select * from test.data_deactivated_at")
deactivated <- fetch(data_sql3, n=-1)
data_sql4 <- dbSendQuery(con, "select * from test.data_cessions")
cessions <- fetch(data_sql4, n=-1)
data_sql5 <- dbSendQuery(con, "select * from test.offices_zones")
offices_zones <- fetch(data_sql5, n=-1)

# Set encodings
Encoding(offices_zones$name_office) <- "UTF-8"
Encoding(offices_zones$name_zones) <- "UTF-8"
offices_zones$name_zones <- ifelse(is.na(offices_zones$name_zones),"",
    offices_zones$name_zones)



######################
# Read input files   #
######################

# Read libraries - part 2 
library(openxlsx)
library(gtools)

# Function to calculate previous month's last day
som <- function(x) {as.Date(format(x, "%Y-%m-01"))}

# Set working directory
setwd("C:\\Regular\\R\\input\\")

# Read data
acc <- read.xlsx("Accounting_INPUT.xlsx")
acc_prev <- read.xlsx("Accounting_INPUT_PREVIOUS_MONTH.xlsx")
com <- read.xlsx("Common_INPUT.xlsx")

# Read relevant fields
last_pay_day <- last_pay_day[,2:3]
max_pay_day <- max_pay_day[,2:3]
offices_zones <- offices_zones[,c(1,3:4)]

# Drop column name "Офис при отпускане" from accouting files
acc <- acc[ , -which(names(acc) %in% 
      c("Офис.при.отпускане","Счетоводно.отписан"))]
acc_prev <- acc_prev[ , -which(names(acc_prev) %in% 
      c("Офис.при.отпускане","Счетоводно.отписан"))]
com <- com[ , -which(names(com) %in% c("Трета.страна","Подстатус"))]



#############################################
############# Define Functions ###############
#############################################

months <- function(var){
  output <- ifelse(var=="01","яну",
     ifelse(var=="02","фев",
     ifelse(var=="03","мар",
     ifelse(var=="04","апр",
     ifelse(var=="05","май",
     ifelse(var=="06","юни",
     ifelse(var=="07","юли",
     ifelse(var=="08","авг",
     ifelse(var=="09","сеп",
     ifelse(var=="10","окт",
     ifelse(var=="11","ное",
     ifelse(var=="12","дек",""))))))))))))
  return (output)
}

# Define diapazon roll rate
diapazon <- function(var){
  output <- ifelse(var<=3,"a. до 4",
     ifelse(var<=30,"b. 4-30дни",
     ifelse(var<=60,"c. 30-60",
     ifelse(var<=90,"d. 60-90",
     ifelse(var<=120,"e. 90-120",
     ifelse(var<=150,"f. 120-150",
     ifelse(var<=180,"g. 150-180",
     ifelse(var<=210,"h. 180-210",
     ifelse(var<=240,"i. 210-240",
     ifelse(var<=270,"j. 240-270",
     ifelse(var<=300,"k. 270-300",
     ifelse(var<=330,"l. 300-330",
     ifelse(var<=360,"m. 330-360",
     ifelse(var<=390,"n. 360-390","o. 
                   над 390"))))))))))))))
  return (output)
}


#############################################
############# Accounting File ###############
#############################################

# Correct empty saldo
acc[,30] <- ifelse(is.na(acc[,30]), 0, acc[,30])

# Grupa
acc$група <- ifelse(acc[,31]>180,4,
   ifelse(acc[,31]>90,3,
   ifelse(acc[,31]>30,2,1)))

# Obeztsenka
acc$Обезценка <- ifelse(acc[,36]==1,acc[,30]*0,
   ifelse(acc[,36]==2,acc[,30]*0.1,
   ifelse(acc[,36]==3,acc[,30]*0.5,acc[,30]*1)))



# Correction Obeztsenka for Ipoteki
acc$Обезценка <- ifelse(is.na(acc[,35]), acc$Обезценка,
      ifelse(acc[,35]>acc[,30],0,
      ifelse(acc[,36]==1,(acc[,30]-acc[,35])*0,
      ifelse(acc[,36]==2,(acc[,30]-acc[,35])*0.1,
      ifelse(acc[,36]==3,(acc[,30]-acc[,35])*0.5,
            (acc[,30]-acc[,35])*1)))))


# Portfeil
acc$Портфейл <- acc[,30]


# Nov
acc$Нов <- ifelse(acc[,10]>0,"Да","Не")

# Suma
suma_com <- com[,c(3,8)]
acc <- merge(acc , suma_com, by.x="Номер.на.кредит", 
             by.y="Кредит.№", all.x=TRUE)
names(acc)[ncol(acc)] <- "Suma"

# Diapazon
acc$Diapazon <- ifelse(acc[,40]<=1000,"01 - До 1000 лв.",
    ifelse(acc[,40]<=3000,"02 - От 1 000 до 3 000 лв.",
    ifelse(acc[,40]<=5000,"03 - От 3 000 до 5 000 лв. ",
    ifelse(acc[,40]<=10000,"04 - От 5000 до 10 000 лв.",
                           "05 - Над 10 000 лв."))))

# Last Day of Previous Month
acc$Last_Day <- som(Sys.Date()) - 1
acc$Last_Day <- as.Date(acc$Last_Day)

# Общо за категорията на кредита
acc$Общо.за.категорията.на.кредита <- ifelse(acc[,31]<=30,
            "401- Редовен",
    ifelse(acc[,31]<=90,"402-Под наблюдение",
    ifelse(acc[,31]<=180,
            "403-Необслужван",
            "404-Загуба")))


# Max pay day
acc <- merge(acc , max_pay_day, by.x="Номер.на.кредит", by.y="credit_number", 
             all.x=TRUE)
names(acc)[ncol(acc)] <- "max_pay_day"

# Date difference
acc$Време <- difftime(acc$max_pay_day, acc$Last_Day, units = "days")
acc$Време <- round(acc$Време,0)

# Остатъчен срок
acc$Остатъчен.срок <- ifelse((acc[,45]<=0 & acc[,9]=="Активен"),
                             "4-Просрочие",
    ifelse(acc[,45]<=360,"1-До една година",
    ifelse(acc[,45]<=(360*3),
                              "2-От една до три години",
                              "3-Над три години")))

# Под 360
acc$Под.360 <- ifelse(acc[,31]<=360,"Да","Не")

# Под 720
acc$Под.720 <- ifelse(acc[,31]<=720,"Да","Не")

# Диапазон Брутен Портфейл
acc$Диапазон.Брутен.Портфейл <- ifelse(acc[,31]<=3,"до 3 дни",
      ifelse(acc[,31]<=30,"4-30 дни",
      ifelse(acc[,31]<=90,"31-90 дни",
      ifelse(acc[,31]<=180,"91-180 дни",
                          "над 180 дни"))))

# Дни забава Минал месец
acc_prev_day_delay <- acc_prev[,c(1,31)]
acc <- merge(acc , acc_prev_day_delay, by.x="Номер.на.кредит", 
             by.y="Номер.на.кредит", all.x=TRUE)
names(acc)[ncol(acc)] <- "Дни.забава.Минал.месец"

# Диапазон Roll Rate
acc$Диапазон.Roll.Rate <- diapazon(acc[,31])

# Диапазон Roll Rate
acc$Диапазон.Roll.Rate.Previous <- diapazon(acc[,50])
acc$Диапазон.Roll.Rate.Previous <- ifelse(is.na(acc[,50]), "Нов", 
    acc$Диапазон.Roll.Rate.Previous)

# Салдо главници Минал Месец
acc_glav_minal_mesets <- acc_prev[,c(1,29)]
acc <- merge(acc , acc_glav_minal_mesets, by.x="Номер.на.кредит", 
             by.y="Номер.на.кредит", all.x=TRUE)
names(acc)[ncol(acc)] <- "Салдо.главници.Минал.Месец"

#Last pay day
acc <- merge(acc , last_pay_day, by.x="Номер.на.кредит", 
             by.y="credit_number", all.x=TRUE)
names(acc)[ncol(acc)] <- "Last_Pay_Day"

# Make back-up here for later reports
acc <- acc[ , -which(names(acc) %in% 
                       c("Ликвидационна.стойност.на.обезпечението"))]
acc_raw <- acc

# Output result
setwd("C:\\Regular\\R\\output\\")


###########>>>>>>>>>>>> OUTPUT
write.xlsx(acc, "Accounting_TEMPLATE.xlsx", sheetName="Sheet 1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)



###############################################
################ Roll Rate ####################
###############################################
setwd("C:\\Regular\\R\\RR\\")
roll_rate <- read.xlsx("Roll_Rate.xlsx")


acc_bind <- acc_raw[,c(1,29,31)]
names(acc_bind)[1] <- "credit_number"
names(acc_bind)[2] <- "balance_principal"
names(acc_bind)[3] <- "delays"
prev_last_day <- som(Sys.Date()) - 1
acc_bind$текущ.месец <- paste(substr(prev_last_day, 1, 4), 
                              as.numeric(substr(prev_last_day,6,7)),
                              sep = ".", collapse = NULL)
roll_rate <- smartbind(roll_rate, acc_bind)

# Portfolio
roll_rate$portfolio <- roll_rate$balance_principal

# Grupa
roll_rate$група <- ifelse(roll_rate$delays>180,4,
                          ifelse(roll_rate$delays>90,3,
                                 ifelse(roll_rate$delays>30,2,1)))

# Obezcenka
roll_rate$Обезценка <- ifelse(roll_rate$група==1,roll_rate$portfolio*0,
     ifelse(roll_rate$група==2,roll_rate$portfolio*0.1,
     ifelse(roll_rate$група==3,roll_rate$portfolio*0.5,
                               roll_rate$portfolio*1)))

# Get next delay days
matrix_date <- matrix(unlist(unlist(strsplit(as.character(
  roll_rate$текущ.месец), ".", fixed = TRUE))), ncol=2, byrow=TRUE)
roll_rate$month <- as.numeric(matrix_date[,2])
roll_rate$year <- as.numeric(matrix_date[,1])

roll_rate <- roll_rate[order(roll_rate$credit_number, 
                             roll_rate$year, roll_rate$month),]

for (i in 1:(nrow(roll_rate)-1)){
  if (roll_rate$credit_number[i]==roll_rate$credit_number[i+1]){
    roll_rate$delays.next[i] <- roll_rate$delays[i+1]
  }
  else {
    roll_rate$delays.next[i] = -999
  }
}
roll_rate$delays.next[nrow(roll_rate)] <- -999

# Диапазон Roll Rate
roll_rate$група.забава.текущ.месец <- diapazon(roll_rate$delays)

# Диапазон Roll Rate Next
roll_rate$група.забава.следващ.месец <- ifelse(roll_rate$delays.next==-999, 
        "Нов", diapazon(roll_rate$delays.next))

# Rolling next month
roll_rate$движение.следващ.месец <- ifelse(roll_rate$група.забава.текущ.месец==
        roll_rate$група.забава.следващ.месец,"запазва",
        ifelse(roll_rate$група.забава.текущ.месец>
               roll_rate$група.забава.следващ.месец,"надолу","нагоре"))


###########>>>>>>>>>>>> OUTPUT
# Output result (stack the same file)
write.xlsx(roll_rate, "Roll_Rate.xlsx", sheetName="Sheet 1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)

# Create Tables
table_rr_all <- xtabs(portfolio~група.забава.текущ.месец+текущ.месец, roll_rate)
table_rr_all <- table_rr_all[,c(1,4,5,6,7,8,9,10,11,2,3,
                                12,13,16,17,18,19,20,21,22,23,14,15,
                                24:month_index)]
roll_rate_filt <- subset(roll_rate,roll_rate$движение.следващ.месец=="нагоре")
table_rr_filt <- xtabs(portfolio~група.забава.текущ.месец+текущ.месец, 
                       roll_rate_filt)
table_rr_filt <- table_rr_filt[,c(1,4,5,6,7,8,9,10,11,2,3,
                                  12,13,16,17,18,19,20,21,22,23,14,15,
                                  24:month_index)]
table_pct <- table_rr_filt/table_rr_all
tables_rr <- list(table_rr_all,table_rr_filt,table_pct)


###########>>>>>>>>>>>> OUTPUT
# Output tables
write.xlsx(tables_rr, "Roll_Rate_Tables.xlsx", sheetName="Sheet 1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)



#########################################################
################ Roll Rate Glavnitsi ####################
#########################################################
acc <- acc_raw

table_rr_glavnitsi <- xtabs(acc[,29]~acc[,51]+acc[,50], acc)
table_saldo_prev_month <- xtabs(acc[,52]~acc[,51], acc)
table_saldo_now <- xtabs(acc[,29]~acc[,51], acc)

tables_rr_glavnitsi <- list(table_rr_glavnitsi,table_saldo_prev_month)


###########>>>>>>>>>>>> OUTPUT
# Output tables
setwd("C:\\Regular\\R\\Roll_Rate_Glavnitsi\\")
write.xlsx(tables_rr_glavnitsi , "Roll_Rate_Glavnitsi_Tables.xlsx", 
           sheetName="Sheet 1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)



############################################################
################ Zadarjane ne klienti   ####################
############################################################

# Compute relevant fields (dataframe kept from novi_stari_klienti_refinans report)
com_form <- com
com_form <- merge(com_form , deactivated, by.x="Кредит.№", 
                  by.y="credit_number", all.x=TRUE)
com_form <- com_form[ , -which(names(com_form) %in% c("Вид.портфейл"))]
com_form$deactivated_at <- as.Date(com_form$deactivated_at)
com_form$Ден.Начало <- substring(com_form[,5],1,2)
com_form$Месец.Начало <- substring(com_form[,5],4,5)
com_form$Година.Начало <- substring(com_form[,5],7,10)
com_form <- com_form[order(com_form[,25],com_form[,33],
                           com_form[,32],com_form[,31]),]
com_form$Начало <- paste (com_form$Година.Начало, 
                          com_form$Месец.Начало, 
                          com_form$Ден.Начало, sep = "-", 
                          collapse = NULL)
com_form$Начало <- as.Date(com_form$Начало)
com_form$Стар_Клиент <- 0
com_form$приключил.същия.или.предходен.ден <- 0

# Recompute fields
com_zad <- com_form
com_zad$Под90 <- ifelse(com_zad$Максимален.брой.дни.забави.по.кредита>90,
                        "Не","Да")

com_zad <- com_zad[order(com_zad[,25],com_zad[,30]),]
com_zad$Активиране.следващ <- ""
com_zad$Продукт.следващ <- ""
for (i in 1:(nrow(com_zad)-1)){
  if(com_zad[,25][i+1]==com_zad[,25][i]){
    com_zad$Активиране.следващ[i] <- paste(com_zad$Година.Начало[i+1], 
         com_zad$Месец.Начало[i+1], com_zad$Ден.Начало[i+1], 
              sep = "-", collapse = NULL)
    com_zad$Продукт.следващ[i] <- com_zad$Продукт[i+1]
  }
}

com_zad <- subset(com_zad, com_zad$Статус=="Приключен")

com_zad$deactivated_at <- as.Date(com_zad$deactivated_at, format="%Y-%m-%d")
com_zad$Активиране.следващ <- as.Date(com_zad$Активиране.следващ, 
      format="%Y-%m-%d")
com_zad$difftime <- difftime(com_zad$Активиране.следващ, 
      com_zad$deactivated_at, units = "days")
com_zad$Приключване <- ifelse(is.na(com_zad$deactivated_at),"",
      substring(com_zad$deactivated_at,1,7))
com_zad$Дата.на.активиране <- ifelse(is.na(com_zad$Активиране.следващ),"",
      substring(com_zad$Активиране.следващ,1,7))

for (i in 1:(nrow(com_zad))){
  if(com_zad$Приключване[i]>com_zad$Дата.на.активиране[i]){
    com_zad$Активиране.следващ[i] <- NA
    com_zad$Продукт.следващ[i] <- ""
    com_zad$Дата.на.активиране[i] <- NA
  }
}

names(com_zad)[1] <- "Кредит"
com_zad <- merge(com_zad , cessions, by.x="Кредит", 
                 by.y="credit_number", all.x=TRUE)
names(com_zad)[ncol(com_zad)] <- "Цесия"
com_zad$Цесия <- ifelse(com_zad$Цесия==1,"Да","Не")

com_zad <- com_zad [ , -which(names(com_zad ) %in% c("Офис"))]
com_zad <- merge(com_zad , offices_zones, by.x="Кредит", 
                 by.y="credit_number", all.x=TRUE)
names(com_zad)[(ncol(com_zad)-1)] <- "Офис"
names(com_zad)[ncol(com_zad)] <- "Зона"

com_zad$Офис <- as.character(com_zad$Офис)
com_zad$Зона <- as.character(com_zad$Зона)

com_zad$Офис <- ifelse(com_zad$Офис %in% c("Централен офис",
      "Просрочени ЦО","Вечен дом", "За Продажба", 
      "ЦЕНТРАЛЕН ОФИС-КРЕДИРЕКТ", "МТЕЛ", 
      "За продажба КРЕДИРЕКТ", "EOS MATRIX",
      "Преструктурирани кредити",
      "СЪД-СИТИКЕШ","Загуба", "KRONOS RECOVERY ",
      "Починали","Съдебен отдел","Каса Корпоративна Сигурност"),
      "Централен офис",
      ifelse(com_zad$Офис=="CreDirect","CreDirect",
      ifelse(com_zad$Офис=="Ипотеки","Ипотеки",
      ifelse(com_zad$Офис=="СЪД-КРЕДИРЕКТ","CreDirect",com_zad$Офис))))

com_zad$Зона <- ifelse(com_zad$Офис %in% c("Централен офис","Просрочени ЦО",
      "Вечен дом", "За Продажба",
      "ЦЕНТРАЛЕН ОФИС-КРЕДИРЕКТ", "МТЕЛ", "За продажба КРЕДИРЕКТ",                                      
      "EOS MATRIX","Преструктурирани кредити","СЪД-СИТИКЕШ","Загуба", 
      "KRONOS RECOVERY", "Починали","Съдебен отдел",
      "Каса Корпоративна Сигурност"),"Централен офис",
      ifelse(com_zad$Офис %in% c("CreDirect","CreDirect-Creditour",
      "CreDirect-WebBroker"),"CreDirect",
      ifelse(com_zad$Офис=="Ипотеки","Ипотеки",
      ifelse(com_zad$Офис=="СЪД-КРЕДИРЕКТ","CreDirect",com_zad$Зона))))

com_zad$Продукт.кат <- ifelse(substring(com_zad$Продукт,0,6)=="City W",
                              "City Week",
   ifelse(substring(com_zad$Продукт,0,6)=="City M","City Month",
   ifelse(substring(com_zad$Продукт,0,6)=="City 2","City 2-Week",
   ifelse(substring(com_zad$Продукт,0,6)=="City 2","City 2-Week",
   ifelse(substring(com_zad$Продукт,0,11)=="CreDirect П",
          "Credirect Потребителски",
   ifelse(substring(com_zad$Продукт,0,16) %in% 
          c("CreDirect14 - 0%","CreDirect30 - 0%",
            "CreDirect14 Грат","CreDirect30 Грат","CreDirect14_без ",
            "CreDirect Flex_б","CreDirect Flex_Г",
            "CreDirect Flex Г"), "CrediRect Flex 0%",
   ifelse(substring(com_zad$Продукт,0,11)=="CreDirect14" | 
          substring(com_zad$Продукт,0,11)=="CreDirect30" |
          substring(com_zad$Продукт,0,16)== "CreDirect Flex", "CrediRect Flex",
   ifelse(substring(com_zad$Продукт,0,5)=="Пенси","Пенсионер","Друг"))))))))

com_zad$Продукт.следващ.кат <- ifelse(
  substring(com_zad$Продукт.следващ,0,6)=="City W",
  "City Week",
  ifelse(substring(com_zad$Продукт.следващ,0,6)=="City M","City Month",
  ifelse(substring(com_zad$Продукт.следващ,0,6)=="City 2","City 2-Week",
  ifelse(substring(com_zad$Продукт.следващ,0,6)=="City 2","City 2-Week",
  ifelse(substring(com_zad$Продукт.следващ,0,11)=="CreDirect П",
     "Credirect Потребителски",
  ifelse(substring(com_zad$Продукт.следващ,0,16) %in% c("CreDirect14 - 0%",
     "CreDirect30 - 0%","CreDirect14 Грат","CreDirect30 Грат",
     "CreDirect14_без ","CreDirect Flex_б","CreDirect Flex_Г",
     "CreDirect Flex Г"), "CrediRect Flex 0%",
  ifelse(substring(com_zad$Продукт.следващ,0,11)=="CreDirect14" | 
         substring(com_zad$Продукт.следващ,0,11)=="CreDirect30" |
         substring(com_zad$Продукт.следващ,0,16)== "CreDirect Flex", 
         "CrediRect Flex",
  ifelse(substring(com_zad$Продукт.следващ,0,5)=="Пенси","Пенсионер",
         "Друг"))))))))

com_zad$Продукт.кат <- ifelse(is.na(com_zad$Продукт),"",com_zad$Продукт.кат)
com_zad$Продукт.следващ.кат <- ifelse(com_zad$Продукт.следващ=="","",
                                      com_zad$Продукт.следващ.кат)

com_zad <- com_zad[,c("Кредит","Офис", "Зона", 
    "Максимален.брой.дни.забави.по.кредита","Под90", "Цесия","Приключване", 
    "Дата.на.активиране","Продукт","Продукт.следващ",
    "Продукт.кат","Продукт.следващ.кат")]


###########>>>>>>>>>>>> OUTPUT
setwd("C:\\Regular\\R\\Zadarjane_Klienti")
wb <- loadWorkbook("Zadarjane_Klienti_Output.xlsx")
writeData(wb, sheet = "Data", com_zad, colNames = T)
saveWorkbook(wb,"Zadarjane_Klienti_Output.xlsx", overwrite = T)

