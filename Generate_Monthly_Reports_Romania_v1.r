

#####################################################
#####################################################
#####                                           #####
#####    Generate monthly reports for ROMANIA   #####
#####                                           #####
#####################################################
#####################################################


####################
# Manual Settings  #
####################
slujebni_pog <- 0
month_index <- 0



############################
# Make connection with SQL #
############################

# Read libraries
library(RMySQL)
library(openxlsx)
library(openxlsx)
library(gtools)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "flexcredit"
db_host <- "127.0.0.1"
df_port <- 3306

# Set working directory
setwd("C:\\Regular_Romania\\R\\input")

# Read database
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
data_encoding <- suppressWarnings(dbSendQuery(con, 'set character set "utf8"'))
credits <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id, credit_number, amount, activated_at 
FROM flexcredit.loans
WHERE activated_at IS NOT NULL")), n=-1)

max_pay_day <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT loan_id, max(pay_day) as max_pay_day
FROM flexcredit.loan_daily_plan
GROUP BY loan_id")), n=-1)

credits <- merge(credits, max_pay_day, by.x = "id", 
                 by.y = "loan_id", all.x = TRUE)




######################
# Read input files   #
######################

# Function to calculate previous month's last day
som <- function(x) {as.Date(format(x, "%Y-%m-01"))}
sopm <- function(x) {as.Date(format(x-35, "%Y-%m-01"))}

# Read data
acc <- read.xlsx("Accounting_INPUT.xlsx")
acc_prev <- read.xlsx("Accounting_INPUT_PREVIOUS_MONTH.xlsx")



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
            ifelse(var<=390,"n. 360-390","o. над 390"))))))))))))))
  return (output)
}


#############################################
############# Accounting File ###############
#############################################

# Correct empty saldo
acc[,23] <- ifelse(is.na(acc[,23]), 0, acc[,23])

# Grupa
acc$група <- ifelse(acc[,24]>180,4,
             ifelse(acc[,24]>90,3,
             ifelse(acc[,24]>30,2,1)))

# Obeztsenka
acc$Обезценка <- ifelse(acc[,28]==1,acc[,23]*0,
                 ifelse(acc[,28]==2,acc[,23]*0.1,
                 ifelse(acc[,28]==3,acc[,23]*0.5,acc[,23]*1)))

# Portfeil
acc$Портфейл <- acc[,23]
                        
                        
# Nov 
acc$Activated.at <- as.Date(paste(substring(acc$Activated.at,7,10),
                                   substring(acc$Activated.at,4,5),
                                   substring(acc$Activated.at,1,2), sep="-"))
acc$Нов <- ifelse(as.Date(acc$Activated.at)>=as.Date(sopm(Sys.Date())),
                  "Да","Не")

# Suma 
acc <- merge(acc , credits[,c("credit_number","amount")] , 
  by.x="Credit.number", by.y="credit_number", all.x=TRUE)
names(acc)[ncol(acc)] <- "Suma"

# Diapazon 
acc$Diapazon <- ifelse(acc[,32]<=1000,"01 - До 1000 лв.",
                ifelse(acc[,32]<=3000,"02 - От 1 000 до 3 000 лв.",
                ifelse(acc[,32]<=5000,"03 - От 3 000 до 5 000 лв. ",
                ifelse(acc[,32]<=10000,"04 - От 5000 до 10 000 лв.","05 - Над 10 000 лв."))))

# Last Day of Previous Month
acc$Last_Day <- som(Sys.Date()) - 1
acc$Last_Day <- as.Date(acc$Last_Day)

# Общо за категорията на кредита
acc$Общо.за.категорията.на.кредита <- ifelse(acc[,24]<=30,"401- Редовен",
                                      ifelse(acc[,24]<=90,"402-Под наблюдение",
                                      ifelse(acc[,24]<=180,"403-Необслужван","404-Загуба")))

# Max pay day 
acc <- merge(acc , credits[,c("credit_number","max_pay_day")], 
             by.x="Credit.number", by.y="credit_number", 
             all.x=TRUE)
acc$max_pay_day <- as.Date(acc$max_pay_day, origin="1899-12-30")


# Date difference
acc$Време <- difftime(acc$max_pay_day, acc$Last_Day, units = "days")
acc$Време <- round(acc$Време,0)

# Остатъчен срок
acc$Остатъчен.срок <- ifelse((acc$Време<=0 & acc[,6]=="Активен"),"4-Просрочие",
                      ifelse(acc$Време<=360,"1-До една година",
                      ifelse(acc$Време<=(360*3),"2-От една до три години",
                             "3-Над три години")))

# Под 360
acc$Под.360 <- ifelse(acc[,24]<=360,"Да","Не")

# Под 720
acc$Под.720 <- ifelse(acc[,24]<=720,"Да","Не")

# Диапазон Брутен Портфейл
acc$Диапазон.Брутен.Портфейл <- ifelse(acc[,24]<=3,"до 3 дни",
                                ifelse(acc[,24]<=30,"4-30 дни",
                                ifelse(acc[,24]<=90,"31-90 дни",
                                ifelse(acc[,24]<=180,"91-180 дни","над 180 дни"))))

# Дни забава Минал месец
acc_prev_day_delay <- acc_prev[,c(1,24)]
acc <- merge(acc , acc_prev_day_delay, by.x="Credit.number", 
             by.y="Credit.number", all.x=TRUE)
names(acc)[ncol(acc)] <- "Дни.забава.Минал.месец"

# Диапазон Roll Rate
acc$Диапазон.Roll.Rate <- diapazon(acc[,24])

# Диапазон Roll Rate
acc$Диапазон.Roll.Rate.Previous <- diapazon(acc$Дни.забава.Минал.месец)
acc$Диапазон.Roll.Rate.Previous <- ifelse(is.na(acc$Дни.забава.Минал.месец), "Нов", 
                                          acc$Диапазон.Roll.Rate.Previous)

# Салдо главници Минал Месец
acc_glav_minal_mesets <- acc_prev[,c(1,22)]
acc <- merge(acc , acc_glav_minal_mesets, by.x="Credit.number", 
             by.y="Credit.number", all.x=TRUE)
names(acc)[ncol(acc)] <- "Салдо.главници.Минал.Месец"

# Make back-up here for later reports
acc_raw <- acc

# Output result
setwd("C:\\Regular_Romania\\R\\output")


###########>>>>>>>>>>>> OUTPUT
write.xlsx(acc, "Accounting_TEMPLATE.xlsx", sheetName="Sheet 1", 
            col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)

