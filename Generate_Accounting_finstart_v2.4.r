

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
slujebni_pog <- 0


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
data_sql <- dbSendQuery(con, "select * from test.data_max_pay_day")
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
data_sql2 <- dbSendQuery(con, "select * from test.data_last_pay_day")
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
data_sql3 <- dbSendQuery(con, "select * from test.data_deactivated_at")
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
data_sql4 <- dbSendQuery(con, "select * from test.data_cessions")
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
data_sql5 <- dbSendQuery(con, "select * from test.offices_zones")

# Read all necessary tables directly from database
max_pay_day <- fetch(data_sql, n=-1)
last_pay_day <- fetch(data_sql2, n=-1)
deactivated <- fetch(data_sql3, n=-1)
cessions <- fetch(data_sql4, n=-1)
offices_zones <- fetch(data_sql5, n=-1)
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
acc_report <- read.xlsx("Accounting_Report_Input_finstart.xlsx")
com <- read.xlsx("Common_INPUT.xlsx")

# Read relevant fields
last_pay_day <- last_pay_day[,2:3]
max_pay_day <- max_pay_day[,2:3]
offices_zones <- offices_zones[,c(1,3:4)]

# Keep office "Finstart" which is a separate company
acc <- subset(acc,acc$����=="��������")
acc_prev <- subset(acc_prev,acc_prev$����=="��������")

# Drop column name "���� ��� ���������" from accouting files
acc <- acc[ , -which(names(acc) %in% 
      c("����.���.���������","����������.�������"))]
acc_prev <- acc_prev[ , -which(names(acc_prev) %in% 
      c("����.���.���������","����������.�������"))]
com <- com[ , -which(names(com) %in% c("�����.������","���������"))]



#############################################
############# Define Functions ###############
#############################################

months <- function(var){
  output <- ifelse(var=="01","���",
     ifelse(var=="02","���",
     ifelse(var=="03","���",
     ifelse(var=="04","���",
     ifelse(var=="05","���",
     ifelse(var=="06","���",
     ifelse(var=="07","���",
     ifelse(var=="08","���",
     ifelse(var=="09","���",
     ifelse(var=="10","���",
     ifelse(var=="11","���",
     ifelse(var=="12","���",""))))))))))))
  return (output)
}

# Define diapazon roll rate
diapazon <- function(var){
  output <- ifelse(var<=3,"a. �� 4",
     ifelse(var<=30,"b. 4-30���",
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
                   ��� 390"))))))))))))))
  return (output)
}


#############################################
############# Accounting File ###############
#############################################

# Correct empty saldo
acc[,30] <- ifelse(is.na(acc[,30]), 0, acc[,30])

# Grupa
acc$����� <- ifelse(acc[,31]>180,4,
   ifelse(acc[,31]>90,3,
   ifelse(acc[,31]>30,2,1)))

# Obeztsenka
acc$��������� <- ifelse(acc[,36]==1,acc[,30]*0,
   ifelse(acc[,36]==2,acc[,30]*0.1,
   ifelse(acc[,36]==3,acc[,30]*0.5,acc[,30]*1)))



# Correction Obeztsenka for Ipoteki
acc$��������� <- ifelse(is.na(acc[,35]), acc$���������,
      ifelse(acc[,35]>acc[,30],0,
      ifelse(acc[,36]==1,(acc[,30]-acc[,35])*0,
      ifelse(acc[,36]==2,(acc[,30]-acc[,35])*0.1,
      ifelse(acc[,36]==3,(acc[,30]-acc[,35])*0.5,
            (acc[,30]-acc[,35])*1)))))


# Portfeil
acc$�������� <- acc[,30]


# Nov
acc$��� <- ifelse(acc[,10]>0,"��","��")

# Suma
suma_com <- com[,c(3,8)]
acc <- merge(acc , suma_com, by.x="�����.��.������", 
             by.y="������.�", all.x=TRUE)
names(acc)[ncol(acc)] <- "Suma"

# Diapazon
acc$Diapazon <- ifelse(acc[,40]<=1000,"01 - �� 1000 ��.",
    ifelse(acc[,40]<=3000,"02 - �� 1 000 �� 3 000 ��.",
    ifelse(acc[,40]<=5000,"03 - �� 3 000 �� 5 000 ��. ",
    ifelse(acc[,40]<=10000,"04 - �� 5000 �� 10 000 ��.",
                           "05 - ��� 10 000 ��."))))

# Last Day of Previous Month
acc$Last_Day <- som(Sys.Date()) - 1
acc$Last_Day <- as.Date(acc$Last_Day)

# ���� �� ����������� �� �������
acc$����.��.�����������.��.������� <- ifelse(acc[,31]<=30,
            "401- �������",
    ifelse(acc[,31]<=90,"402-��� ����������",
    ifelse(acc[,31]<=180,
            "403-�����������",
            "404-������")))


# Max pay day
acc <- merge(acc , max_pay_day, by.x="�����.��.������", by.y="credit_number", 
             all.x=TRUE)
names(acc)[ncol(acc)] <- "max_pay_day"

# Date difference
acc$����� <- difftime(acc$max_pay_day, acc$Last_Day, units = "days")
acc$����� <- round(acc$�����,0)

# ��������� ����
acc$���������.���� <- ifelse((acc[,45]<=0 & acc[,9]=="�������"),
                             "4-���������",
    ifelse(acc[,45]<=360,"1-�� ���� ������",
    ifelse(acc[,45]<=(360*3),
                              "2-�� ���� �� ��� ������",
                              "3-��� ��� ������")))

# ��� 360
acc$���.360 <- ifelse(acc[,31]<=360,"��","��")

# ��� 720
acc$���.720 <- ifelse(acc[,31]<=720,"��","��")

# �������� ������ ��������
acc$��������.������.�������� <- ifelse(acc[,31]<=3,"�� 3 ���",
      ifelse(acc[,31]<=30,"4-30 ���",
      ifelse(acc[,31]<=90,"31-90 ���",
      ifelse(acc[,31]<=180,"91-180 ���",
                          "��� 180 ���"))))

# ��� ������ ����� �����
acc_prev_day_delay <- acc_prev[,c(1,31)]
acc <- merge(acc , acc_prev_day_delay, by.x="�����.��.������", 
             by.y="�����.��.������", all.x=TRUE)
names(acc)[ncol(acc)] <- "���.������.�����.�����"

# �������� Roll Rate
acc$��������.Roll.Rate <- diapazon(acc[,31])

# �������� Roll Rate
acc$��������.Roll.Rate.Previous <- diapazon(acc[,50])
acc$��������.Roll.Rate.Previous <- ifelse(is.na(acc[,50]), "���", 
    acc$��������.Roll.Rate.Previous)

# ����� �������� ����� �����
acc_glav_minal_mesets <- acc_prev[,c(1,29)]
acc <- merge(acc , acc_glav_minal_mesets, by.x="�����.��.������", 
             by.y="�����.��.������", all.x=TRUE)
names(acc)[ncol(acc)] <- "�����.��������.�����.�����"

#Last pay day
acc <- merge(acc , last_pay_day, by.x="�����.��.������", 
             by.y="credit_number", all.x=TRUE)
names(acc)[ncol(acc)] <- "Last_Pay_Day"

# Make back-up here for later reports
acc <- acc[ , -which(names(acc) %in% 
                       c("�������������.��������.��.�������������"))]
acc_raw <- acc

# Output result
setwd("C:\\Regular\\R\\output\\")



###############################################
############# Accounting Report ###############
###############################################

# Make table of obetsenka for accounting report 
table_��������� <- aggregate(acc$���������, 
                             by=list(�����=acc$�����), FUN=sum)
names(table_���������)[ncol(table_���������)] <- "���������"
table_��������� <- cbind(table_���������,aggregate(
  acc$��������, by=list(�����=acc$�����),  FUN=sum))
names(table_���������)[ncol(table_���������)] <- "��������"
table_��������� <- table_���������[,c(1,2,4)]

# Make a row for the current month
acc_report <- rbind(acc_report,0)

# Compute this month's values
acc_report[nrow(acc_report),1] <- 
  paste(months(substring(som(Sys.Date()) - 1,6,7)),
       substring(som(Sys.Date()) - 1,3,4),sep=".")
acc_report[nrow(acc_report),2] <- sum(acc[,30])
acc_report[nrow(acc_report),3] <- sum(acc[,10])
acc_report[nrow(acc_report),4] <- 0
acc_report[nrow(acc_report),5] <- slujebni_pog 
acc_report[nrow(acc_report),6] <- sum(ifelse(is.na(acc[,14]),0,acc[,14]))
acc_report[nrow(acc_report),4] <- sum(acc_report[nrow(acc_report),6],
                                      -acc_report[nrow(acc_report),5])
acc_report[nrow(acc_report),7] <- sum(ifelse(is.na(acc[,11]),0,acc[,11]))
acc_report[nrow(acc_report),8] <- sum(ifelse(is.na(acc[,12]),0,acc[,12]))
acc_report[nrow(acc_report),9] <- sum(ifelse(is.na(acc[,13]),0,acc[,13]))
acc_report[nrow(acc_report),10] <- sum(ifelse(is.na(acc[,15]),0,acc[,15]))
acc_report[nrow(acc_report),11] <- sum(ifelse(is.na(acc[,16]),0,acc[,16]))
acc_report[nrow(acc_report),12] <- sum(ifelse(is.na(acc[,17]),0,acc[,17]))
acc_report[nrow(acc_report),13] <- sum(ifelse(is.na(acc[,18]),0,acc[,18]))
acc_report[nrow(acc_report),14] <- acc_report[nrow(acc_report),10] + 
  acc_report[nrow(acc_report),11] + acc_report[nrow(acc_report),12] + 
  acc_report[nrow(acc_report),13] 
acc_report[nrow(acc_report),15] <- sum(table_���������[,2])
acc_report[nrow(acc_report),16] <- acc_report[nrow(acc_report),15] - 
  acc_report[nrow(acc_report)-1,15]
acc_report[nrow(acc_report),17] <- acc_report[nrow(acc_report),16] + 
  acc_report[nrow(acc_report),5]
acc_report[nrow(acc_report),18] <- acc_report[nrow(acc_report)-1,18] + 
  acc_report[nrow(acc_report),9] - acc_report[nrow(acc_report),10]
acc_report[nrow(acc_report),19] <- acc_report[nrow(acc_report)-1,19] + 
  acc_report[nrow(acc_report),8] - acc_report[nrow(acc_report),11]
acc_report[nrow(acc_report),20] <- acc_report[nrow(acc_report)-1,20] + 
  acc_report[nrow(acc_report),7] - acc_report[nrow(acc_report),12]
acc_report[nrow(acc_report),21] <- acc_report[nrow(acc_report)-1,21] + 
  acc_report[nrow(acc_report),3] - acc_report[nrow(acc_report),13]
acc_report[nrow(acc_report),22] <- acc_report[nrow(acc_report),18] + 
  acc_report[nrow(acc_report),19] + acc_report[nrow(acc_report),20] + 
  acc_report[nrow(acc_report),21]
acc_report[nrow(acc_report),23] <- acc_report[nrow(acc_report)-1,23] + 
  acc_report[nrow(acc_report),6] - acc_report[nrow(acc_report),14]
acc_report[nrow(acc_report),24] <- acc_report[nrow(acc_report),23] - 
  acc_report[nrow(acc_report)-1,23]
acc_report[nrow(acc_report),25] <- acc_report[nrow(acc_report),2] - 
  acc_report[nrow(acc_report),22]
acc_report[nrow(acc_report),26] <- acc_report[nrow(acc_report),25] - 
  acc_report[nrow(acc_report)-1,25]
acc_report[nrow(acc_report),26] <- acc_report[nrow(acc_report),25] - 
  acc_report[nrow(acc_report)-1,25]
acc_report[nrow(acc_report),27] <- acc_report[nrow(acc_report),7] + 
  acc_report[nrow(acc_report),8] + acc_report[nrow(acc_report),9] + 
  acc_report[nrow(acc_report),26]
acc_new <- subset(acc,acc$���=="��")
acc_report[nrow(acc_report),28] <- 
  sum(ifelse(is.na(acc_new[,11]),0,acc_new[,11])) + 
  sum(ifelse(is.na(acc_new[,12]),0,acc_new[,12])) + 
  sum(ifelse(is.na(acc_new[,13]),0,acc_new[,13]))
acc_report[nrow(acc_report),29] <- sum(ifelse(is.na(acc_new[,14]),0,acc_new[,14]))
acc_report[nrow(acc_report),30] <- sum(ifelse(is.na(acc[,26]),0,acc[,26]))
acc_report[nrow(acc_report),31] <- sum(ifelse(is.na(acc[,27]),0,acc[,27]))
acc_report[nrow(acc_report),32] <- sum(ifelse(is.na(acc[,28]),0,acc[,28]))
acc_report[nrow(acc_report),33] <- sum(ifelse(is.na(acc[,29]),0,acc[,29]))
acc_report[nrow(acc_report),34] <- acc_report[nrow(acc_report),30] + 
  acc_report[nrow(acc_report),31] + acc_report[nrow(acc_report),32] + 
  acc_report[nrow(acc_report),33]
acc_report[nrow(acc_report),35] <- sum(ifelse(is.na(acc[,20]),0,acc[,20]))
acc_report[nrow(acc_report),36] <- acc_report[nrow(acc_report),18] - 
  acc_report[nrow(acc_report),30]
acc_report[nrow(acc_report),37] <- acc_report[nrow(acc_report),19] - 
  acc_report[nrow(acc_report),31]
acc_report[nrow(acc_report),38] <- acc_report[nrow(acc_report),20] - 
  acc_report[nrow(acc_report),32]
acc_report[nrow(acc_report),39] <- acc_report[nrow(acc_report),21] - 
  acc_report[nrow(acc_report),33]
acc_report[nrow(acc_report),40] <- acc_report[nrow(acc_report),36] + 
  acc_report[nrow(acc_report),37] + acc_report[nrow(acc_report),38] + 
  acc_report[nrow(acc_report),39]
acc_report[nrow(acc_report),41] <- acc_report[nrow(acc_report),23] - 
  acc_report[nrow(acc_report),35]


###########>>>>>>>>>>>> OUTPUT
write.xlsx(acc_report, "Accounting_Report_Output_1_finstart.xlsx", 
           sheetName="Sheet 1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
write.xlsx(table_���������, "Accounting_Report_Output_2_finstart.xlsx", 
           sheetName="Sheet 1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)

