
#######################################################
############# CHECK SCORING PER TIME PERIOD ###########
#######################################################


############ Manual settings ##########

# Choose credirect or citycash
criteria <- "online"
beginning <- "2020-12-01 00:00:00"
end <- "2020-12-31 23:59:59"



############ Define functions ##########

gen_PSI_distribution <- function(var1,var2,bins,flag_continuous){
  if(flag_continuous==1){
    now <- as.data.frame(table(cut(var1,bins)))
    usual <- as.data.frame(table(cut(var2,bins)))
    dataframe <- merge(usual, now, by.x="Var1", by.y="Var1", all.x=TRUE)
  } else{
    now <- as.data.frame(table(var1))
    usual <- as.data.frame(table(var2)) 
    dataframe <- merge(usual, now, by.x="var2", by.y="var1", all.x=TRUE)
  }
  dataframe$Freq.y <- ifelse(is.na(dataframe$Freq.y), 0.1, dataframe$Freq.y)
  dataframe$A <- dataframe$Freq.x/sum(dataframe$Freq.x)
  dataframe$B <- dataframe$Freq.y/sum(dataframe$Freq.y)
  dataframe$result <- (dataframe$A - dataframe$B)*
    log(dataframe$A/dataframe$B)
  return(dataframe)
}
gen_PSI <- function(var1,var2,bins,flag_continuous){
  dataframe <- gen_PSI_distribution(var1,var2,bins,flag_continuous)
  return(sum(dataframe$result))
}


############ Automatic settings ##########

# Call libraries 
library(RMySQL)
library(openxlsx)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Load data
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
  host=db_host, port = df_port)
data_encoding <- suppressWarnings(dbSendQuery(con,'set character set "utf8"'))
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
df_raw <- df

# Subset useful
if(criteria!="all"){
  df <- subset(df, df$online_offline==criteria)
}

# Get usual distribution and see if distribution has changed
modeled_var <- c("age","maturity","gender","ratio_installment_income",
                 "ownership","education","household_children","purpose",
                 "experience_employer","on_address","marital_status",
                 "status_work")
df_rawest <- df

# Compute if new to company
df <- df[order(df$date_entry),]
df <- df[order(df$egn),]
df$has_prev_credit <- 0
for(i in 2:nrow(df)){
  if(df$egn[i]==df$egn[i-1] & df$status[i-1] %in% c(4,5)){
    df$has_prev_credit[i] <- 1
  }
  else if(df$egn[i]==df$egn[i-1] & df$has_prev_credit[i-1]==1){
    df$has_prev_credit[i] <- 1
  }
  else {
    df$has_prev_credit[i] <- 0
  }
}

df_bu <- df
df <- df_bu
             
# Re-subset useful        
df <- subset(df, df$status %in% c(2,4,5))
df <- subset(df, df$date_entry>=beginning & df$date_entry<=end)

# Read CKR and merge
ckr_string_fct <- function(type){
  return(paste("SELECT 
citycash_db.clients_ckr_files.client_id,
citycash_db.clients_ckr_files.created_at as ckr_created_at,
citycash_db.clients_ckr_files_data.status_active
FROM citycash_db.clients_ckr_files_data
INNER JOIN citycash_db.clients_ckr_files
ON citycash_db.clients_ckr_files.id=citycash_db.clients_ckr_files_data.file_id
WHERE citycash_db.clients_ckr_files_data.type=", type, sep=""))
}

# Work on bank ckr
ckr_sql_bank <- suppressWarnings(dbSendQuery(con, ckr_string_fct(1)))
ckr_bank <- fetch(ckr_sql_bank, n=-1) 
df <- merge(df, ckr_bank, by.x = "client_id", by.y = "client_id", all.x = TRUE)
df$difftime_ckr <- abs(as.numeric(round(difftime(df$date_entry, 
  df$ckr_created_at, units=c("days")),0)))
df <- df[order(df$difftime_ckr),]
df <- df[order(df$id),]
df <- df[!duplicated(df$id),]
df <- df[ , -which(names(df) %in% c("difftime_ckr","ckr_created_at"))]
names(df)[ncol(df)] <- "current_status_active_bank"

# Work on bank ckr
ckr_sql_fin <- suppressWarnings(dbSendQuery(con, ckr_string_fct(2)))
ckr_fin <- fetch(ckr_sql_fin, n=-1) 
df <- merge(df, ckr_fin, by.x = "client_id", by.y = "client_id", all.x = TRUE)
df$difftime_ckr <- abs(as.numeric(round(difftime(df$date_entry, 
    df$ckr_created_at, units=c("days")),0)))
df <- df[order(df$difftime_ckr),]
df <- df[order(df$id),]
df <- df[!duplicated(df$id),]
df <- df[ , -which(names(df) %in% c("difftime_ckr","ckr_created_at"))]
names(df)[ncol(df)] <- "current_status_active_fin"

# Make final ckr field
df$current_status_active_bank <- ifelse(is.na(df$current_status_active_bank), 
      -999, df$current_status_active_bank)
df$current_status_active_fin <- ifelse(is.na(df$current_status_active_fin), 
      -999, df$current_status_active_fin)
df$current_status_active_tot <- ifelse(
      df$current_status_active_fin>df$current_status_active_bank,
      df$current_status_active_fin, df$current_status_active_bank)

# Subset based on ckr
df$ЦКР <- ifelse(df$current_status_active_tot==0, "от 0 до 30 дни",
          ifelse(df$current_status_active_tot==71, "от 31 до 60 дни",
          ifelse(df$current_status_active_tot==72, "от 61 до 90 дни",
          ifelse(df$current_status_active_tot==73, "от 91 до 180 дни",
          ifelse(df$current_status_active_tot==74, "от 181 до 360 дни", 
          ifelse(df$current_status_active_tot==75, "над 361 дни", 
                 "няма информация"))))))

# Read scoring table and merge
scoring_sql <- suppressWarnings(dbSendQuery(con, "SELECT 
   application_id, amount, score, period, display_score
   FROM citycash_db.credits_applications_scoring"))
scoring <- fetch(scoring_sql, n=-1)
df <- merge(df, scoring, by.x = c("id","amount","installments"), 
            by.y = c("application_id","amount","period"), 
            all.x = TRUE)
df <- df[!duplicated(df$id),]

# Merge with data
data_sql2 <- suppressMessages(suppressWarnings(dbSendQuery(con, "
SELECT application_id, CASE 
WHEN status_reason=1 THEN 'Лошо ЦКР'
WHEN status_reason=2 THEN 'НОИ трудови договори,осигуровки'
WHEN status_reason=3 THEN 'ТЕЛК'
WHEN status_reason=4 THEN 'Невалиднаповредена лична карта'
WHEN status_reason=5 THEN 'Недопустима възраст'
WHEN status_reason=6 THEN 'Рисков профил'
WHEN status_reason=7 THEN 'Липса на поръчител'
WHEN status_reason=8 THEN 'Вътрешно кредитна история'
WHEN status_reason=9 THEN 'Съмнение за измама'
WHEN status_reason=10 THEN 'Няма контакт с Клиент'
WHEN status_reason=11 THEN 'Скоринг'
WHEN status_reason=12 THEN 'Активен Ситикеш'
WHEN status_reason=13 THEN 'Цедиран'
WHEN status_reason=14 THEN 'Повторно въведена молба'
WHEN status_reason=15 THEN 'Активен Credirect'
WHEN status_reason=16 THEN 'Необслужван район'
WHEN status_reason=17 THEN 'Няма данни в ЦКР'
WHEN status_reason=18 THEN 'По преценка на КИ'
WHEN status_reason=19 THEN 'Клиента се отказва - сума'
WHEN status_reason=20 THEN 'Клиента се отказва - оскъпяване'
WHEN status_reason=21 THEN 'Клиента се отказва - време'
WHEN status_reason=22 THEN 'Клиента се отказва - не желае да рабо'
WHEN status_reason=23 THEN 'Свързаност с лош КЛ'
WHEN status_reason=24 THEN 'Отказ от РО'
WHEN status_reason=25 THEN 'Грешенневалиден телефон на КЛ'
WHEN status_reason=27 THEN 'Вътрешно кредитна история'
WHEN status_reason=28 THEN 'Няма имот на негово име'
WHEN status_reason=29 THEN 'Не предоставя лица за контакт'
WHEN status_reason=30 THEN 'Безработен'
WHEN status_reason=31 THEN 'Няма контакт с КЛ- изключен телефон'
WHEN status_reason=32 THEN 'Няма контакт с КЛ- вдига друго лице'
WHEN status_reason=33 THEN 'Няма контакт с КЛ- свободно'
WHEN status_reason=34 THEN 'КЛ не предоставя ЛК'
WHEN status_reason=35 THEN 'Недопустима възраст на клиента'
WHEN status_reason=51 THEN 'Технически проблем'
WHEN status_reason=52 THEN 'Контакт с клиент'
WHEN status_reason=53 THEN 'Лице за контакт'
WHEN status_reason=54 THEN 'Становище'
WHEN status_reason=55 THEN 'Не е посетен на адрес'
WHEN status_reason=56 THEN 'Допълване на данни'
WHEN status_reason=57 THEN 'Няма файлове'
WHEN status_reason=58 THEN 'ОВ'
WHEN status_reason=59 THEN 'АМ/КС'
END as decline_reason
FROM citycash_db.credits_applications_decisions")))
reason_decline <- fetch(data_sql2, n=-1)

# Merge with data
df <- merge(df, reason_decline, by.x = "id", by.y = "application_id", 
            all.x = TRUE)

# Reshape columns
df$has_prev_credit <- ifelse(df$has_prev_credit==1, "Да", "Не")
df$final_status <- ifelse(df$status==4,"Активен",
                   ifelse(df$status==5,"Приключен",
                   ifelse(df$sub_status==111,"В одобрение",
                   ifelse(df$sub_status==118,"Отказан от клиент",
                   ifelse(df$sub_status==119,"Отказан от РО",
                   ifelse(df$sub_status==120,"Отказан от КИ",
                   ifelse(df$sub_status==132,"Неконтакнат",
                   ifelse(df$sub_status==136,"Отказан от КМ","Друго"))))))))
Encoding(df$product_name) <- "UTF-8"
df_raw <- df

# Remove rejected by KM
df <- df_raw
df <- subset(df, is.na(df$sub_status) | df$sub_status!=136)

# Select relevant columns
df <- df[,c("credit_number","id","score","product_name","display_score",
            "date_entry","has_prev_credit","ЦКР","sub_status",
            "final_status","decline_reason")]

# Rename fields 
names(df) <- c("Кредит_номер","ID","Скор","Продукт","Дисплей Скор",
  "Дата","Пореден","ЦКР","Статус","Резултат","Причина_отказ")

# Table with decline reasons for Application
tab_dec_app <- as.data.frame.matrix(table(df$Причина_отказ[df$Пореден=="Не" 
      & !(df$Резултат %in% c("Активен","Приключен"))],
      df$Скор[df$Пореден=="Не" & !(df$Резултат %in% c("Активен","Приключен"))], 
      exclude=NULL))
tab_dec_app <- tab_dec_app[,c(1,6,2:5)]
tab_dec_app$Total <- rowSums(tab_dec_app)
tab_dec_app <- tab_dec_app[rev(order(tab_dec_app$Total)),]

# Table with decline reasons for Repeat
tab_dec_rep <- as.data.frame.matrix(table(df$Причина_отказ[df$Пореден=="Да" 
     & !(df$Резултат %in% c("Активен","Приключен"))],
     df$Скор[df$Пореден=="Да" & !(df$Резултат %in% c("Активен","Приключен"))], 
     exclude=NULL))
tab_dec_rep <- tab_dec_rep[,c(1,6,2:5)]
tab_dec_rep$Total <- rowSums(tab_dec_rep)
tab_dec_rep <- tab_dec_rep[rev(order(tab_dec_rep$Total)),]

# Acceptance rate detailed for repeat
subs <- subset(df,df$Резултат %in% c("Отказан от КИ","Активен",
    "Приключен") & df$Пореден=="Да")
subs <- subset(subs,subs$Продукт %in% 
    names(table(subs$Продукт)[table(subs$Продукт)>50]))
subs <- merge(subs,df_raw[,c("id","egn")],by.x = "ID",by.y = "id",all.x = TRUE)
subs_dec <- subset(subs,subs$Резултат=="Отказан от КИ")
subs_dec <- subs_dec[!duplicated(subs_dec$egn),]
subs_acc <- subset(subs,subs$Резултат!="Отказан от КИ")
subs_acc <- subs_acc[!duplicated(subs_acc$egn),]
subs <- rbind(subs_dec,subs_acc)
acceptance_rates <- as.data.frame(
  table(subs$Продукт,subs$Скор)/rowSums(table(subs$Продукт,subs$Скор)))
acceptance_rates_rep <- 
  acceptance_rates[acceptance_rates$Var2 %in% c("Bad","NULL"),c("Var1","Freq")]
acceptance_rates_rep <- 
  aggregate(acceptance_rates_rep$Freq,by=list(acceptance_rates_rep$Var1),
            FUN=sum)
acceptance_rates_rep$x <- round(1-acceptance_rates_rep$x,2)
names(acceptance_rates_rep) <- c("product","acceptance_rate")

# Acceptance rate detailed for repeat
subs <- subset(df,df$Резултат %in% c("Отказан от КИ","Активен",
    "Приключен") & df$Пореден=="Не")
subs <- subset(subs,subs$Продукт %in% 
    names(table(subs$Продукт)[table(subs$Продукт)>50]))
subs <- merge(subs,df_raw[,c("id","egn")],by.x = "ID",by.y = "id",all.x = TRUE)
#subs <- subset(subs,subs$Скор!="NULL")
subs_dec <- subset(subs,subs$Резултат=="Отказан от КИ")
subs_dec <- subs_dec[!duplicated(subs_dec$egn),]
subs_acc <- subset(subs,subs$Резултат!="Отказан от КИ")
subs_acc <- subs_acc[!duplicated(subs_acc$egn),]
subs <- rbind(subs_dec,subs_acc)
acceptance_rates <- as.data.frame(
  table(subs$Продукт,subs$Скор)/rowSums(table(subs$Продукт,subs$Скор)))
acceptance_rates_new <- 
  acceptance_rates[acceptance_rates$Var2 %in% c("Bad","NULL"),c("Var1","Freq")]
acceptance_rates_new <- 
  aggregate(acceptance_rates_new$Freq,by=list(acceptance_rates_new$Var1),
  FUN=sum)
acceptance_rates_new$x <- round(1-acceptance_rates_new$x,2)
names(acceptance_rates_new) <- c("product","acceptance_rate")

# Get PSI of modeled variables
if(criteria=="online"){
  result_PSI <- as.data.frame(cbind(
    gen_PSI(df_raw$age,df_rawest$age,c(-Inf,24,29,37,45,Inf),1),
    gen_PSI(df_raw$ownership,df_rawest$ownership,c(),0),
    gen_PSI(df_raw$education,df_rawest$education,c(),0),
    gen_PSI(df_raw$marital_status,df_rawest$marital_status,c(),0),
    gen_PSI(df_raw$maturity,df_rawest$maturity,
            c(-Inf,1.99,3.99,6.99,12.99,Inf),1),
    gen_PSI(df_raw$gender,df_rawest$gender,c(),0),
    gen_PSI(df_raw$ratio_installment_income,df_rawest$ratio_installment_income,
            c(-Inf,0.130,0.206,0.325,0.567,Inf),1),
    gen_PSI(df_raw$household_children,df_rawest$household_children,
            c(-Inf,0,1,Inf),1),
    gen_PSI(df_raw$purpose,df_rawest$purpose,c(),0),
    gen_PSI(df_raw$status_work,df_rawest$status_work,c(),0),
    gen_PSI(df_raw$marital_status,df_rawest$marital_status,c(),0),
    gen_PSI(df_raw$experience_employer,df_rawest$experience_employer,
            c(-Inf,3.99,10.99,25.99,57.99,Inf),1),
    gen_PSI(df_raw$on_address,df_rawest$on_address,
            c(-Inf,24.99,120.99,240.99,349.99,Inf),1)))
} else {
  result_PSI <- as.data.frame(cbind(
    gen_PSI(df_raw$age,df_rawest$age,c(-Inf,28.99,36.99,43.99,52.99,Inf),1),
    gen_PSI(df_raw$ownership,df_rawest$ownership,c(),0),
    gen_PSI(df_raw$education,df_rawest$education,c(),0),
    gen_PSI(df_raw$marital_status,df_rawest$marital_status,c(),0),
    gen_PSI(df_raw$maturity,df_rawest$maturity,
            c(-Inf,2.99,3.97,5.13,8.17,Inf),1),
    gen_PSI(df_raw$gender,df_rawest$gender,c(),0),
    gen_PSI(df_raw$ratio_installment_income,df_rawest$ratio_installment_income,
            c(-Inf,0.075,0.104,0.138,0.193,Inf),1),
    gen_PSI(df_raw$household_children,df_rawest$household_children,
            c(-Inf,0,1,Inf),1),
    gen_PSI(df_raw$purpose,df_rawest$purpose,c(),0),
    gen_PSI(df_raw$status_work,df_rawest$status_work,c(),0),
    gen_PSI(df_raw$marital_status,df_rawest$marital_status,c(),0),
    gen_PSI(df_raw$experience_employer,df_rawest$experience_employer,
            c(-Inf,3.99,10.99,24.99,72.99,Inf),1),
    gen_PSI(df_raw$on_address,df_rawest$on_address,
            c(-Inf,12.99,50.99,204.99,360.99,Inf),1)))
}
names(result_PSI) <- c("age","ownership","education","marital_status",
                       "maturity","gender","ratio_installment_income",
                       "household_children","purpose","status_work",
                       "marital_status","experience_employer","on_address")

# Get PSI of scorecards
if(criteria!="all"){
  scoring_cur <- merge(as.data.frame(
    round(table(df$Скор[df$Пореден=="Не" & !(df$Скор %in% c("NULL","Bad"))])/
            nrow(df[df$Пореден=="Не" & !(df$Скор %in% c("NULL","Bad")),]),3)),
    as.data.frame(
      round(table(df$Скор[df$Пореден=="Да" & !(df$Скор %in% c("NULL","Bad"))])/
              nrow(df[df$Пореден=="Да" & !(df$Скор %in% c("NULL","Bad")),]),3)),
    by.x = "Var1",by.y = "Var1",all.x = TRUE)
  if(criteria=="offline"){
    scoring_norm <- as.data.frame(cbind(
      c("Indeterminate","Good 1","Good 2","Good 3","Good 4"),
      c(0.16,0.21,0.25,0.25,0.13),c(0.06,0.13,0.19,0.28,0.35)))
  } else {
    scoring_norm <- as.data.frame(cbind(
      c("Indeterminate","Good 1","Good 2","Good 3","Good 4"),
      c(0.21,0.19,0.28,0.18,0.14),c(0.16,0.19,0.20,0.18,0.27)))
  }
  scoring_PSI <- merge(scoring_cur,scoring_norm,by.x = "Var1",by.y = "V1",
                       all.x = TRUE)
  names(scoring_PSI) <- c("score","app_curr","rep_curr","app_norm","rep_norm")
  scoring_PSI$app_norm <- as.numeric(as.character(scoring_PSI$app_norm))
  scoring_PSI$rep_norm <- as.numeric(as.character(scoring_PSI$rep_norm))
  scoring_PSI$Index_app <-  (scoring_PSI$app_cur - scoring_PSI$app_norm) *
    log(scoring_PSI$app_cur/scoring_PSI$app_norm)
  scoring_PSI$Index_rep <-  (scoring_PSI$rep_cur - scoring_PSI$rep_norm) *
    log(scoring_PSI$rep_cur/scoring_PSI$rep_norm)
  scoring_PSI <- as.data.frame(cbind(round(sum(
    scoring_PSI$Index_app),3),round(sum(scoring_PSI$Index_rep),3)))
  names(scoring_PSI) <- c("PSI_scorecard_app","PSI_scorecard_repeat")
} else{
  scoring_PSI <- NA
}


# Output file
setwd("C:\\Projects\\Apply_Scoring")
output_name <- paste("Scoring_from_DB_",criteria,"-",
                     as.character(Sys.time()),".xlsx",sep="")
output_name <- gsub(":", "-", output_name)
output_name <- gsub(" ", "_", output_name)
OUT <- createWorkbook()
addWorksheet(OUT, "Time Window")
addWorksheet(OUT, "Acceptance Rate New")
addWorksheet(OUT, "Acceptance Rate Repeat")
addWorksheet(OUT, "PSI variables")
addWorksheet(OUT, "PSI scorecard")
addWorksheet(OUT, "Raw data")
addWorksheet(OUT, "Decline Reasons App")
addWorksheet(OUT, "Decline Reasons Rep")
writeData(OUT, sheet = "Time Window", x = cbind(beginning,end), 
          rowNames = FALSE)
writeData(OUT, sheet = "Acceptance Rate New", x = acceptance_rates_new, 
          rowNames = FALSE)
writeData(OUT, sheet = "Acceptance Rate Repeat", x = acceptance_rates_rep, 
          rowNames = FALSE)
writeData(OUT, sheet = "PSI variables", x = result_PSI, rowNames = FALSE)
writeData(OUT, sheet = "PSI scorecard", x = scoring_PSI, rowNames = FALSE)
writeData(OUT, sheet = "Raw data", x = df)
writeData(OUT, sheet = "Decline Reasons App", x = tab_dec_app,  
          rowNames = TRUE)
writeData(OUT, sheet = "Decline Reasons Rep", x = tab_dec_rep,  
          rowNames = TRUE)
saveWorkbook(OUT, output_name)


#######
# End #
#######

