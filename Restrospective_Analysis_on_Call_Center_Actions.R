
#################################################
######## Check actions retrospectively   ########
#################################################

########### Define functions

# Define safe ifelse
safe.ifelse <- function(cond, yes, no){ class.y <- class(yes)
 X <- ifelse(cond, yes, no)
 class(X) <- class.y; return(X)}

plan_filt <-  function(time,lag){
  input <- subset(plan,plan$days_delay>=time)
  input <- input[order(input$pay_day),]
  input <- input[order(input$application_id),]
  input <- input[!duplicated(input$application_id),]
  input$begin <- as.Date(input$pay_day) + time
  input$end <- as.Date(input$pay_day) + time + lag
  result <- input
  return(result)
}




########### Make connection to DB

# Set seed
set.seed(15)

# Call libraries 
suppressWarnings(library(RMySQL))
suppressWarnings(library(openxlsx))

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Load data
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
  host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
df_raw <- df




########### Build action data

# Read actions table
actions <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM citycash_db.clients_actions")), n=-1)

# Join results table
results <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id, call_type, resultable_type, resultable_id, type
FROM citycash_db.results
")), n=-1)
actions <- merge(actions,results,by.x = "actionable_id",by.y = "id",
    all.x = TRUE)

# Remove too old actions
actions <- subset(actions,actions$created_at>="2019-01-01")

# Make month field
actions$month <- substring(actions$created_at,1,7) 

# Choose columns
actions <- actions[,c("application_id","client_id","created_at","actionable_id",
                            "created_by","type","call_type","actionable_type",
                            "resultable_type")]

# Join job description
users <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id, position_id
FROM citycash_db.users")), n=-1)
positions <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id, name
FROM citycash_db.positions")), n=-1)
users <- merge(users,positions,by.x = "position_id",by.y = "id",all.x = TRUE)
Encoding(users$name) <- "UTF-8"
names(users)[3] <- "position_name"
actions <- merge(actions,users[,c("id","position_name")],
   by.x = "created_by",by.y = "id",all.x = TRUE)

# Remove certin job positions
actions <- subset(actions,is.na(actions$position_name) | 
 !(actions$position_name %in% c("Корпоративна сигурност",
 "Кредитен инспектор","Рекламни и оперативни материали CITYCASH")))

# Simply job position name
actions$position_short <- ifelse(is.na(actions$position_name),"sms", 
 ifelse(substring(actions$position_name,1,8)=="Оператор","call_center",
 ifelse(actions$position_name %in% c("Мениджър Кол център Продажби",
        "Супервайзър Кол център"),"call_center","office")))

# Make dataframe with actions only from call center
actions_cc <- subset(actions,actions$position_short=="call_center")

# Make additional fields
actions_cc$result_type <- 
  ifelse(actions_cc$type %in% c(14,40,39,15),"not_found",
  ifelse(actions_cc$type %in% c(35),"doesnt_want_pay",
  ifelse(actions_cc$type %in% c(37),"no_promise",
  ifelse(actions_cc$type %in% c(38),"promises",
  ifelse(actions_cc$type %in% c(34),"promises",
  ifelse(actions_cc$type %in% c(24),"other",
  ifelse(actions_cc$type %in% c(10),"other",
  ifelse(actions_cc$type %in% c(33),"other",
  ifelse(actions_cc$type %in% c(36),"other","other")))))))))





########### Choose credits to look into 

# Choose time window 
df <- subset(df,df$online_offline=="offline")
df <- subset(df,df$status %in% c(4))
df <- subset(df,is.na(df$sub_status) | df$sub_status!=122)
df <- subset(df,df$date>="2019-01-01")

# Remove finstart and ipoteki
df <- subset(df,!(df$product_id %in% c(3,13,12,65,51,54,53)))

# Read credits plan 
plan <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, installment_num, pay_day,
days_delay, payed_at , pmt_final 
FROM ",db_name,".credits_plan_main", sep="")), n=-1))
plan_raw <- plan

# Get only those selected
plan <- plan[plan$application_id %in% df$id,]

# Select only past installments
#plan <- subset(plan,plan$pay_day<=Sys.time())
plan <- subset(plan,plan$pay_day<"2020-12-01")

# Subset only defaults
plan_def <- subset(plan,is.na(plan$payed_at) & plan$days_delay>=90)
plan_def <- merge(plan_def,df[,c("id","client_id")],
                  by.x = "application_id",by.y = "id",all.x = TRUE)

# Treat actions and get last action
actions_cc_spec <- actions_cc[actions_cc$application_id %in% 
        plan_def$application_id,]
last_action <- actions_cc_spec[rev(order(actions_cc_spec$created_at)),]
last_action <- last_action[order(last_action$application_id),]
last_action <- last_action[!duplicated(last_action$application_id),]

# Make final dataframe with unique application IDs
final <- plan_def[rev(order(plan_def$days_delay)),]
final <- final[order(final$application_id),]
final <- final[!duplicated(final$application_id),]

# Get last action by call center
final <- merge(final,last_action[,c("application_id",
  "created_at","result_type")],
  by.x = "application_id",by.y = "application_id",all.x = TRUE)
final$time_since <- as.numeric(round(difftime(as.Date("2020-12-01"),
  final$created_at,units = c("days")),2))
final$time_since_bin <- 
  ifelse(final$time_since<=7,"1) [0,7]",
  ifelse(final$time_since<=14,"2) [8,14]",
  ifelse(final$time_since<=30,"3) [15,30]",
  ifelse(final$time_since<=90,"4) [31,90]","5) [90,Inf)"))))

# Set and choose final fields
final$days_delay_bin <- 
  ifelse(final$days_delay<=120,"1) [90,120]",
  ifelse(final$days_delay<=150,"2) [121,150]",
  ifelse(final$days_delay<=180,"3) [151,180]",
  ifelse(final$days_delay<=240,"4) [181,240]","5) [241,Inf)"))))
final <- final[,c("application_id","installment_num","days_delay","created_at",
                  "time_since","time_since_bin","days_delay_bin","result_type")]



########### Add more aggregate data to main dataframe

# Get current office
final <- merge(final,df[,c("id","office_id")],by.x = "application_id",
               by.y = "id",all.x = TRUE)
offices <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id,name
FROM citycash_db.structure_offices")), n=-1)
Encoding(offices$name) <- "UTF-8"
final <- merge(final,offices,by.x = "office_id",by.y = "id",all.x = TRUE)
names(final)[ncol(final)] <- "office"
final$office_real <- ifelse(!(final$office) %in% c("Централен офис",
"СЪД-СИТИКЕШ","Просрочени ЦО","Полиция","Каса Корпоративна Сигурност"),
"real_office",final$office)

# Get when became third side 
third_side <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT id, third_side_date
FROM ",db_name,".credits_applications", sep="")), n=-1))
final <- merge(final,third_side ,by.x = "application_id",by.y = "id",
               all.x = TRUE)
final$time_third_state <- safe.ifelse(is.na(final$third_side_date),
  as.numeric(NA),
  as.numeric(round(difftime(as.Date("2020-12-01"),
  final$third_side_date,units = c("days")),2)))
final$time_third_state <- ifelse(is.na(final$time_third_state),NA,
  ifelse(final$time_third_state<0,NA,final$time_third_state))
final$third_state_bin <- 
  ifelse(is.na(final$time_third_state),"none",
  ifelse(final$time_third_state<=30,"1) [0,30]",
  ifelse(final$time_third_state<=60,"2) [31,60]",
  ifelse(final$time_third_state<=90,"3) [61,90]",
  ifelse(final$time_third_state<=180,"4) [91,180]","5) [90,Inf)")))))

# Remove some outlier cases
final <- subset(final,!(is.na(final$time_since_bin)))

# Make statistics
table(final$days_delay_bin,final$time_since_bin)
table(final$third_state_bin,final$time_since_bin)
table(final$office_real,final$time_since_bin)




########### Join bucket information

# Read bucket information
bucket <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT application_id, bucket_id, created_at
FROM citycash_db.call_center_buckets_credits")), n=-1)
bucket_info <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id, type FROM citycash_db.call_center_buckets")), n=-1)
bucket <- merge(bucket,bucket_info,by.x = "bucket_id",by.y = "id",all.x = TRUE)
bucket <- bucket[bucket$application_id %in% final$application_id,]

final_merg <- merge(final,bucket[,c("application_id","type","created_at")],
               by.x = "application_id",by.y = "application_id",all.x = TRUE)
final_merg$bucket <- ifelse(final_merg$type==1,"CityCash_10_29days",
  ifelse(final_merg$type==2,"CityCash_30_59days",
  ifelse(final_merg$type==3,"CityCash_60_89days",
  ifelse(final_merg$type==4,"CityCash_over_90days",
  ifelse(final_merg$type==5,"Third_Side",
  ifelse(final_merg$type==6,"Overdue","other"))))))
final_merg <- final_merg[,-which(names(final_merg) %in% c("type"))]
names(final_merg)[c(5,15)] <- c("created_at","bucket_entry")

final_merg <- subset(final_merg,final_merg$bucket_entry<final_merg$created_at)
final_merg <- final_merg[rev(order(final_merg$bucket_entry)),]
final_merg <- final_merg[order(final_merg$application_id),]
final_merg <- final_merg[!duplicated(final_merg$application_id),]

final <- merge(final,final_merg[,c("application_id","bucket")],
               by.x = "application_id",by.y = "application_id",all.x = TRUE)


# Check judicial and third side
final$third_side_binary <- ifelse(final$third_state_bin=="none","no","yes")

final2 <- subset(final,final$third_side_binary=="no")
table(final2$days_delay_bin,final2$time_since_bin,final2$bucket)



########### Focus on 60_89 days

spec <- subset(final,is.na(final$bucket))
spec$result_type_bin <- ifelse(spec$result_type=="not_found",
                               "not_found","other")





