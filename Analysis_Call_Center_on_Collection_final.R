
##########################################################################
########       Generate analysis and statistics for building      ########
########   relation between call center actions and payments      ########
##########################################################################


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




########### Read and build raw data

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

# Choose time window 
df <- subset(df,df$date>="2019-06-01" &  df$date<="2019-12-31")
df <- subset(df,df$online_offline=="offline")
df <- subset(df,df$status %in% c(4,5))
df <- subset(df,is.na(df$sub_status) | df$sub_status!=122)

# Read actions table
actions <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM citycash_db.clients_actions")), n=-1)

# Remove too old actions
actions <- subset(actions,actions$created_at>="2019-01-01")

# Make month field
actions$month <- substring(actions$created_at,1,7) 
plot(table(actions$month))

# Join results table
results <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id, call_type, resultable_type, resultable_id, type
FROM citycash_db.results
")), n=-1)
actions <- merge(actions,results,by.x = "actionable_id",by.y = "id",
                 all.x = TRUE)

# Read credits plan 
plan <- suppressWarnings(fetch(dbSendQuery(con, 
paste("SELECT application_id, installment_num, pay_day,
days_delay, payed_at , pmt_final 
FROM ",db_name,".credits_plan_main", sep="")), n=-1))
plan_raw <- plan

# Select from credits plan main credits with exact DPD (hit for first time)
plan_90 <- plan_filt(90,30)
plan_90 <- plan_90[plan_90$application_id %in% df$id,]
plan_120 <- plan_filt(120,30)
plan_120 <- plan_120[plan_120$application_id %in% df$id,]
plan_150 <- plan_filt(150,30)
plan_150 <- plan_150[plan_150$application_id %in% df$id,]
plan_180 <- plan_filt(180,30)
plan_180 <- plan_180[plan_180$application_id %in% df$id,]
plan_210 <- plan_filt(210,60)
plan_210 <- plan_210[plan_210$application_id %in% df$id,]
plan_270 <- plan_filt(270,90)
plan_270 <- plan_270[plan_270$application_id %in% df$id,]

# Get payments
paid <- fetch(data_sql <- suppressWarnings(dbSendQuery(con, 
"SELECT object_id, amount, created_at AS pay_date,id 
FROM citycash_db.cash_flow
WHERE nomenclature_id in (90,100,101) 
AND deleted_at IS NULL AND object_type=4")), n=-1)
paid_raw <- paid

# Get taxes
taxes <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM citycash_db.credits_plan_taxes
WHERE tax_id NOT IN (4,22)")), n=-1)
taxes_raw <- taxes




########### Make analysis dataframe and/or create fields of relevant variables

# Take actions only with selected application_id
actions_df <- actions[actions$application_id %in% plan_90$application_id,]

# Choose columns
actions_df <- actions_df[,c("application_id","client_id","created_at",
  "created_by","type","call_type","actionable_type",
  "resultable_type")]

# Make additional fields
actions_df$result_type <- 
  ifelse(actions_df$type %in% c(14,40,39,15),"not_found",
  ifelse(actions_df$type %in% c(35),"doesnt_want_pay",
  ifelse(actions_df$type %in% c(37),"no_promise",
  ifelse(actions_df$type %in% c(38),"promises",
  ifelse(actions_df$type %in% c(34),"promises",
  ifelse(actions_df$type %in% c(24),"other",
  ifelse(actions_df$type %in% c(10),"other",
  ifelse(actions_df$type %in% c(33),"other",
  ifelse(actions_df$type %in% c(36),"other","other")))))))))

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
actions_df <- merge(actions_df,users[,c("id","position_name")],
                    by.x = "created_by",by.y = "id",all.x = TRUE)

# Remove certin job pisitions
actions_df <- subset(actions_df,is.na(actions_df$position_name) | 
  !(actions_df$position_name %in% c("Корпоративна сигурност",
  "Кредитен инспектор","Рекламни и оперативни материали CITYCASH")))

# Simply job position name
actions_df$position_short <- ifelse(is.na(actions_df$position_name),"sms", 
  ifelse(substring(actions_df$position_name,1,8)=="Оператор","call_center",
  ifelse(actions_df$position_name %in% c("Мениджър Кол център Продажби",
   "Супервайзър Кол център"),"call_center","office")))

# More complex position name with result
actions_df$action_result <- paste(actions_df$position_short,
  actions_df$result_type,sep="_")
actions_df$action_result <- ifelse(actions_df$position_short=="sms","sms",
  actions_df$action_result)


########### Choose dataframe to aggregate 

# Choose bucket dataset to assess
input <- plan_150


# Remove those who are Third Side (which call center never call)
judicial <- fetch(suppressWarnings(dbSendQuery(con, 
"SELECT id, third_side_date
FROM citycash_db.credits_applications")), n=-1)
input <- merge(input,judicial,by.x = "application_id",by.y = "id",all.x = TRUE)
input$third_side_date <- ifelse(is.na(input$third_side_date),NA,
  ifelse(input$third_side_date<=input$end,input$third_side_date,NA))
input$third_side <- ifelse(is.na(input$third_side_date),0,1)
input <- subset(input,input$third_side==0)
input <- input[,-which(names(input) %in% c("third_side_date","third_side"))]




########### Aggreate actions prior to payments  

# Get payments during a 30-days time span after hitting specific DPD
paid_here <- paid_raw
paid_here <- paid_here[paid_here$object_id %in% input$application_id,]
paid_here <- merge(paid_here,input[,c("application_id","begin","end")],
   by.x = "object_id",by.y = "application_id",all.x = TRUE)
paid_here <- subset(paid_here,paid_here$pay_date>=paid_here$begin & 
   paid_here$pay_date<=paid_here$end)
paid_here_raw <- paid_here

# Join actions
paid_here <- merge(paid_here,
  actions_df[,c("application_id","created_at","position_short",
                "action_result")],
  by.x = "object_id",by.y = "application_id",all.x = TRUE)
paid_here$difftime <- as.numeric(difftime(
  paid_here$pay_date,paid_here$created_at,units = c("days")))

# Filter actions which are only 7 days prior to payment
paid_here <- subset(paid_here,paid_here$difftime>0 & paid_here$difftime<=14)

# Only leave one payment per application_id
first_payment <- as.data.frame(aggregate(paid_here$id,
    by=list(paid_here$object_id),FUN=min))
paid_here <- paid_here[paid_here$id %in% first_payment$x,]

# Make action columns
paid_here$call_center <- ifelse(paid_here$position_short=="call_center",1,0)
paid_here$office <- ifelse(paid_here$position_short=="office",1,0)
paid_here_raw2 <- paid_here

# Aggregate call center and office actions by application_id
agg_paid_here <- merge(aggregate(paid_here$call_center,
    by=list(paid_here$object_id),FUN=sum),aggregate(paid_here$office,
    by=list(paid_here$object_id),FUN=sum),by.x = "Group.1",by.y = "Group.1",
    all.x = TRUE)
names(agg_paid_here) <- c("id","call_center","office")

# Make new column : combination of call center and office 
agg_paid_here$action_bin <- 
  ifelse(agg_paid_here$call_center==0 & agg_paid_here$office==0,
         "1) none",
  ifelse(agg_paid_here$call_center==1 & agg_paid_here$office==0,
         "2) CC = 1 & office = 0",
  ifelse(agg_paid_here$call_center==0 & agg_paid_here$office==1,
         "3) CC = 0 & office = 1",
  ifelse(agg_paid_here$call_center==1 & agg_paid_here$office==1,
         "4) CC = 1 & office = 1",
  ifelse(agg_paid_here$call_center>=2 & agg_paid_here$office==0,
         "5) CC > 1 & office = 0",
  ifelse(agg_paid_here$call_center==0 & agg_paid_here$office>=2,
         "6) CC = 0 & office > 1",
         "7) CC >=1 & office >= 1"))))))

# Get those who paid but didn't receive action 
agg_paid_here_left <- as.data.frame(paid_here_raw$object_id[!(
  paid_here_raw$object_id %in% agg_paid_here$id)])
names(agg_paid_here_left) <- c("id")
agg_paid_here_left <- as.data.frame(agg_paid_here_left[!duplicated(
  agg_paid_here_left$id),])
names(agg_paid_here_left) <- c("id")
if(nrow(agg_paid_here_left)>0) {
  names(agg_paid_here_left) <- c("id")
  agg_paid_here_left$call_center <- 0
  agg_paid_here_left$office <- 0
  agg_paid_here_left$action_bin <- "1) none"
  agg_paid_here <- rbind(agg_paid_here_left,agg_paid_here)
}

agg_paid_here$action_simple <- ifelse(agg_paid_here$action_bin=="1) none",0,1)
agg_paid_here_raw <- agg_paid_here


# Make final stats on payments/actions
result_paid <- as.data.frame(table(agg_paid_here$action_bin))





########### Aggreate actions prior to none payments   

# Check those who did not pay
no_paid <- input[!(input$application_id %in% agg_paid_here$id),]
no_paid$random_diff <- round(runif(nrow(no_paid), min=1, max=30))
no_paid$random_date <- as.Date(no_paid$begin) + no_paid$random_diff
no_paid <- no_paid[,c("application_id","random_date")]
no_paid_raw <- no_paid
no_paid  <- merge(no_paid ,
  actions_df[,c("application_id","created_at","position_short",
                "action_result")],
  by.x = "application_id",by.y = "application_id",all.x = TRUE)
no_paid$difftime <- as.numeric(difftime(
  no_paid$random_date,no_paid$created_at,units = c("days")))
no_paid <- subset(no_paid,no_paid$difftime>-0.45 & no_paid$difftime<=14)

# Make action columns
no_paid$call_center <- ifelse(no_paid$position_short=="call_center",1,0)
no_paid$office <- ifelse(no_paid$position_short=="office",1,0)
no_paid_raw2 <- no_paid

# Aggregate
agg_no_paid_here <- merge(aggregate(no_paid$call_center,
 by=list(no_paid$application_id),FUN=sum),aggregate(no_paid$office,
 by=list(no_paid$application_id),FUN=sum),by.x = "Group.1",by.y = "Group.1",
                       all.x = TRUE)
names(agg_no_paid_here) <- c("id","call_center","office")
agg_no_paid_here$action_bin <- 
  ifelse(agg_no_paid_here$call_center==0 & agg_no_paid_here$office==0,
         "1) none",
  ifelse(agg_no_paid_here$call_center==1 & agg_no_paid_here$office==0,
         "2) CC = 1 & office = 0",
  ifelse(agg_no_paid_here$call_center==0 & agg_no_paid_here$office==1,
         "3) CC = 0 & office = 1",
  ifelse(agg_no_paid_here$call_center==1 & agg_no_paid_here$office==1,
         "4) CC = 1 & office = 1",
  ifelse(agg_no_paid_here$call_center>=2 & agg_no_paid_here$office==0,
         "5) CC > 1 & office = 0",
  ifelse(agg_no_paid_here$call_center==0 & agg_no_paid_here$office>=2,
         "6) CC = 0 & office > 1",
         "7) CC >=1 & office >= 1"))))))

# Get those who did not paid and didn't receive action 
agg_no_paid_here_left <- as.data.frame(no_paid_raw$application_id[!(
  no_paid_raw$application_id %in% agg_no_paid_here$id)])
if(nrow(agg_no_paid_here_left)>0) {
  names(agg_no_paid_here_left) <- c("id")
  agg_no_paid_here_left$call_center <- 0
  agg_no_paid_here_left$office <- 0
  agg_no_paid_here_left$action_bin <- "1) none"
  agg_no_paid_here <- rbind(agg_no_paid_here_left,agg_no_paid_here)
}


# Make final stats on payments
result_no_paid <- as.data.frame(table(agg_no_paid_here$action_bin))




########### Make final dataframe 

final_result <- merge(result_no_paid,result_paid,by.x = "Var1",
                      by.y = "Var1",all.x = TRUE)
names(final_result) <- c("action","not_paid","paid")
final_result$ratio <- round(final_result$paid / 
    (final_result$paid + final_result$not_paid),2)
View(final_result)




############ Subset only actions with 2) CC = 1 & office = 0 and see its result

subs_paid <- subset(agg_paid_here,
  agg_paid_here$action_bin=="2) CC = 1 & office = 0")
subs_no_paid <- subset(agg_no_paid_here,
  agg_no_paid_here$action_bin=="2) CC = 1 & office = 0")

subs_paid <- merge(subs_paid,paid_here_raw2[,c("object_id","action_result")],
                   by.x = "id",by.y = "object_id",all.x = TRUE)
subs_paid <- subs_paid[!duplicated(subs_paid$id),]
subs_no_paid <- merge(subs_no_paid,
                   no_paid_raw2[,c("application_id","action_result")],
                   by.x = "id",by.y = "application_id",all.x = TRUE)
subs_no_paid <- subs_no_paid[!duplicated(subs_no_paid$id),]
final_result_subs <- merge(as.data.frame(table(subs_no_paid$action_result)),
                           as.data.frame(table(subs_paid$action_result)),
                           by.x = "Var1",by.y = "Var1",all.x = TRUE)
names(final_result_subs) <- c("action-result","not_paid","paid")
final_result_subs$ratio <- round(final_result_subs$paid/
      (final_result_subs$paid+final_result_subs$not_paid),2)
View(final_result_subs)




############ Subset only actions with 5) CC > 1 & office = 0 and see its result
subs_paid <- subset(agg_paid_here,
  agg_paid_here$action_bin=="5) CC > 1 & office = 0")
subs_no_paid <- subset(agg_no_paid_here,
  agg_no_paid_here$action_bin=="5) CC > 1 & office = 0")

subs_paid <- merge(subs_paid,paid_here_raw2[,c("object_id","action_result",
  "created_at")],by.x = "id",by.y = "object_id",all.x = TRUE)
subs_no_paid <- merge(subs_no_paid,no_paid_raw2[,c("application_id",
  "action_result","created_at")],by.x = "id",by.y = "application_id",
  all.x = TRUE)

# Check counts of number of CC actions
uniques <- subs_paid[!duplicated(subs_paid$id),]
table(uniques$call_center)

# Choose counts of number of CC actions = 2
subs_paid_2 <- subset(subs_paid,subs_paid$call_center==2)
subs_no_paid_2 <- subset(subs_no_paid,subs_no_paid$call_center==2)

# Make groups
subs_paid_2 <- subs_paid_2[order(subs_paid_2$created_at),]
subs_paid_2 <- subs_paid_2[order(subs_paid_2$id),]
subs_paid_2$group <- NA
for(i in 1:(nrow(subs_paid_2)-1)){
  subs_paid_2$group[i] <- paste(subs_paid_2$action_result[i],
                                subs_paid_2$action_result[i+1],sep="_")
}
subs_paid_2 <- subs_paid_2[!duplicated(subs_paid_2$id),]

subs_no_paid_2 <- subs_no_paid_2[order(subs_no_paid_2$created_at),]
subs_no_paid_2 <- subs_no_paid_2[order(subs_no_paid_2$id),]
subs_no_paid_2$group <- NA
for(i in 1:(nrow(subs_no_paid_2)-1)){
  subs_no_paid_2$group[i] <- paste(subs_no_paid_2$action_result[i],
                                   subs_no_paid_2$action_result[i+1],sep="_")
}
subs_no_paid_2 <- subs_no_paid_2[!duplicated(subs_no_paid_2$id),]

View(table(subs_paid_2$group))
View(table(subs_no_paid_2$group))




# Check why some clients get targeted and others not
check <- agg_paid_here_raw
check <- check[!duplicated(check$id),]
check <- check[,c("id","action_simple")]
check <- merge(check,df_raw[,c("id","date","product_id","zone_id","amount")],
               by.x = "id",by.y = "id",all.x = TRUE)
check$month <- substring(check$date,1,7)
check$amount_bin <- ifelse(check$amount<=350,"100_350",
                    ifelse(check$amount<=600,"400_600",
                    ifelse(check$amount<=1000,"650_1000","1050_more")))
table(check$amount_bin,check$action_simple)



########### End ############


