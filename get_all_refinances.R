

################################################################
############## GET ALL REFINANCES TRUE AND HIDDEN ##############
################################################################


############## Read data and set fields ##############

# Select time window (Included)
beginning <- "2013-01-01"
end <- "2020-01-01"

# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Read database
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
    dbname=db_name, host=db_host, port = df_port)
data_sql1 <- suppressWarnings(dbSendQuery(con, 
"SELECT *
FROM test.data_final"))
df <- fetch(data_sql1, n=-1)

# Subset fields
df <- subset(df, df$status %in% c(4,5))



##################### Get hidden refinances ###################

# Compute fields
df$online <- ifelse(df$online_offline=="online",1,0)
df$offline <- ifelse(df$online_offline=="offline",1,0)
df$offline_cum <-0
df$online_cum <-0
df$default_cum <-0
df$credits_cum <-0
df$refinance_cum <-0
df$cession_cum <- 0
df$days_diff_last_credit <- NA
df$prev_credit <- NA
df$amount_prev <- NA
df$maturity_ratio <- NA
df$prev_online <- NA
df$prev_refinanced <- NA

df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1] & df$status[i-1]==5) 
  {df$offline_cum[i] <- df$offline[i]+df$offline_cum[i-1]
  df$online_cum[i] <- df$online[i]+df$online_cum[i-1]
  }
  else if (df$egn[i]==df$egn[i-1]) {
    df$offline_cum[i] <- df$offline_cum[i-1]
    df$online_cum[i] <- df$online_cum[i-1]
  }
  if (df$egn[i]==df$egn[i-1]){
    df$default_cum[i] <- df$default_flag[i-1]+df$default_cum[i-1]
    df$days_diff_last_credit[i] <- as.numeric(as.Date(df$date[i]) - 
         as.Date(df$deactivated_at[i-1],format="%Y-%m-%d"))
    df$cession_cum[i] <- df$cession_cum[i-1] + df$cession[i-1]
    df$prev_credit[i] <- df$credit_number[i-1]
    df$amount_prev[i] <- df$amount[i-1]
    df$maturity_ratio[i] <- df$maturity[i]/df$maturity[i-1]
    df$prev_online[i] <- ifelse(df$online[i-1]==1, 1, 0)
    df$prev_refinanced[i] <- ifelse(df$sub_status[i-1]==126, 1, 0)
  }
  df$credits_cum[i] <- df$online_cum[i] + df$offline_cum[i]
}


# Flag if previous is refinance or not 
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_previous <- 0
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_previous <- ifelse(abs(df$days_diff_last_credit)<=1,1,0)
df$refinance_previous <- ifelse(is.na(df$refinance_previous), 0, 
                                df$refinance_previous)

# Cumulative refinance
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]
df$refinance_cum <- 0
for (i in 2:(nrow(df))) {
  if (df$egn[i]==df$egn[i-1]) 
  {
    df$refinance_cum[i] <- df$refinance_previous[i]+df$refinance_cum[i-1]
  }
}

# Get flag of last payment
data_sql3 <- suppressWarnings(dbSendQuery(con, 
  "SELECT object_id AS application_id, amount, pay_date 
FROM citycash_db.cash_flow
WHERE nomenclature_id in (90,100,101) 
AND deleted_at IS NULL AND object_type=4"))
paids <- fetch(data_sql3, n=-1)
paids <- merge(paids, df[,c("id","credit_number","egn")], 
               by.x = "application_id", by.y = "id" , all.x = TRUE) 


paids <- paids[with(paids, rev(order(pay_date))), ]
paids <- paids[with(paids, order(credit_number)), ]
paids1 <- paids[!duplicated(paids$credit_number),]
paids1 <- paids1[,c("credit_number","pay_date")]
paids2 <- merge(paids, paids1, by.x = c("credit_number","pay_date"), 
                by.y = c("credit_number","pay_date"))
paid_last_day <- aggregate(paids2$amount, 
                           by=list(paids2$credit_number), FUN=sum)
names(paid_last_day) <- c("credit_number","amount_paid_last_day")

paid_last_day <- merge(paid_last_day, df[,c("credit_number","total_amount")], 
                       by.x = "credit_number", 
                       by.y = "credit_number", 
                       all.x = TRUE)
paid_last_day$ratio_last_paid <- paid_last_day$amount_paid_last_day / 
  paid_last_day$total_amount
paid_last_day <- paid_last_day[,
      c("credit_number","amount_paid_last_day","ratio_last_paid")]
df <- merge(df, paid_last_day, by.x = "prev_credit", 
            by.y = "credit_number", all.x = TRUE)

# Reorder
df <- df[with(df, order(date)), ]
df <- df[with(df, order(egn)), ]

# Set fields
df$days_diff_last_credit <- ifelse(df$days_diff_last_credit<0,0,
                                   df$days_diff_last_credit)
df$flag_high_last_paid <- ifelse(df$days_diff_last_credit %in% c(0,1) & 
                                   df$ratio_last_paid>=0.5, 1, 0)

all <- df[,c("credit_number","id","amount_prev","amount")]
df_backup <- df


# Select real refinances #
real_refs <- subset(df, df$prev_refinanced==1)
real_refs <- real_refs[,c("credit_number","id")]


# Select fake refinances
fake_refs <- subset(df, df$flag_high_last_paid==1)
fake_refs <- fake_refs[,c("credit_number","id")]




##################### Make global dataframe and output ###################

all <- merge(all, real_refs, by.x = "credit_number",
             by.y = "credit_number", all.x = TRUE)
all <- merge(all, fake_refs, by.x = "credit_number",
             by.y = "credit_number", all.x = TRUE)
names(all) <- c("credit_number","id", "amount_prev","amount",
                "real_refinance","fake_refinance")
all$flag_refinance <- 
  ifelse(!is.na(all$real_refinance) | !is.na(all$fake_refinance),
         "Да", "Не")
all$real_refinance <- ifelse(!is.na(all$real_refinance),
         "Да", "Не")
all$fake_refinance <- ifelse(!is.na(all$fake_refinance),
                             "Да", "Не")
write.xlsx(all,
"C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\all_refinances.xlsx")



