

# Library
library(RMySQL)
library(openxlsx)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306


con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)

app_sql <- paste("SELECT id, credit_number, client_id 
FROM citycash_db.credits_applications
WHERE status IN (4,5)",sep="")
app <- suppressWarnings(fetch(dbSendQuery(con, app_sql ), n=-1))

phones_sql <- paste("SELECT client_id, number, updated_at
FROM citycash_db.clients_phones WHERE preferred=1",sep="")
phones <- suppressWarnings(fetch(dbSendQuery(con, phones_sql), n=-1))

df <- merge(app, phones, by.x = "client_id", by.y = "client_id",
            all.x = TRUE)
df <- df[rev(order(df$updated_at)),]
df <- df[order(df$id),]
df <- df[!duplicated(df$id),]
names(df)[4] <- "pers_number_1"

phones_sql <- paste("SELECT client_id, number, updated_at
FROM citycash_db.clients_phones WHERE (preferred<>1 OR preferred IS NULL OR 
preferred='') AND deleted_at IS NULL",sep="")
phones <- suppressWarnings(fetch(dbSendQuery(con, phones_sql), n=-1))
df <- merge(df, phones, by.x = "client_id", by.y = "client_id",
            all.x = TRUE)
df <- df[rev(order(df$updated_at.y)),]
df <- df[order(df$id),]
df <- df[!duplicated(df[c(3,6)]),]
names(df)[6] <- "pers_number_2"
df <- df[,c(1,2,3,4,6)]

# Personal number 2
df$pers_number_3 <- NA
for (i in (1:(nrow(df)-1))){
  if(df$id[i]==df$id[i+1]){
    df$pers_number_3[i] <- df$pers_number_2[i+1]
  }
}
df <- df[!duplicated(df$id),]

df_bu <- df
df <- df_bu

# Contact phones
contact_sql <- paste("SELECT client_id, phone
FROM citycash_db.clients_partners",sep="")
contact <- suppressWarnings(fetch(dbSendQuery(con, contact_sql), n=-1))
df <- merge(df, contact, by.x = "client_id", by.y = "client_id",
            all.x = TRUE)
names(df)[ncol(df)] <- "partner_number"


# Contact phones
contact_sql <- paste("SELECT client_id, phone
FROM citycash_db.clients_contact_persons",sep="")
contact <- suppressWarnings(fetch(dbSendQuery(con, contact_sql), n=-1))
df <- merge(df, contact, by.x = "client_id", by.y = "client_id",
            all.x = TRUE)
names(df)[ncol(df)] <- "contact_number"

# Output
setwd("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output")
write.xlsx(df,"all_phone_numbers.xlsx")




