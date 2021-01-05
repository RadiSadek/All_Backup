
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
app <- suppressWarnings(fetch(dbSendQuery(con, app_sql), n=-1))

app_client_sql <- paste("SELECT egn, application_id 
FROM citycash_db.credits_applications_clients",sep="")
app_client <- suppressWarnings(fetch(dbSendQuery(con, app_client_sql), n=-1))

clients_sql <- paste("SELECT egn, first_name, middle_name, last_name 
FROM citycash_db.clients",sep="")
clients <- suppressWarnings(fetch(dbSendQuery(con, clients_sql), n=-1))
Encoding(clients$first_name) <- "UTF-8"
Encoding(clients$middle_name) <- "UTF-8"
Encoding(clients$last_name) <- "UTF-8"

# Make dataframe
final <- merge(app[,c("id","credit_number")],
               app_client[,c("application_id","egn")],
               by.x = "id", by.y = "application_id", all.x = TRUE)
final <- merge(final,
               clients,
               by.x = "egn", by.y = "egn", all.x = TRUE)
final$three_names <- paste(final$first_name,final$middle_name,
                           final$last_name,sep=" ")

# Output
write.xlsx(final,
           "C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\all_names.xlsx")

