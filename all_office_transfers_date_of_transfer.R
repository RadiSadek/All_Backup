

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
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")


data_sql <- suppressWarnings(dbSendQuery(con, 
"SELECT 
citycash_db.credits_applications.credit_number,
citycash_db.credits_applications.date,
citycash_db.credits_applications_transfers.old_office_id,
citycash_db.credits_applications_transfers.created_at,
citycash_db.structure_offices.name
FROM citycash_db.credits_applications
INNER JOIN citycash_db.credits_applications_transfers ON 
citycash_db.credits_applications.id = 
citycash_db.credits_applications_transfers.application_id
INNER JOIN citycash_db.structure_offices ON 
citycash_db.credits_applications_transfers.old_office_id = 
citycash_db.structure_offices.id"))
office_transfer <- fetch(data_sql, n=-1)
Encoding(office_transfer$name) <- "UTF-8"
office_transfer$type <- "old"
office_transfer <- office_transfer[,c("credit_number","date","name","type",
                                      "created_at")]


data_sql <- suppressWarnings(dbSendQuery(con, 
"SELECT 
citycash_db.credits_applications.credit_number,
citycash_db.credits_applications.date,
citycash_db.credits_applications_transfers.new_office_id,
citycash_db.credits_applications_transfers.created_at,
citycash_db.structure_offices.name
FROM citycash_db.credits_applications
INNER JOIN citycash_db.credits_applications_transfers ON 
citycash_db.credits_applications.id = 
citycash_db.credits_applications_transfers.application_id
INNER JOIN citycash_db.structure_offices ON 
citycash_db.credits_applications_transfers.new_office_id = 
citycash_db.structure_offices.id"))
office_transfer2 <- fetch(data_sql, n=-1)
Encoding(office_transfer2$name) <- "UTF-8"
office_transfer2$type <- "new"
office_transfer2 <- office_transfer2[,c("credit_number","date","name","type",
                                        "created_at")]

# Append results
final <- rbind(office_transfer,office_transfer2)

# Select interesting
final <- subset(final,final$name=="Просрочени ЦО" & final$type=="new")
final <- final[rev(order(final$created_at)),]
final <- final[order(final$credit_number),]
final <- final[!duplicated(final$credit_number),]

# Outputs
write.xlsx(final,"C:\\Users\\rsadek.CC\\Desktop\\out3.xlsx")


