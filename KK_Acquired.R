
############ Automatic settings ##########

# Define input files
kk <- "users_offices_input.xlsx"
staj <- "Стажантска програма_октомври.xlsx"
staj_old <- "стажанти_old.xlsx"

# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)
library(Hmisc)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Load data
con <- dbConnect(MySQL(), user=db_user, password=db_password, dbname=db_name, 
  host=db_host, port = df_port)

# Read data
setwd("C:\\Regular\\R\\KK\\")
df <- read.xlsx(kk,1)
stajant <- read.xlsx(staj,1)
stajant_old <- read.xlsx(staj_old,1)
stajant <- stajant[,c(5,7,10)]
names(stajant) <- c("egn","activated_staj","deactivated_staj")
stajant_old <- stajant_old[!(stajant_old$egn %in% stajant$egn),]
stajant <- rbind(stajant,stajant_old)

# Choose and rename fields
df <- df[,c(1,6,7,8,9)]
names(df) <- c("egn","office","zone","activated","deactivated")

# Make correct fields
df$activated <- as.Date(paste(substring(df$activated,7,10),
                               substring(df$activated,4,5),
                               substring(df$activated,1,2),sep="-"))
df$deactivated <- as.Date(ifelse(is.na(df$deactivated),NA,
                          paste(substring(df$deactivated,7,10),
                               substring(df$deactivated,4,5),
                               substring(df$deactivated,1,2),sep="-")))

# Clean data
df <- subset(df,df$activated>="2013-01-01")
df <- df[!(df$egn %in% names(table(df$egn)[table(df$egn)>10])),]

# Join stajant program
stajant$activated_staj <- as.Date(stajant$activated_staj, 
                                   origin = "1899-12-30")
stajant$deactivated_staj <- as.Date(stajant$deactivated_staj, 
                                  origin = "1899-12-30")
df <- merge(df,stajant, by.x = "egn", by.y = "egn", all.x = TRUE)
df <- subset(df,!(is.na(df$activated_staj)))

# Make stats
nrow(df[is.na(df$activated_staj),])/nrow(df)

# Get all available months from stajant and make them into columns
names <- t(names(table(substring(df$deactivated_staj,1,7))))
df_dates <- data.frame(matrix(ncol=ncol(names), nrow=nrow(df)))
colnames(df_dates) <- names
df <- cbind(df,df_dates)

# Compute if active per 
df$id <-1:nrow(df)
for(i in (8:(ncol(df)-1))){
  df[,i] <- ifelse(names(df)[i]>=substring(df$deactivated_staj[df$id],1,7) &
                   names(df)[i]>=substring(df$activated[df$id],1,7) &
                   is.na(df$deactivated[df$id]), 1,
            ifelse(names(df)[i]>=substring(df$deactivated_staj[df$id],1,7) & 
                   names(df)[i]>=substring(df$activated[df$id],1,7) & 
                   names(df)[i]<=substring(df$deactivated[df$id],1,7),
                          1,0))
  
}

# Aggregate per egn
res <- as.data.frame(aggregate(df[,8],by=list(df$egn),FUN=sum))
names(res) <- c("egn",names(df)[8])
for (i in (9:(ncol(df)-1))){
  res2 <- as.data.frame(aggregate(df[,i],by=list(df$egn),FUN=sum))
  names(res2) <- c("egn",names(df)[i])
  res <- merge(res,res2,by.x = "egn",by.y = "egn",all.x = TRUE)
}
res[,2:ncol(res)] <- sapply(res[,2:ncol(res)],function(x) ifelse(x>=1,1,0))
final_res <- cbind(res[,1],res[,2:ncol(res)])
names(final_res)[1] <- "egn"

# Create final dataframe
final <- merge(final_res,df[,c(1:3,7)], by.x = "egn", by.y = "egn",
               all.x = TRUE)
final <- final[order(final$deactivated_staj),]
final$deactivated_staj <- substring(final$deactivated_staj,1,7)
final <- final[order(final$egn),]
final <- final[!duplicated(final$egn),]

# Output
write.xlsx(final,"output.xlsx")
write.xlsx(stajant,"стажанти_old.xlsx")


