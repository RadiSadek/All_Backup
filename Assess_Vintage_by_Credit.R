
########## GENERATE VINTAGE REPORT ############


# Function to calculate GB index
gb_index_fct <- function(data,var,gb_flag){
  gb_ratio_all <- count(data, gb_flag)[1,2]/count(data, gb_flag)[2,2]
  index <- vector(length = length(unique(var)))
  for (i in 1:length(unique(var))) {
    local <- count(subset(data,var==unique(var)[i]), gb_flag)[1,2]/
      count(subset(data,var==unique(var)[i]), gb_flag)[2,2]
    all <- gb_ratio_all
    index[i] <- ifelse(local>all,paste(round(local/all*100,1),"G",sep=""),
                       paste(round(all/local*100,1),"B",sep=""))
  }
  index <- as.data.frame(t(index))
  for (i in 1:length(unique(var))) {
    names(index)[i] <- paste(unique(var)[i])
  }
  return (index[ , order(names(index))])
}



# Choose end date of study (not included) for profit analysis
end_date <- "2020-09-01"


# Get libraries
library(openxlsx)
library(RMySQL)
library(lubridate)
library(zoo)
library(matrixStats)
library(mltools)
library(Information)


# Database specifications
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306


# Read and make connection
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, 
                 host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
data <- df

# Read data
setwd("C:\\Regular\\R\\Vintage\\")
df <- read.xlsx("vintage_template.xlsx",3)

# Remove 2020
df <- subset(df,df$`year(`=="2019")

# Arrange fields
df[,26] <- ifelse(is.na(df[,26]),0,df[,26])
df[,27] <- ifelse(is.na(df[,27]),0,df[,27])
df[,28] <- ifelse(is.na(df[,28]),0,df[,28])
df[,29] <- ifelse(is.na(df[,29]),0,df[,29])
df[,30] <- ifelse(is.na(df[,30]),0,df[,30])
df[,31] <- ifelse(is.na(df[,31]),0,df[,31])
df[,32] <- ifelse(is.na(df[,32]),0,df[,32])
df[,33] <- ifelse(is.na(df[,33]),0,df[,33])
df[,34] <- ifelse(is.na(df[,34]),0,df[,34])
df[,35] <- ifelse(is.na(df[,35]),0,df[,35])
df[,36] <- ifelse(is.na(df[,36]),0,df[,36])
df[,37] <- ifelse(is.na(df[,37]),0,df[,37])
df[,38] <- ifelse(is.na(df[,38]),0,df[,38])
df[,39] <- ifelse(is.na(df[,39]),0,df[,39])
df[,40] <- ifelse(is.na(df[,40]),0,df[,40])
df[,41] <- ifelse(is.na(df[,41]),0,df[,41])

# Make month field
df$month <- ifelse(nchar(df$month)==1,paste("0",df$month,sep=""),df$month)
df$month_cor <- paste(df$`year(`,df$month,sep="-")

# Define profit
df$r360 <- ifelse(is.na(df$r360),0,df$r360)
df$profit <- df$r360/df$Сума

# Subset only those with positive za sabirane
df <- subset(df,df$m360>0)

# Define vintage
df$v30 <- 1 - df$r30/df$m30
df$v60 <- 1 - df$r60/df$m60
df$v90 <- 1 - df$r90/df$m90
df$v180 <- 1 - df$r180/df$m180
df$v210 <- 1 - df$r210/df$m210
df$v360 <- 1 - df$r360/df$m360

# Bin variables
df$profit_bin <- ifelse(df$profit>=1,0,1)
df$v60_bin <- bin_data(round(df$v60,2), bins=10, binType = "quantile")

# Create list for different products
df1 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
            df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City We",]
df2 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
            df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City Mo",]
df3 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
            df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="Пенсион",]
df4 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
            df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City 2-",]
df5 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
            df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City We",]
df6 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
            df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City Mo",]
df7 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
            df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="Пенсион",]
df8 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
            df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City 2-",]
df9 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
            df$Зона=="CrediRect" & 
            substring(df$Тип.продукт,1,15)=="CreDirect Потре",]
df10 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
             df$Зона=="CrediRect" & 
             substring(df$Тип.продукт,1,15)!="CreDirect Потре",]
df13 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
             df$Зона=="CrediRect" & 
             substring(df$Тип.продукт,1,15)=="CreDirect Потре",]
df14 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
             df$Зона=="CrediRect" & 
             substring(df$Тип.продукт,1,15)!="CreDirect Потре",]


#df <- df[,c(52:58)]
IV <- create_infotables(data=df, y="profit_bin", bins=10, parallel=FALSE) 
plot_infotables(IV, "v60")

IV9 <- create_infotables(data=df9, y="profit_bin", bins=10, parallel=FALSE) 
plot_infotables(IV9, "v60")

df9$v60_bin <- ifelse(df9$v60<0.2,"0_0.2",
               ifelse(df9$v60<1,"0.2_0.99","1"))
gb_ratio_all <- count(data, gb_flag)[1,2]/count(data, gb_flag)[2,2]

