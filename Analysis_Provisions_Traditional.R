

############## READ DATA ####################

# Read libraries
suppressWarnings(suppressMessages(library(openxlsx)))
suppressWarnings(suppressMessages(library(vars)))
suppressWarnings(suppressMessages(library(RMySQL)))

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)

# Read past accounting files
load(paste("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\",
           "accounting.rdata",sep=""))
df <- final
df <- subset(df,df$credit_number!="Номер на кредит")
rm(final)

# Read income expenses files
load(paste("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\",
           "income_expenses.rdata",sep=""))
repay <- final
repay$repay_amount <- as.numeric(repay$repay_amount)
rm(final)

# Join cession amount
agg_repay <- aggregate(repay$repay_amount,
      by=list(repay$credit_number,repay$month),FUN=sum)
names(agg_repay) <- c("credit_number","month","repay_amount")
df <- merge(df,agg_repay,by.x = c("credit_number","month"),
            by.y = c("credit_number","month"),all.x = TRUE)
df$repay_amount <- ifelse(is.na(df$repay_amount),0,df$repay_amount)

# Choose and set columns
df <- df[,c(1:7,10,32,33,37,39,13:15)]
df[is.na(df)] <- 0
df$total_receivables_end <- as.numeric(df$total_receivables_end)
df$days_delay <- as.numeric(df$days_delay)
df$collateral_value <- as.numeric(df$collateral_value)
df$profit <- as.numeric(df$lihva) + as.numeric(df$neustoika) + 
  as.numeric(df$taksa)
df$profit <- ifelse(is.na(df$profit),0,df$profit)

# Compute provisions using old method
df$collateral_value_cor <- 
  ifelse((df$total_receivables_end-df$collateral_value)<=0,0,
         (df$total_receivables_end-df$collateral_value))
df$provisions <- ifelse(df$collateral_value_cor<=0,0,
  ifelse(df$days_delay<=30,0,
  ifelse(df$days_delay<=90,0.1*df$collateral_value_cor,
  ifelse(df$days_delay<=180,0.5*df$collateral_value_cor,
  df$collateral_value_cor))))
df <- df[,-which(names(df) %in% c("collateral_value_cor","collateral_value",
  "days_delay"))]

# Make product category field
df$product_cat <- 
  ifelse(substring(df$product,1,11)=="BigFin","BigFin",
  ifelse(substring(df$product,1,11)=="City 2-week","City_2_week",
  ifelse(substring(df$product,1,9)=="City Mont","City_Month",
  ifelse(substring(df$product,1,9)=="City Week","City_Week",
  ifelse(substring(df$product,1,11)=="CreDirect F","CreDirect_Flex",
  ifelse(substring(df$product,1,11)=="CreDirect П","CreDirect_Потребитески",
  ifelse(substring(df$product,1,11) %in% c("CreDirect14","CreDirect30"),
         "CreDirect_Flex",
  ifelse(substring(df$product,1,9) %in% c("Пенсионер"),"Пенсионер",
         "Other"))))))))
  
# Aggregate and make correct table for provisions
agg <- aggregate(df$provisions,by = list(df$product_cat,df$month), FUN=sum)
names(agg) <- c("product","month","provision")
df_tab <- as.data.frame(unique(agg$month))
names(df_tab) <- c("month")
for(i in 1:length(unique(agg$product))){
  df_tab <- suppressWarnings(merge(df_tab,
    agg[agg$product==unique(agg$product)[i],
    c("month","provision")],by.x = "month",by.y = "month",all.x = TRUE))
}
names(df_tab)[2:ncol(df_tab)] <- unique(agg$product)
names(df_tab)[2:ncol(df_tab)] <- paste(names(df_tab[2:ncol(df_tab)]),
                                       "_EL",sep="")
df_tab$City_2_week_provision <- NA
df_tab$City_Month_provision <- NA
df_tab$City_Week_provision <- NA
df_tab$Other_provision <- NA
df_tab$Пенсионер_provision <- NA
df_tab$BigFin_provision <- NA
df_tab$CreDirect_Flex_provision <- NA
df_tab$CreDirect_Потребитески_provision <- NA

for(i in 2:nrow(df_tab)){
  df_tab$City_2_week_provision[i] <- df_tab$City_2_week_EL[i] - 
    df_tab$City_2_week_EL[i-1]
  df_tab$City_Month_provision[i] <- df_tab$City_Month_EL[i] - 
    df_tab$City_Month_EL[i-1]
  df_tab$City_Week_provision[i] <- df_tab$City_Week_EL[i] - 
    df_tab$City_Week_EL[i-1]
  df_tab$Other_provision[i] <- df_tab$Other_EL[i] - 
    df_tab$Other_EL[i-1]
  df_tab$Пенсионер_provision[i] <- df_tab$Пенсионер_EL[i] - 
    df_tab$Пенсионер_EL[i-1]
  df_tab$BigFin_provision[i] <- df_tab$BigFin_EL[i] - 
    df_tab$BigFin_EL[i-1]
  df_tab$CreDirect_Flex_provision[i] <- df_tab$CreDirect_Flex_EL[i] - 
    df_tab$CreDirect_Flex_EL[i-1]
  df_tab$CreDirect_Потребитески_provision[i] <- df_tab$CreDirect_Потребитески_EL[i] - 
    df_tab$CreDirect_Потребитески_EL[i-1]
}
df_tab <- df_tab[,c(1,10:17)]

# Aggregate and make correct table for receivables
agg <- aggregate(df$profit,by = list(df$product_cat,
                                         df$month), FUN=sum)
names(agg) <- c("product","month","profit")
df_tab2 <- as.data.frame(unique(agg$month))
names(df_tab2) <- c("month")
for(i in 1:length(unique(agg$product))){
  df_tab2 <- suppressWarnings(merge(df_tab2,
   agg[agg$product==unique(agg$product)[i],
  c("month","profit")],by.x = "month",by.y = "month",all.x = TRUE))
}
names(df_tab2)[2:ncol(df_tab2)] <- unique(agg$product)
names(df_tab2)[2:ncol(df_tab2)] <- paste(names(df_tab2[2:ncol(df_tab2)]),
                                       "_profit",sep="")

# Join repayments
agg_repay_per_product <- aggregate(df$repay_amount,
    by=list(df$product_cat,df$month),FUN=sum)
names(agg_repay_per_product) <- c("product","month","repayments")
df_tab3 <- as.data.frame(unique(agg_repay_per_product$month))
names(df_tab3) <- c("month")
for(i in 1:length(unique(agg_repay_per_product$product))){
  df_tab3 <- suppressWarnings(merge(df_tab3,
     agg_repay_per_product[agg_repay_per_product$product==unique
                           (agg_repay_per_product$product)[i],
     c("month","repayments")],by.x = "month",by.y = "month",all.x = TRUE))
}
names(df_tab3)[2:ncol(df_tab3)] <- unique(agg_repay_per_product$product)
names(df_tab3)[2:ncol(df_tab3)] <- paste(names(df_tab3[2:ncol(df_tab3)]),
                                         "_repayments",sep="")

# Make final dataset
final <- cbind(df_tab,df_tab2[,c(2:ncol(df_tab2))],
               df_tab3[,c(2:ncol(df_tab3))])
final <- final[,-which(names(final) %in% 
     c("Other_provision","Other_profit","Other_repayments"))]

# Replace NA by zeros
final[is.na(final)] <- 0

# Compute final provision field
final$City_2_week_provision_cor <- final[,2] + final[,16]
final$City_Month_provision_cor <- final[,3] + final[,17]
final$City_Week_provision_cor <- final[,4] + final[,18]
final$Pensioner_provision_cor <- final[,5] + final[,19]
final$BigFin_provision_cor <- final[,6] + final[,20]
final$Credirect_Flex_provision_cor <- final[,7] + final[,21]
final$Credirect_Potrebitelski_cor <- final[,8] + final[,22]

# Make new fields
final$City_2week_ratio <- round(final[,23]/final[,9],3)
final$City_Month_ratio <- round(final[,24]/final[,10],3)
final$City_Week_ratio <- round(final[,25]/final[,11],3)
final$City_Pensioner_ratio <- round(final[,26]/final[,12],3)
final$BigFin_ratio <- round(final[,27]/final[,13],3)
final$Credirect_Flex_ratio <- round(final[,28]/final[,14],3)
final$Credirect_Potrebitelski_ratio <- round(final[,29]/final[,15],3)

# Choose only relevant columns
final <- final[,c(1,30:36)]

# Choose only after 2018
final <- final[c(25:nrow(final)),]

# Display result
View(final)


