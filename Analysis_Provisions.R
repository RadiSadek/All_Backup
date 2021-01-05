

library(openxlsx)

#Set working directory
setwd("C:\\Projects\\Provisions_Modeled\\data\\")

now <- read.xlsx("provisions_by_months\\Provisions_2019-06-01.xlsx")
before <- read.xlsx("provisions_by_months\\Provisions_2019-05-01.xlsx")
before2 <- read.xlsx("provisions_by_months\\Provisions_2019-04-01.xlsx")



# Computer provisions now
before_EL <- before[,c("credit_number","EL")]
names(before_EL) <- c("credit_number","EL_before")
now <- merge(now, before_EL, by.x = "credit_number", by.y = "credit_number", all.x = TRUE)
now$EL_before <- ifelse(is.na(now$EL_before), 0, now$EL_before)
now$provisions <- now$EL - now$EL_before
now$default30 <- ifelse(now$dpd>=30, 1, 0)
now$default90 <- ifelse(now$dpd>=90, 1, 0)
now$default180 <- ifelse(now$dpd>=180, 1, 0)

now$product_name <- ifelse(substring(now$product,1,6)=="City 2", "City 2-week",
                    ifelse(substring(now$product,1,6)=="City M", "City Month",
                    ifelse(substring(now$product,1,6)=="City W", "City Week",
                    ifelse(substring(now$product,1,6)=="Credir", "Credirect Потребителски",
                    ifelse(substring(now$product,1,6)=="CrediR", "Credirect 1430",
                    ifelse(substring(now$product,1,6)=="Пенсио", "Пенсионер",
                    ifelse(substring(now$product,1,6)=="VIP Се", "VIP", "Other")))))))

provisions <- as.data.frame(aggregate(now$provisions, by=list(now$product_name), FUN=sum))
names(provisions) <- c("product","provision")
provisions <- merge(provisions, as.data.frame(table(now$product_name)), by.x = "product", by.y = "Var1", all.x = TRUE)
provisions$ratio <- provisions$provision/provisions$Freq

provisions <- merge(provisions, as.data.frame(aggregate(now$default30, by=list(now$product_name), FUN=sum)), 
                    by.x = "product", by.y = "Group.1", all.x = TRUE)
names(provisions)[ncol(provisions)] <- "count_default30"
provisions$default30_pct <- provisions$count_default30/provisions$Freq

provisions <- merge(provisions, as.data.frame(aggregate(now$default90, by=list(now$product_name), FUN=sum)), 
                    by.x = "product", by.y = "Group.1", all.x = TRUE)
names(provisions)[ncol(provisions)] <- "count_default90"
provisions$default90_pct <- provisions$count_default90/provisions$Freq

provisions <- merge(provisions, as.data.frame(aggregate(now$default180, by=list(now$product_name), FUN=sum)), 
                    by.x = "product", by.y = "Group.1", all.x = TRUE)
names(provisions)[ncol(provisions)] <- "count_default180"
provisions$default180_pct <- provisions$count_default180/provisions$Freq

provisions <- provisions[,c(1,2,3,4,6,8,10)]
View(provisions)





# Computer provisions a month ago
before_EL2 <- before2[,c("credit_number","EL")]
names(before_EL2) <- c("credit_number","EL_before")
before <- merge(before, before_EL2, by.x = "credit_number", by.y = "credit_number", all.x = TRUE)
before$EL_before <- ifelse(is.na(before$EL_before), 0, before$EL_before)
before$provisions <- before$EL - before$EL_before
before$default30 <- ifelse(before$dpd>=30, 1, 0)
before$default90 <- ifelse(before$dpd>=90, 1, 0)
before$default180 <- ifelse(before$dpd>=180, 1, 0)

before$product_name <- ifelse(substring(before$product,1,6)=="City 2", "City 2-week",
                    ifelse(substring(before$product,1,6)=="City M", "City Month",
                    ifelse(substring(before$product,1,6)=="City W", "City Week",
                    ifelse(substring(before$product,1,6)=="Credir", "Credirect Потребителски",
                    ifelse(substring(before$product,1,6)=="CrediR", "Credirect 1430",
                    ifelse(substring(before$product,1,6)=="Пенсио", "Пенсионер",
                    ifelse(substring(before$product,1,6)=="VIP Се", "VIP", "Other")))))))

provisions2 <- as.data.frame(aggregate(before$provisions, by=list(before$product_name), FUN=sum))
names(provisions2) <- c("product","provision")
provisions2 <- merge(provisions2, as.data.frame(table(before$product_name)), by.x = "product", by.y = "Var1", all.x = TRUE)
provisions2$ratio <- provisions2$provision/provisions2$Freq

provisions2 <- merge(provisions2, as.data.frame(aggregate(before$default30, by=list(before$product_name), FUN=sum)), 
                    by.x = "product", by.y = "Group.1", all.x = TRUE)
names(provisions2)[ncol(provisions2)] <- "count_default30"
provisions2$default30_pct <- provisions2$count_default30/provisions2$Freq

provisions2 <- merge(provisions2, as.data.frame(aggregate(before$default90, by=list(before$product_name), FUN=sum)), 
                     by.x = "product", by.y = "Group.1", all.x = TRUE)
names(provisions2)[ncol(provisions2)] <- "count_default90"
provisions2$default90_pct <- provisions2$count_default90/provisions2$Freq

provisions2 <- merge(provisions2, as.data.frame(aggregate(before$default180, by=list(before$product_name), FUN=sum)), 
                     by.x = "product", by.y = "Group.1", all.x = TRUE)
names(provisions2)[ncol(provisions2)] <- "count_default180"
provisions2$default180_pct <- provisions2$count_default180/provisions2$Freq


provisions2 <- provisions2[,c(1,2,3,4,6,8,10)]
View(provisions2)









########## ROl data ###########
# Main dataframe
now_old <- read.xlsx("back_up_accounting\\Accounting_INPUT_apr.xlsx")
before_old <- read.xlsx("back_up_accounting\\Accounting_INPUT_mar.xlsx")
before_old_raw <- before_old

# Filter non used columns
now_old <- now_old [ , -which(names(now_old) %in% c("Офис.при.отпускане","Счетоводно.отписан"))]
before_old <- before_old[ , -which(names(before_old) %in% c("Офис.при.отпускане","Счетоводно.отписан"))]

# Calculate provisions
now_old$група <- ifelse(now_old[,31]>180,4,
                 ifelse(now_old[,31]>90,3,
                 ifelse(now_old[,31]>30,2,1)))
now_old$Обезценка <- ifelse(now_old[,36]==1,now_old[,30]*0,
                     ifelse(now_old[,36]==2,now_old[,30]*0.1,
                     ifelse(now_old[,36]==3,now_old[,30]*0.5,now_old[,30]*1)))
now_old$Обезценка <- ifelse(is.na(now_old[,35]), now_old$Обезценка,
                     ifelse(now_old[,35]>now_old[,30],0,
                     ifelse(now_old[,36]==1,(now_old[,30]-now_old[,35])*0,
                     ifelse(now_old[,36]==2,(now_old[,30]-now_old[,35])*0.1,
                      ifelse(now_old[,36]==3,(now_old[,30]-now_old[,35])*0.5,(now_old[,30]-now_old[,35])*1)))))
now_old$Обезценка <- ifelse(is.na(now_old$Обезценка), 0, now_old$Обезценка)



before_old$група <- ifelse(before_old[,31]>180,4,
                    ifelse(before_old[,31]>90,3,
                    ifelse(before_old[,31]>30,2,1)))
before_old$Обезценка <- ifelse(before_old[,36]==1,before_old[,30]*0,
                        ifelse(before_old[,36]==2,before_old[,30]*0.1,
                        ifelse(before_old[,36]==3,before_old[,30]*0.5,before_old[,30]*1)))
before_old$Обезценка <- ifelse(is.na(before_old[,35]), before_old$Обезценка,
                        ifelse(before_old[,35]>before_old[,30],0,
                        ifelse(before_old[,36]==1,(before_old[,30]-before_old[,35])*0,
                        ifelse(before_old[,36]==2,(before_old[,30]-before_old[,35])*0.1,
                        ifelse(before_old[,36]==3,(before_old[,30]-before_old[,35])*0.5,(before_old[,30]-before_old[,35])*1)))))
before_old$Обезценка <- ifelse(is.na(before_old$Обезценка), 0, before_old$Обезценка)


before_old <- before_old[,c("Номер.на.кредит","Обезценка")]
now_old <- merge(now_old, before_old, by.x = "Номер.на.кредит", by.y = "Номер.на.кредит", 
                 all.x = TRUE)
now_old$Обезценка.y <- ifelse(is.na(now_old$Обезценка.y), 0, now_old$Обезценка.y)
now_old$provisions <- now_old$Обезценка.x - now_old$Обезценка.y


now_old$product_name <- ifelse(substring(now_old$Вид.заем,1,6)=="City 2", "City 2-week",
                       ifelse(substring(now_old$Вид.заем,1,6)=="City M", "City Month",
                       ifelse(substring(now_old$Вид.заем,1,6)=="City W", "City Week",
                       ifelse(substring(now_old$Вид.заем,1,6)=="Credir", "Credirect Потребителски",
                       ifelse(substring(now_old$Вид.заем,1,6)=="CrediR", "Credirect 1430",
                       ifelse(substring(now_old$Вид.заем,1,6)=="Пенсио", "Пенсионер",
                       ifelse(substring(now_old$Вид.заем,1,6)=="VIP Се", "VIP", "Other")))))))
aggregate(now_old$provisions, by=list(now_old$product_name), FUN=sum)



sum(now_old$provisions)
now_old <- now_old[,c(1,2,3,30,31,37,38,39,40,36)]
dpd_old <- before_old_raw[,c(1,32)]
now_old <- merge(now_old, dpd_old, by.x = "Номер.на.кредит", by.y = "Номер.на.кредит", 
                  all.x = TRUE)
now_old$dpd_change <- now_old[,5]-now_old[,11]
now_old$dpd_change <- ifelse(is.na(now_old$dpd_change), -999, now_old$dpd_change)
now_old$dpd_change_bin <- ifelse(now_old$dpd_change>=30, "increase_30_more",
                          ifelse(now_old$dpd_change>=0, "increase_0_29",
                          ifelse(now_old$dpd_change==-999, "new","decrease")))


cred_user <- subset(now_old, now_old$product_name=="Credirect Потребителски")
cityweek <- subset(now_old, now_old$product_name=="City Week")


aggregate(cred_user$provisions, by=list(cred_user$dpd_change_bin), FUN=sum)
aggregate(cityweek$provisions, by=list(cityweek$dpd_change_bin), FUN=sum)



setwd("C:\\Projects\\Provisions_Modeled\\data\\")
write.xlsx(cred_user,"cred_user.xlsx")
write.xlsx(cityweek,"cityweek.xlsx")


cred_user$provisions_binned <- ifelse(cred_user$provisions>1000,">+1000",
                               ifelse(cred_user$provisions>500,"500_1000",
                               ifelse(cred_user$provisions>0,"0_500",
                               ifelse(cred_user$provisions>-500,"0_-500","<=-500"))))

cityweek$provisions_binned <- ifelse(cityweek$provisions>1000,">+1000",
                               ifelse(cityweek$provisions>500,"500_1000",
                               ifelse(cityweek$provisions>0,"0_500",
                               ifelse(cityweek$provisions>-500,"0_-500","<=-500"))))





