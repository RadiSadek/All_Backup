
library(openxlsx)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
df <- subset(df,df$deactivated_at>="2019-01-01" & 
               df$deactivated_at<="2019-12-31")
df <- subset(df,df$sub_status==133)
df <- df[order(df$id),]
df$month_deactivated_at <- substring(df$deactivated_at,1,7)
df <- df[,c("credit_number","id","date","status","deactivated_at",
            "month_deactivated_at")]
write.xlsx(df,"C:\\Users\\rsadek.CC\\Desktop\\out.xlsx")
