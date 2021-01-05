
# Load libraries and data
library(openxlsx)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")


# Choose criteria of selection
df <- subset(df,df$date_entry>="2020-11-20" & df$date_entry<="2020-11-30")
df <- subset(df,df$product_id==9)
df <- subset(df,df$has_prev_credits==0)
df <- subset(df,df$online_offline=="online")
df <- subset(df,df$deactivated_at>="2020-06-01")
df <- subset(df,df$status %in% c(4,5))
df <- subset(df,is.na(df$sub_status) | df$sub_status!=136)
df <- df[,c("id","credit_number","egn")]
df$days_delay_bin <- cut(df$days_delay,c(-Inf,30,60,90,Inf))


# Output results
df <- df[order(df$id),]
df$cession <- ifelse(df$sub_status==124,"Yes","No")
write.xlsx(df,"C:\\Users\\rsadek.CC\\Desktop\\out.xlsx")
