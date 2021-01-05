

##########################################
# Generate Table of Analysis             #
##########################################


# Load libraries #
library(caTools)
library(dummies)
library(neuralnet)
library(smbinning)
library(dummies)
library(plyr)
library(binr)


# USER CHOOSES INPUT 
# Read data
setwd("C:\\Projects\\Behavioral_Scoring\\CityCash_v2\\data\\")
df_final <- read.csv("Results_Logit_for_Analysis.csv", sep=",")


# Arrange some fields 
#names(df_final)[names(df_final) == 'amount_paid'] <- 'Total_Paid'
df_final$Total_Paid <- ifelse(is.na(df_final$amount_paid),0,
                              df_final$amount_paid)
df_final$Principal <- df_final$amount
df_final$Principal <- as.numeric(df_final$Principal)
df_final$amount <- as.numeric(df_final$amount)


# Define cut-off
df_final$default_flag_Predict <- ifelse(df_final$Score>0.1,1,0)


# USER CHOOSES CUTS 
# Cut into optimum groups
bins(df_final$Score, target.bins = 10, minpts = 10)
df_final <- subset(df_final,!(is.na(df_final$Score)))
bins <- c(0,0.025,0.0375,0.05,0.0625,0.075,0.0875,
          0.1,0.125,0.15,0.175,0.2,0.225,0.25,0.275,0.3,
          0.325,0.35,0.375,0.4,0.5,0.6,1)
df_final$Score_Group <- cut(df_final$Score,bins)


# AUTOMATIC PART GENERATES ANALYSIS TABLE
# Subset into good and bads
df_bads <- subset(df_final, default_flag == 1)
df_goods <- subset(df_final, default_flag == 0)


# Calculate total principal and total paid per score group
sum_principal_bads <- aggregate(df_bads$amount, 
                                by=list(Category=df_bads$Score_Group), FUN=sum)
sum_principal_goods <- aggregate(df_goods$amount, 
                                 by=list(Category=df_goods$Score_Group), FUN=sum)
paid_bads <- aggregate(df_bads$Total_Paid, 
                       by=list(Category=df_bads$Score_Group), FUN=sum)
paid_goods <- aggregate(df_goods$Total_Paid, 
                        by=list(Category=df_goods$Score_Group), FUN=sum)


# Merge into one big dataframe
df_analysis <- merge(sum_principal_bads ,
                     sum_principal_goods, by="Category", all=TRUE)
df_analysis <- merge(df_analysis,paid_bads, 
                     by="Category", all=TRUE)
df_analysis <- merge(df_analysis,paid_goods, 
                     by="Category", all=TRUE)
names(df_analysis) <- c("Category","Principal_Bads","Principal_Goods",
                        "Paid_Bads","Paid_Goods")


# Set null values to zeros
df_analysis[is.na(df_analysis)] <- 0


# Calculate net profit, profit per population in score group
df_analysis$Profit <- (df_analysis$Paid_Bads+df_analysis$Paid_Goods)-
  (df_analysis$Principal_Goods+df_analysis$Principal_Bads)
df_analysis$Population <- table(df_final$Score_Group)[apply(table(
  df_final$Score_Group), 1, function(row) all(row !=0 ))]
df_analysis$Profit_per_Pop <- df_analysis$Profit/df_analysis$Population


# Calculate cumulative population and ratio of cum. population
df_analysis$Cumulative_Pop[1]=df_analysis$Population[1] 
for (i in (2:nrow(df_analysis))) {
  df_analysis$Cumulative_Pop[i]=df_analysis$Cumulative_Pop[i-1]+
    df_analysis$Population[i]
}
for (i in (1:nrow(df_analysis))) {
  df_analysis$Cumulative_Pop[i]=df_analysis$Cumulative_Pop[i]/
    df_analysis$Cumulative_Pop[nrow(df_analysis)]
}


# Calculate number of goods and bads per score group and % for the real data 
df_analysis <- cbind(df_analysis,t(table(
  df_final$default_flag,df_final$Score_Group))[,1])
df_analysis <- cbind(df_analysis,t(table(
  df_final$default_flag,df_final$Score_Group))[,2])
names(df_analysis)[10] <- "True_Goods"
names(df_analysis)[11] <- "True_Bads"


# Calculate cumulative bads
df_analysis$Cumulative_Goods[1]=df_analysis$True_Goods[1] 
for (i in (2:nrow(df_analysis))) {
  df_analysis$Cumulative_Goods[i]=df_analysis$Cumulative_Goods[i-1]+
    df_analysis$True_Goods[i]
}
df_analysis$Cumulative_Bads[1]=df_analysis$True_Bads[1] 
for (i in (2:nrow(df_analysis))) {
  df_analysis$Cumulative_Bads[i]=df_analysis$Cumulative_Bads[i-1]+
    df_analysis$True_Bads[i]
}
for (i in (1:nrow(df_analysis))) {
  df_analysis$Bads_Rate[i]=df_analysis$Cumulative_Bads[i]/
    (df_analysis$Cumulative_Bads[i]+df_analysis$Cumulative_Goods[i])
}


# Calculate bad rates
real_bad_rate <- sum(df_analysis$True_Bads)/
  (sum(df_analysis$True_Bads)+sum(df_analysis$True_Goods))
df_final$default_flag_Predict <- ifelse(df_final$Score>0.45,1,0)
predict_bad_rate <- sum(df_final$default_flag_Predict)/nrow(df_final)


# DISPLAY RESULT 
df_analysis

