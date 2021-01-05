

##########################################################
######## Compute difference between two datasets #########
##########################################################

# Call libraries 
library(dplyr)
library(RMySQL)
library(dplyr)
library(openxlsx)

# Main dataframe
setwd("C:\\Projects\\Provisions_Modeled\\data\\")

df <- read.csv("plan1.csv")
df_old <- read.csv("plan2.csv")


df_old <- df_old[ , order(colnames(df_old))]
df <- df[ , order(colnames(df))]


# Function for difference between two datasets
return_diff <- function(var){
  df_old <- df_old[,c("credit_number", var)]
  df <- df[,c("credit_number",var)]
  mer <- merge(df, df_old, by.x = "credit_number", by.y = "credit_number", all.x=TRUE )
  mer$diff <- ifelse(mer[paste(var,".x",sep="")]==mer[paste(var,".y",sep="")],1,0)
  return(subset(mer, mer$diff==0))
}

# Get indices of column of difference
num_row <- matrix(, nrow = ncol(df), ncol = 2)
for (i in c(1,3:ncol(df))){
a <- return_diff(names(df)[i])
num_row[i,1] <- i
num_row[i,2] <- nrow(a)
}
num_row <- as.data.frame(num_row)

View(num_row)

num_row_diff <- subset(num_row, num_row$V2>0)

for (i in 1:nrow(num_row_diff)){
  num_row_diff$name[i] <- names(df)[num_row_diff$V1[i]]
}

View(num_row_diff)

