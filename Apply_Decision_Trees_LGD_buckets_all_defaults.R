

##########################################
# APPLY LOSS GIVEN DEFAULT TO NEW DATA   #
##########################################

# Define constants
days_def <- 90

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# set the seed
set.seed(550)

# Load libraries
library(caTools)
library(ROCR)
library(Hmisc)
library(plyr)
library(binr)
library(gmodels)
library(cluster)
library(gmodels)
library(party)
library(dplyr)
library(RMySQL)
library(openxlsx)

# Load decision trees models for each time and return rate bucket
ranges <- c(0,30,60,90,120,150,180,240,300)
ranges_pct <- c(0,5,10,20,30,40,50,60,70,80,90,100)

for (j in 1:length(ranges_pct)){
  setwd(paste('C:\\Projects\\LGD_model\\models\\', ranges_pct[j], 'pct\\', sep=""))
  fname <- paste('Decision_Trees_',ranges,'_', ranges_pct[j],'pct.rdata', sep="")
  lapply(fname,load,.GlobalEnv)
  for (i in 1:length(ranges)){
    assign(paste0("dt",ranges[i], '_', ranges_pct[j], 'pct', sep=""), 
           get(noquote(paste("decision_tree_model_", ranges[i], sep=""))), 
           envir = .GlobalEnv)
  }
}
rm(list=ls(pattern="^decision_tree_model"))

# Load data to score 
setwd("C:\\Projects\\LGD_model\\Apply_LGD\\")
df <- read.csv("alls_input.csv")

# Load data to score 
credits <- read.xlsx("Accounting.xlsx")
credits <- as.data.frame(credits)
names(credits) <- c("credit_number","Saldo", "current_days_delay")

# Select credits to score
df <- merge(df , credits, by.x="credit_number", by.y="credit_number", TRUE, all.y=TRUE)

# Take correct days of delay
df$days_delay <- df$current_days_delay

# Select delayed
df <- subset(df,df$days_delay>=90)

# Correct data
df$days_diff_last_credit <- ifelse(df$days_diff_last_credit<0,0,df$days_diff_last_credit)
df$ratio_last_amount_paid <- ifelse(is.na(df$ratio_last_amount_paid),0,df$ratio_last_amount_paid)
df$ratio_installment_income <- as.numeric(df$ratio_installment_income)
df$age <- ifelse(df$age>90 | df$age<18, NA, df$age)

# Convert to integers
df$age <- as.integer(df$age)
df$education <- as.integer(df$education)
df$experience_employer <- as.integer(df$experience_employer)
df$gender <- as.integer(df$gender)
df$period <- as.integer(df$period)
df$credits_cum <- as.integer(df$credits_cum)
df$ownership <- as.integer(df$ownership)
df$last_payment_before_def <- as.integer(df$last_payment_before_def)
df$max_delay <- as.integer(df$max_delay)

# Convert to doubles
df$ratio_installment_income <- as.numeric(df$ratio_installment_income)
df$days_diff_last_credit <- as.numeric(df$days_diff_last_credit)
df$ratio_passed_installments <- as.numeric(df$ratio_passed_installments)

# Bin days of delay
df$days_delay_bin <- cut(df$days_delay,c(0,90,90+60,90+90,120+90,150+90,180+90,240+90,300+90,100000))
options(scipen=999)

# Get real final EAD for each credit according to delay days
df$realEAD <- ifelse(df$days_delay<(days_def + 30), df$EAD, ifelse(df$days_delay<(days_def + 60), df$EAD30,
              ifelse(df$days_delay<(days_def + 90), df$EAD60, ifelse(df$days_delay<(days_def + 120), df$EAD90,
              ifelse(df$days_delay<(days_def + 150), df$EAD120, ifelse(df$days_delay<(days_def + 180), df$EAD150,
              ifelse(df$days_delay<(days_def + 240), df$EAD180, ifelse(df$days_delay<(days_def + 300), df$EAD240, 
                                                                       df$EAD300))))))))

# Define cutoffs for each bucket 
cut_0pct <- c(0.5,0.5,0.4,0.4,0.35,0.3,0.2,0.15,0.1)
cut_5pct <- c(0.6,0.45,0.4,0.3,0.3,0.25,0.2,0.15,0.1)
cut_10pct <- c(0.5,0.45,0.35,0.3,0.3,0.25,0.2,0.1,0.075)
cut_20pct <- c(0.5,0.35,0.3,0.275,0.25,0.2,0.2,0.1,0.075)
cut_30pct <- c(0.4,0.35,0.3,0.25,0.2,0.2,0.15,0.1,0.075)
cut_40pct <- c(0.4,0.35,0.3,0.25,0.2,0.15,0.125,0.1,0.075)
cut_50pct <- c(0.4,0.35,0.3,0.25,0.2,0.15,0.15,0.1,0.075)
cut_60pct <- c(0.35,0.3,0.25,0.2,0.2,0.15,0.15,0.1,0.075)
cut_70pct <- c(0.35,0.3,0.25,0.2,0.2,0.15,0.125,0.1,0.075)
cut_80pct <- c(0.35,0.3,0.25,0.2,0.15,0.15,0.1,0.075,0.075)
cut_90pct <- c(0.3,0.3,0.25,0.2,0.2,0.15,0.15,0.1,0.075)
cut_100pct <- c(0.3,0.25,0.25,0.225,0.2,0.175,0.1,0.1,0.075)

# Set payment diffs to current
df$last_payment_before_def <- as.integer(df$current_time_diff)
df$payment_time_diff30 <- df$current_time_diff
df$payment_time_diff60 <- df$current_time_diff
df$payment_time_diff90 <- df$current_time_diff
df$payment_time_diff120 <- df$current_time_diff
df$payment_time_diff150 <- df$current_time_diff
df$payment_time_diff180 <- df$current_time_diff
df$payment_time_diff240 <- df$current_time_diff
df$payment_time_diff300 <- df$current_time_diff



# Define function to calculate predicted return rates for each bucket
RR_fct <- function(cutoff_vect, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9){
  return(ifelse(df$days_delay<(days_def + 30), ifelse(predict(mod1, df)<=cutoff_vect[1],0,1),
         ifelse(df$days_delay<(days_def + 60), ifelse(predict(mod2, df)<=cutoff_vect[2],0,1),
         ifelse(df$days_delay<(days_def + 90), ifelse(predict(mod3, df)<=cutoff_vect[3],0,1),
         ifelse(df$days_delay<(days_def + 120), ifelse(predict(mod4, df)<=cutoff_vect[4],0,1),
         ifelse(df$days_delay<(days_def + 150), ifelse(predict(mod5, df)<=cutoff_vect[5],0,1),
         ifelse(df$days_delay<(days_def + 180), ifelse(predict(mod6, df)<=cutoff_vect[6],0,1),
         ifelse(df$days_delay<(days_def + 240), ifelse(predict(mod7, df)<=cutoff_vect[7],0,1),
         ifelse(df$days_delay<(days_def + 300), ifelse(predict(mod8, df)<=cutoff_vect[8],0,1),
         ifelse(predict(mod9, df)<=cutoff_vect[9],0,1))))))))))
}

# Calculate return rate for each RR bucket
RR0pct <- RR_fct(cut_0pct,dt0_0pct,dt30_0pct,dt60_0pct,dt90_0pct,
                 dt120_0pct,dt150_0pct,dt180_0pct,dt240_0pct,dt300_0pct)
RR5pct <- RR_fct(cut_5pct,dt0_5pct,dt30_5pct,dt60_5pct,dt90_5pct,
                 dt120_5pct,dt150_5pct,dt180_5pct,dt240_5pct,dt300_5pct)
RR10pct <- RR_fct(cut_10pct,dt0_10pct,dt30_10pct,dt60_10pct,dt90_10pct,
                  dt120_10pct,dt150_10pct,dt180_10pct,dt240_10pct,dt300_10pct)
RR20pct <- RR_fct(cut_20pct,dt0_20pct,dt30_20pct,dt60_20pct,dt90_20pct,
                  dt120_20pct,dt150_20pct,dt180_20pct,dt240_20pct,dt300_20pct)
RR30pct <- RR_fct(cut_30pct,dt0_30pct,dt30_30pct,dt60_30pct,dt90_30pct,
                  dt120_30pct,dt150_30pct,dt180_30pct,dt240_30pct,dt300_30pct)
RR40pct <- RR_fct(cut_40pct,dt0_40pct,dt30_40pct,dt60_40pct,dt90_40pct,
                  dt120_40pct,dt150_40pct,dt180_40pct,dt240_40pct,dt300_40pct)
RR50pct <- RR_fct(cut_50pct,dt0_50pct,dt30_50pct,dt60_50pct,dt90_50pct,
                  dt120_50pct,dt150_50pct,dt180_50pct,dt240_50pct,dt300_50pct)
RR60pct <- RR_fct(cut_60pct,dt0_60pct,dt30_60pct,dt60_60pct,dt90_60pct,
                  dt120_60pct,dt150_60pct,dt180_60pct,dt240_60pct,dt300_60pct)
RR70pct <- RR_fct(cut_70pct,dt0_70pct,dt30_70pct,dt60_70pct,dt90_70pct,
                  dt120_70pct,dt150_70pct,dt180_70pct,dt240_70pct,dt300_70pct)
RR80pct <- RR_fct(cut_80pct,dt0_80pct,dt30_80pct,dt60_80pct,dt90_80pct,
                  dt120_80pct,dt150_80pct,dt180_80pct,dt240_80pct,dt300_80pct)
RR90pct <- RR_fct(cut_90pct,dt0_90pct,dt30_90pct,dt60_90pct,dt90_90pct,
                  dt120_90pct,dt150_90pct,dt180_90pct,dt240_90pct,dt300_90pct)
RR100pct <- RR_fct(cut_100pct,dt0_100pct,dt30_100pct,dt60_100pct,dt90_100pct,
                   dt120_100pct,dt150_100pct,dt180_100pct,dt240_100pct,dt300_100pct)

# Join to main data frame
df <- cbind(df, RR0pct, RR5pct, RR10pct, RR20pct, RR30pct, RR40pct, 
                RR50pct, RR60pct, RR70pct, RR80pct, RR90pct, RR100pct)

# Join binary return rates
df$RR1 <- ifelse(df$RR0pct==0, 0,
          ifelse(df$RR5pct==0, 0.025,
          ifelse(df$RR10pct==0, 0.075,
          ifelse(df$RR20pct==0, 0.15,
          ifelse(df$RR30pct==0, 0.25,
          ifelse(df$RR40pct==0, 0.35,
          ifelse(df$RR50pct==0, 0.45,
          ifelse(df$RR60pct==0, 0.55,
          ifelse(df$RR70pct==0, 0.65,
          ifelse(df$RR80pct==0, 0.75,
          ifelse(df$RR90pct==0, 0.85,
          ifelse(df$RR100pct==0, 0.95,1))))))))))))

df$RR2 <- ifelse(df$RR100pct==1, 1,
          ifelse(df$RR90pct==1, 0.95,
          ifelse(df$RR80pct==1, 0.85,
          ifelse(df$RR70pct==1, 0.75,
          ifelse(df$RR60pct==1, 0.65,
          ifelse(df$RR50pct==1, 0.55,
          ifelse(df$RR40pct==1, 0.45,
          ifelse(df$RR30pct==1, 0.35,
          ifelse(df$RR20pct==1, 0.25,
          ifelse(df$RR10pct==1, 0.15,
          ifelse(df$RR5pct==1, 0.075,
          ifelse(df$RR0pct==1, 0.025,0))))))))))))

# Treat predicted return rate to produce more accurate results
df$RR <- (df$RR1 + df$RR2) / 2

# Get collected amounts
df$collected <- ifelse(df$Saldo<0, 0, df$RR * df$Saldo)
sum(df$collected)/sum(df$Saldo)*100

# Bin return rate
df$RR_bin <- cut(df$RR, c(-1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99,1))
table(df$RR_bin)/sum(table(df$RR_bin))*100

# Analysis 
df_analysis <- df[,c("credit_number","payment_time_diff300","education","experience_employer","days_delay","RR",
                     "RR0pct","RR5pct","RR10pct","RR20pct","RR30pct","RR40pct","RR50pct","RR60pct","RR70pct","RR80pct"
                     ,"RR90pct","RR100pct")]



# END #
