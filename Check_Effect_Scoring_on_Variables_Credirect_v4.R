

#############
# Libraries #
#############

library(smbinning)
library(caTools)
library(ROCR)
library(Hmisc)
library(dummies)
library(plyr)
library(binr)
library(My.stepwise)
library(Information)



##########################
# Load and set dataframe #
##########################

# Load data #
setwd("C:\\Projects\\Application_Scoring\\Credirect_v4\\data\\")
df <- read.csv("input_current_all.csv", sep=",")
df_raw <- df

# Choose variables
list_var_model <-  c("default_flag","date",
"age","education","status_work","ownership","experience_employer",
"marital_status","on_address","maturity","purpose","household_total",
"status_active_total","status_finished_total",
"outs_overdue_ratio_total","cred_count_total","source_entity_count_total",
"viber","whatsapp"
)
df <- df[,list_var_model]





###############################
# Cut and bin as if scorecard #
###############################

# Maturity :
plot_infotables(IV, "maturity")
df$maturity <- ifelse(df$maturity<=4,"3_4",
               ifelse(df$maturity<=6,"5_6",
               ifelse(df$maturity<=8,"7_8",
               ifelse(df$maturity<=12,"9_12","more_12"))))
df$maturity <- as.factor(df$maturity)

# Age :
df$age <- ifelse(df$age<=20,"20_less",
    ifelse(df$age<=30,"20_30",
    ifelse(df$age<=40,"30_40",
           ifelse(df$age<=50,"40_50","50+"))))      
df$age <- as.factor(df$age)

# Household total :
df$household_total <- ifelse(is.na(df$household_total), "not_2",
   ifelse(df$household_total==2,"2","not_2"))
df$household_total <- as.factor(df$household_total)

# Experience employer : 
df$experience_employer <- 
  ifelse(is.na(df$experience_employer), "NA",
  ifelse(df$experience_employer<=1,"0_1",
  ifelse(df$experience_employer<=12,"2_12",
  ifelse(df$experience_employer<=120,"13_120","120+"))))
df$experience_employer <- as.factor(df$experience_employer)

# ON ADDRESS  :
df$on_address <- 
  ifelse(is.na(df$on_address), "NA",
  ifelse(df$on_address<=1,"0_1",
  ifelse(df$on_address<=12,"2_12",
  ifelse(df$on_address<=120,"13_120","120+"))))
df$on_address <- as.factor(df$on_address)

# Source entity count : 
df$source_entity_count_total <- 
  ifelse(is.na(df$source_entity_count_total),"0_4",
         ifelse(df$source_entity_count_total<=4, "0_4","more_5"))
df$source_entity_count_total <- as.factor(df$source_entity_count_total)

# Cred count total : 
df$cred_count_total <- ifelse(is.na(df$cred_count_total), "1_3",
  ifelse(df$cred_count_total<=3,"1_3","more_4"))
df$cred_count_total <- as.factor(df$cred_count_total)

# Ratio : YES 
df$outs_overdue_ratio_total <- ifelse(
  is.na(df$outs_overdue_ratio_total), "0_0.01",
  ifelse(df$outs_overdue_ratio_total==-999,"0_0.01",
   ifelse(df$outs_overdue_ratio_total<=0.01,"0_0.01",
   ifelse(df$outs_overdue_ratio_total<=0.21,"0.01_0.21","more_0.21"))))
df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total)





###########################################################################
# Check if population has changed before and after scorecards implemented #
###########################################################################

df$before_score <- ifelse(df$date<="2019-06-20",1,
                   ifelse(df$date<="2020-06-20",2,3))

gen_dist <- function(input){
  return(round(t(t(table(df[,c(input)],df$before_score,exclude = NULL))/
    colSums(table(df[,c(input)],df$before_score,exclude = NULL))),2))
}

df <- df[ , -which(names(df) %in% c("date"))]

for(i in 1:ncol(df)){
  print(names(df)[i])
  print(gen_dist(names(df)[i]))
}



