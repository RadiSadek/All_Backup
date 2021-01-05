


################################################
### BUILD PREDICTION MODEL FOR PROVISIONNING ###
###     MULTIVARIATE TIME SERIES BASED ON    ###
###        VECTOR AUTO REGRESSION            ###
###  (BASED ON AGGREGATE PORTFOLIO PER DPD)  ###
################################################



############## READ DATA ####################
 
# Read libraries
suppressWarnings(suppressMessages(library(openxlsx)))
suppressWarnings(suppressMessages(library(vars)))

# Define useful functions
agg_month_receiv <- function(input,var,beg,end,name){
  result <- as.data.frame(
  aggregate(var$total_receivables_end[var$days_delay>beg & 
                                      var$days_delay<=end], 
  by=list(var$month[var$days_delay>beg & var$days_delay<=end]),FUN=sum))
  names(result) <- c("month",name)
  return(merge(input,result, by.x = "month", by.y = "month",
                     all.x = TRUE))
}
agg_month_distr_pop <- function(input,var,dpd){
  result <- merge(input, var[var$dpd_bin==dpd,],
                  by.x = "month", by.y = "month", all.x = TRUE)
  names(result)[ncol(result)] <- c(paste("dpd_",dpd,sep=""))
  result <- result[,-c(ncol(result)-1)]
  return(result)
}

# Read past accounting files
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
info <- df[,c("credit_number","online_offline")]
load("C:\\Projects\\Times_Series_Provisions\\data\\all_accountings_201520162017201820192020.rdata")
df <- final
df <- merge(df, info, by.x = "credit_number", by.y = "credit_number",
            all.x = TRUE)

# Work on City Cash solely for now
df <- subset(df,df$online_offline=="offline")




############## ARRANGE FIELDS ####################

# Arrange some fields
df$collateral_value <- 
  ifelse(is.na(df$collateral_value),0,df$collateral_value)
df$total_receivables_end <- 
  ifelse(is.na(df$total_receivables_end),0,df$total_receivables_end)
df$cession_amount <- ifelse(is.na(df$cession_amount),0,df$cession_amount)
df$total_receivables_end <- as.numeric(df$total_receivables_end)
df$collateral_value_end <- as.numeric(df$collateral_value_end)
df$collateral_receivable <- ifelse(
  (df$total_receivables_end - df$collateral_value)<=0, 0, 
  (df$total_receivables_end - df$collateral_value))

# Compute Expected Loss
df$EL <- 
  ifelse(df$collateral_receivable<=0, 0,
  ifelse(df$days_delay<=30, 0,
  ifelse(df$days_delay<=90, 0.1*df$collateral_receivable,
  ifelse(df$days_delay<=180, 0.5*df$collateral_receivable,
         df$collateral_receivable))))

# Compute Expected Loss without cession
df$EL_without_cession <- df$EL + df$cession_amount

# Aggregate by month
agg_month <- as.data.frame(aggregate(df$EL, by=list(df$month),FUN=sum))
names(agg_month) <- c("month","EL")
agg_month_wc <- as.data.frame(aggregate(df$EL_without_cession, 
                  by=list(df$month),FUN=sum))
names(agg_month_wc) <- c("month","EL_without_cession")
agg_month <- merge(agg_month, agg_month_wc, by.x = "month", by.y = "month",
                   all.x = TRUE)
agg_month$provision <- 0
for (i in 2:nrow(agg_month)){
  agg_month$provision[i] <- agg_month$EL_without_cession[i] - 
    agg_month$EL[i-1]
}

# Get portfolio and cession at beginning
agg_month_receiv_end <- as.data.frame(aggregate(df$total_receivables_end, 
    by=list(df$month),FUN=sum))
names(agg_month_receiv_end) <- c("month","portfolio")
agg_month <- merge(agg_month, agg_month_receiv_end, by.x = "month", 
                   by.y = "month",
                   all.x = TRUE)
agg_month_cession <- as.data.frame(aggregate(df$cession_amount, 
                                                by=list(df$month),FUN=sum))
names(agg_month_cession) <- c("month","cession")
agg_month <- merge(agg_month, agg_month_cession, by.x = "month", 
                   by.y = "month",
                   all.x = TRUE)
agg_month$portfolio_beg <- 0
agg_month$cession_beg <- 0
for (i in 2:nrow(agg_month)){
  agg_month$portfolio_beg[i] <- agg_month$portfolio[i-1] 
  agg_month$cession_beg[i] <- agg_month$cession[i-1]
}

# Make distrbution based on days_delay and aggregate
df$days_delay <- ifelse(is.na(df$days_delay),0,df$days_delay)
df$days_delay_bin <- cut(df$days_delay,c(-Inf,0,15,30,45,60,75,90,120,150,180,
                                         Inf))
pct_days_delay <- as.data.frame(round(table(df$month,df$days_delay_bin)/
  rowSums(table(df$month,df$days_delay_bin)),4))
names(pct_days_delay) <- c("month","dpd_bin","dpd_pct")

# Aggregate distrubtion by DPD 
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(-Inf,0]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(0,15]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(15,30]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(30,45]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(45,60]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(60,75]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(75,90]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(90,120]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(120,150]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(150,180]")
agg_month <- agg_month_distr_pop(agg_month,pct_days_delay,"(180, Inf]")

# Aggregate portfolio but according to days of delay
agg_month <- agg_month_receiv(agg_month,df,-Inf,0,"portfolio0")
agg_month <- agg_month_receiv(agg_month,df,0,15,"portfolio0_15")
agg_month <- agg_month_receiv(agg_month,df,15,30,"portfolio15_30")
agg_month <- agg_month_receiv(agg_month,df,30,45,"portfolio30_45")
agg_month <- agg_month_receiv(agg_month,df,45,60,"portfolio45_60")
agg_month <- agg_month_receiv(agg_month,df,60,75,"portfolio60_75")
agg_month <- agg_month_receiv(agg_month,df,75,90,"portfolio75_90")
agg_month <- agg_month_receiv(agg_month,df,90,120,"portfolio90_120")
agg_month <- agg_month_receiv(agg_month,df,120,150,"portfolio120_150")
agg_month <- agg_month_receiv(agg_month,df,150,180,"portfolio150_180")
agg_month <- agg_month_receiv(agg_month,df,180,Inf,"portfolio180_more")






############## MAKE CHECKS FOR DATA ####################

# View auto-correlation
View(abs(cor(agg_month[,-1])))

# Check for stationarity of provisions
rollmean_df <- as.data.frame(
  rollmean(zoo(agg_month$provision[2:nrow(agg_month)],
  agg_month$month[2:nrow(agg_month)]),12))
rollmean_df$names <- rownames(rollmean_df)
names(rollmean_df) <- c("rollmean_prov","month")
agg_month <- merge(agg_month,rollmean_df,by.x = "month",
                   by.y = "month",all.x = TRUE)
agg_month$id <- seq(1:nrow(agg_month))

# Plot rolling mean by 8 months for provisions
plot(agg_month$id[2:nrow(agg_month)], 
     agg_month$provision[2:nrow(agg_month)], type="o", col="blue", pch="o")
points(agg_month$id[2:nrow(agg_month)], 
       agg_month$rollmean_prov[2:nrow(agg_month)], col="red", pch="*")
lines(agg_month$id[2:nrow(agg_month)],
      agg_month$rollmean_prov[2:nrow(agg_month)], col="red",lty=2)

# Conclusion : data becomes stationary after jan 2017

# Check differentialy of data
plot(diff(agg_month$provision),col="red",type = "o")






############## BUILD REAL PROVISIONS AND CHECK MODEL ####################

# Choose correct time frame
agg_month <- subset(agg_month,agg_month$month>="2017-01")

# Build VAR model
var_model <- VAR(agg_month[
  2:(nrow(agg_month)),
  c("provision","portfolio150_180","portfolio0","portfolio75_90","cession")] ,
    type = "const")
summary(var_model$varresult$provision)

# Assess quality of results
agg_month$predict <- 0
for (i in 2:nrow(agg_month)){
  agg_month$predict[i] <- 
    var_model$varresult$provision$coefficients[1] * 
    agg_month$provision[i-1] + 
    
    var_model$varresult$provision$coefficients[2] * 
    agg_month$portfolio150_180[i-1] +
    
    var_model$varresult$provision$coefficients[3] * 
    agg_month$portfolio0[i-1] + 
    
    var_model$varresult$provision$coefficients[4] * 
    agg_month$portfolio75_90[i-1] + 
    
    var_model$varresult$provision$coefficients[5] * 
    agg_month$cession[i-1] + 
    
    var_model$varresult$provision$coefficients[6]
}
agg_month$error <- round(
  abs((agg_month$predict - agg_month$provision) / agg_month$provision),3)*100
plot(agg_month$error[order(agg_month$error)])
mean(agg_month$error[2:nrow(agg_month)])

# Carry-out analysis
analysis <- agg_month[,c("month","provision","predict", "error","portfolio",
                          "portfolio150_180","portfolio0","portfolio75_90",
                          "cession")]
plot(agg_month$provision[2:nrow(agg_month)],
     agg_month$predict[2:nrow(agg_month)],col='blue', lwd=5,
     xlab = "real provisions", ylab = "predicted provisions")
par(new=TRUE)
plot(cbind(c(agg_month$provision[2],agg_month$predict[2]),
           c(agg_month$provision[2],agg_month$predict[2])),
     col="red",type = "o",lwd=5, axes=F,xlab='',ylab='')

# Output coefficients
coefficients <- cbind(var_model$varresult$provision$coefficients[1],
                      var_model$varresult$provision$coefficients[2],
                      var_model$varresult$provision$coefficients[3],
                      var_model$varresult$provision$coefficients[4],
                      var_model$varresult$provision$coefficients[5],
                      var_model$varresult$provision$coefficients[6])
save(coefficients,
 file="C:\\Projects\\Times_Series_Provisions\\coefficients\\coefficients.rdata")

# Apply to new month to predict next month
predict_provision <- 
  var_model$varresult$provision$coefficients[1] * 
  agg_month$provision[nrow(agg_month)] + 
  
  var_model$varresult$provision$coefficients[2] * 
  agg_month$portfolio150_180[nrow(agg_month)] +
  
  var_model$varresult$provision$coefficients[3] * 
  agg_month$portfolio0[nrow(agg_month)] + 
  
  var_model$varresult$provision$coefficients[4] * 
  agg_month$portfolio75_90[nrow(agg_month)] + 
  
  var_model$varresult$provision$coefficients[5] * 
  agg_month$cession[nrow(agg_month)] + 
  
  var_model$varresult$provision$coefficients[6]

# Display prediction for next month
cat("The prediction for next month are", round(predict_provision,2) ,"ыт.")

