

######### Load and set dataframe

# Define working directory
setwd("C:\\Projects\\Ad_Hoc\\Budget_Previsionning\\Predict_Credit_Counts_per_Month\\")

# Manual choose months and years ahead
months_ahead <- c(10,11,12,1,2,3,4,5,6,7,8,9,10,11,12)
years_ahead <- c(2020,2020,2020,
                 2021,2021,2021,2021,2021,2021,2021,2021,2021,2021,2021,2021)

# Define target counts for entire year
target_next_year <- 83000

# Call libraries 
library(RMySQL)
library(openxlsx)
library(mltools)
library(rworldmap)
library(vars)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Read entire dataframe
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

# Subset correctly
df <- subset(df,df$online_offline=="offline")
df <- subset(df,df$status %in% c(4,5))
df <- subset(df,is.na(df$sub_status) | !(df$sub_status %in% c(122)))

# Create month field
df$month <-  substring(df$date,1,7)

# Create month dataframe
months <- as.data.frame(table(df$month))
names(months) <- c("month","counts")
months$id <- seq(1,nrow(months),1)

# Create months and year ID 
months$month_id <- as.numeric(substring(months$month,6,7))
months$year_id <- as.numeric(substring(months$month,1,4))



##########Check effect of monthly working days 

working_days <- read.xlsx("R\\Working_Days_per_Month.xlsx")
months <- merge(months,working_days,by.x = "month", by.y = "month",all.x = TRUE)
months$counts <- months$counts / months$working_days



######### Create trend and remove trend 

# Take a look at the monhtly evolution of counts
plot(months$id,months$counts)

# Remove credits prior to Nov 2020 (no apparent seasonality fluctuation)
months <- subset(months,months$id>40)

# Remove incomplete most recent month
months <- months[1:(nrow(months)-1),]
plot(months$id,months$counts)

# Create linear regression to get trend
fit <- lm(months$counts ~ months$id)
summary(fit)

# Create trend and remove trend
months$trend <- fit$coefficients[1] + fit$coefficients[2] * months$id
months$minus_trend <- round(months$counts - months$trend,2)
matplot(months$id, cbind(months$counts,months$trend),
        type="l",col=c("red","blue"),lty=c(1,1),lwd=c(3,3),
        ylab="counts",xlab="month id")
legend("topleft", inset=.02,legend =c("counts","trend"), 
       lty=c(1,1),lwd=c(3,3),col=c("red","blue"))
matplot(months$id, months$minus_trend,
        type="l",col=c("black"),lty=c(1),lwd=c(3),
        xlab="month id",ylab="seasonality = trend - real counts")



######### Create seasonality hypothesis (real - trend)

# Choose data set for seasonality approximation (remove Coronavirus effect)
months_for_season <- subset(months,!(months$month_id>2 & months$year_id==2020))

# Approximate seasonality (hypothesis of average per month)
season <- as.data.frame(round(aggregate(months_for_season$minus_trend[
  months_for_season$year_id %in% c(2019,2018)],
  by=list(months_for_season$month_id[months_for_season$year_id %in% c(2019,2018)]),
  FUN=mean),2))
names(season) <- c("month_id","seasonality")

# Draw plot of averaged seasonality
matplot(season$month_id, season$seasonality,
        type="l",col=c("green"),lty=c(1),lwd=c(3),
        ylab="seasonality counts per working day",xlab="month")
legend("topleft", inset=.02,legend =c("seasonality model"), 
       lty=c(1),lwd=c(3),col=c("green"))
axis(side=1, at=seq(1,12,by=1), labels = FALSE)

# Get real data year-by-year to assess seasonality approximation
season <- merge(season,months[months$year_id==2017,
                c("month_id","minus_trend")],
                by.x = c("month_id"), by.y = c("month_id"),all.x = TRUE)
season <- merge(season,months[months$year_id==2018,
                              c("month_id","minus_trend")],
                by.x = c("month_id"), by.y = c("month_id"),all.x = TRUE)
season <- merge(season,months[months$year_id==2019,
                              c("month_id","minus_trend")],
                by.x = c("month_id"), by.y = c("month_id"),all.x = TRUE)
season <- suppressWarnings(merge(season,months[months$year_id==2020,
                              c("month_id","minus_trend")],
                by.x = c("month_id"), by.y = c("month_id"),all.x = TRUE))

# Plot real data year-by-year
names(season)[3:6] <- c("2017","2018","2019","2020")
matplot(season$month_id, cbind(season[[2]],season[[3]],season[[4]],season[[5]],
        season[[6]]),type="l",col=c("black","red","blue","green","orange"),
        xlab = "month",ylab = "seasonality (normal counts - trend)",
        ylim = c(-100,80),
        lty=c(1,1,2,3,4),lwd=c(3,3,3,3,3))
legend("bottomleft", inset=.02,
       legend =c("average fit","2017","2018","2019","2020"), 
       lty=c(1,1,2,3,4),lwd=c(3,3,3,3,3), 
       col=c("black","red","blue","green","orange"))
axis(side=1, at=seq(1,12,by=1), labels = FALSE)

# Rejoin to main data frame
months <- merge(months,season[,c("month_id","seasonality")],
                by.x = "month_id",by.y = "month_id",all.x = TRUE)
months <- months[order(months$id),]
months$approx <- months$trend + months$seasonality
matplot(months$id, cbind(months$minus_trend,months$seasonality),
        type="l",col=c("black","green"),lty=c(1,1),lwd=c(3,3),
        xlab = "month id",ylab = "counts", ylim = c(-80,80))
legend("bottomleft", inset=.02,
       legend =c("seasonality","seasonality approximation"), 
       lty=c(1,1),lwd=c(3,3), 
       col=c("black","green"))



######### Create time-series model

# Make predictions with time series
var_model <- VAR(months[2:(nrow(months)),c("counts","id")] ,type = "const")
summary(var_model$varresult$counts)

# Apply time series to main dataframe
months$predict <- NA
for (i in 2:nrow(months)){
  months$predict[i] <- 
    var_model$varresult$counts$coefficients[1] * 
    months$counts[i-1] + 
    var_model$varresult$counts$coefficients[2] * 
    months$id[i-1] + var_model$varresult$counts$coefficients[3]
}

# Plot time series results 
matplot(months$id, cbind(months$trend,months$counts,
                         months$approx,months$predict),
        type="l",col=c("blue","red","green","orange"),lty=c(1,1),lwd=c(3,3))
legend("topleft", inset=.02,
       legend =c("trend","real counts","trend + seasonality","time series"), 
       lty=c(1,1,1,1),lwd=c(3,3,3,3), 
       col=c("blue","red","green","orange"))



######### Make predictions based on modeled trend and modeled seasonality

# Create dataframe for prediction of next 1-year 
months <- months[,-which(names(months) %in% c("working_days"))]
predict_df <- as.data.frame(suppressWarnings(cbind(months_ahead,NA,NA,
     seq(max(months$id)+1,max(months$id)+length(years_ahead),1),
     NA,NA,NA,NA,NA,NA)))
names(predict_df) <- c(names(months))
months_predict <- rbind(months,predict_df)
months_predict <- months_predict[ , 
     -which(names(months_predict) %in% c("seasonality"))]
months_predict  <- 
  merge(months_predict,season[,c("month_id","seasonality")],
        by.x = "month_id",by.y = "month_id",all.x = TRUE)
months_predict <- months_predict[ , 
     -which(names(months_predict) %in% c("minus_trend"))]
names(months_predict)[8] <- "approx_time_series"
months_predict <- months_predict[order(months_predict$id),]

months_predict$year_id[(min(months_predict$id[is.na(months_predict$counts)]) 
  - min(months_predict$id) + 1):nrow(months_predict)] <- 
  c(2020,2020,2020,
    2021,2021,2021,2021,2021,2021,2021,2021,2021,2021,2021,2021)
months_predict$month <- paste(months_predict$year_id,months_predict$month_id,
    sep = "-")
months_predict$month <- ifelse(nchar(months_predict$month)==6,
    gsub("-", "-0", months_predict$month),months_predict$month)
                              
                               

# Apply models trend to future
months_predict$trend <- fit$coefficients[1] + 
  fit$coefficients[2] * months_predict$id 

# Apply models trend + seasonality to future
months_predict$approx <- months_predict$trend + months_predict$seasonality

# Apply times series to future
for (i in (min(months_predict$id[is.na(months_predict$counts)]) 
           - min(months_predict$id) + 1):nrow(months_predict)){
  months_predict$approx_time_series[i] <- 
    var_model$varresult$counts$coefficients[1] * 
    months_predict$approx_time_series[i-1] + 
    var_model$varresult$counts$coefficients[2] * 
    months_predict$id[i-1] + var_model$varresult$counts$coefficients[3]
}

# Plot final result
matplot(months_predict$id, cbind(months_predict$counts,
                                 months_predict$trend,months_predict$approx,
                                 months_predict$approx_time_series),
        type="l",col=c("red","blue","green","orange"),
        lty=c(1,1,1,1),lwd=c(3,3,3,3),
        ylab="counts",xlab="month id",
        lend = par("lend"))
legend("topleft", inset=.02,
       legend =c("real counts","trend","trend + seasonality"),
       col=c("red","blue","green"),lty=c(1,1,1),lwd=c(3,3,3))



####### Taking into account fixed total previsions of yearly counts

# Make final dataframe 
months_final <- months_predict[,c("month","counts","month_id","id","trend",
                                  "approx","approx_time_series","seasonality")]
months_final <- merge(months_final,working_days,
                      by.x = "month", by.y = "month",all.x = TRUE)
months_final <- months_final[order(months_final$id),]


# Make empty vector for next year fixed counts
next_year_fixed_counts <- rep(NA,12)

# Get first month of next year (starting point of next year prediction)
next_year_fixed_counts[1] <- months_final$approx[
  months_final$month=="2021-01"] 

# Get last month of next year 
# (and assume that gradient is equal to first month - average but reverse)
next_year_fixed_counts[12] <- target_next_year/12/mean(
  months_final$working_days[substring(months_final$month,1,4)=="2021"]) - 
  next_year_fixed_counts[1] +
  target_next_year/12/mean(
    months_final$working_days[substring(months_final$month,1,4)=="2021"])

# Compute steepness
steepness <- (next_year_fixed_counts[12] - next_year_fixed_counts[1])/11

# Recompute fixed counts per year
for (i in 2:length(next_year_fixed_counts)){
  next_year_fixed_counts[i] <- next_year_fixed_counts[i-1] +  steepness
}

# Make new field with 
months_final$trend_chosen <- months_final$trend
months_final$trend_chosen[51:nrow(months_final)] <- next_year_fixed_counts
months_final$approx_chosen <- months_final$trend_chosen + 
  months_final$seasonality

# Plot final result
matplot(months_final$id, cbind(months_final$counts,
        months_final$trend,months_final$approx,
        months_final$trend_chosen),
        type="l",col=c("red","blue","green","blue"),lty=c(1,1,1,4),
        lwd=c(3,3,3,3,3),ylab="counts per working day",
        xlab="month id",lend = par("lend"))
legend("topleft", inset=.02,
       legend =c("real counts","past trend","prediction (past trend)",
                 "chosen trend"),
       col=c("red","blue","green","blue","green"),lty=c(1,1,1,4),
       lwd=c(3,3,3,3))

# Output final result
write.xlsx(months_final,"results\\results.xlsx")

# The end 

