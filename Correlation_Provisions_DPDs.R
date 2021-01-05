

#############################################
### BUILD LINEAR MODEL BETWEEN PROVISIONS ###
###                 AND                   ###
###       INCREASE IN SUM OF DELAYS       ###
#############################################



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




############## READ DATA ####################

# Read data
all_month <- read.xlsx(paste("C:\\Projects\\Provisions_Modeled\\data\\",
 "provisions_and_delayed_sums\\Delayed_Sums_and_Other_for_Modeling.xlsx",sep=""))

# Set fields
all_month$year <- substring(all_month$month,1,4)
all_month$id <- seq(1,nrow(all_month),1)

# Rename fields
names(all_month)[2] <- c("delayed_sum")

# Reselect rows
all_month <- all_month[c(1:(nrow(all_month)-1)),]



############# BUILD LINEAR REGRESSION BETWEEN PROVISION AND DELAYED SUM

# Read relevant years for linear regression 
all_month_fit <- subset(all_month,all_month$year %in% 
                          c("2016","2017","2018","2019","2020"))

# # Remove certain outliers
all_month_fit <- all_month_fit[!(all_month_fit$month %in% c("2020-04",
                                                            "2020-03")),]
# Make linear regression to check relations vis-a-vis the DPD
fit <- lm(delayed_sum ~ provisions,
          data=all_month_fit)
summary(fit)
coefficients(fit)
residuals(fit)
quantile(abs(residuals(fit)/mean(all_month_fit$delayed_sum,na.rm = TRUE)*100))
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

# Predict with linear regression
all_month_fit$delayed_sum_predict <- fit$coefficients[1] + 
  fit$coefficients[2] * all_month_fit$provisions
matplot(all_month_fit$id, 
        cbind(all_month_fit$delayed_sum,all_month_fit$delayed_sum_predict),
        type="l",col=c("red","blue"),lty=c(1,1),lwd=c(3,3),
        ylab="counts",xlab="month id")
legend("topleft", inset=.02,legend =c("delayed sum","delayed sum predicted"), 
       lty=c(1,1),lwd=c(3,3),col=c("red","blue"))

# Compute error
all_month_fit$error <- round(abs((
  abs(all_month_fit$delayed_sum_predict - all_month_fit$delayed_sum) /
  all_month_fit$delayed_sum)*100),1)
aggregate(all_month_fit$error,by=list(all_month_fit$year),FUN=mean)
aggregate(all_month_fit$error,by=list(all_month_fit$year),FUN='quantile',
          probs=c(0.2,0.4,0.6,0.8),na.rm=TRUE)

# View final dataset
View(all_month_fit[,c("month","delayed_sum","delayed_sum_predict","error")])

# Plot result
provisions <- seq(4e+5,1e+6,1e4)
delayed_sum <- fit$coefficients[1] + fit$coefficients[2] * provisions
plot(all_month_fit$provisions,all_month_fit$delayed_sum,cex=0,
     xlab="provisions",ylab="Increase in due sum")
text(x=all_month_fit$provisions, y=all_month_fit$delayed_sum,
     labels=all_month_fit$month,cex=1,col=all_month_fit$year)
par(new=TRUE)
plot(provisions,delayed_sum  , col='blue',axes=F,xlab='',ylab='',lwd=2)




############# APPLY MODEL TO WHOLE DATASET

# Create future dataset
new_provisions <- c(870094,889299,907620,925275,937458,954299,985743,
                    1002985,994218,1014353,1037198,1050625,1059615,1073282,
                    1088952)
all_month_future <- all_month[,c("id","month","provisions","delayed_sum")]
new_df <- as.data.frame(
  cbind(rep(NA,length(new_provisions)),
c("2020-10","2020-11","2020-12","2021-01",
  "2021-02","2021-03","2021-04","2021-05","2021-06","2021-07","2021-08",
  "2021-09","2021-10","2021-11","2021-12"),new_provisions,
  rep(NA,length(new_provisions))))
names(new_df) <- c("id","month","provisions","delayed_sum")
all_month_future <- rbind(all_month_future,new_df)
all_month_future$provisions <- round(as.numeric(all_month_future$provisions),1)
all_month_future <- all_month_future[2:nrow(all_month_future),]
all_month_future$delated_sum <- round(as.numeric(all_month_future$delayed_sum,0))
all_month_future$id[max(all_month_future$id,na.rm = TRUE):nrow(
  all_month_future)] <- seq(max(all_month_future$id,na.rm = TRUE),nrow(
    all_month_future),1)

# Predict with linear regression
all_month_future$delayed_sum_predict <- fit$coefficients[1] + 
  fit$coefficients[2] * all_month_future$provisions
matplot(all_month_future$id, 
  cbind(all_month_future$delayed_sum,all_month_future$delayed_sum_predict),
  type="l",col=c("red","blue"),lty=c(1,1),lwd=c(3,3),
  ylab="counts",xlab="month id")
legend("topleft", inset=.02,legend =c("due sum","due sum predicted"), 
  lty=c(1,1),lwd=c(3,3),col=c("red","blue"))



############# GET TOTAL DUE SUM FOR NEXT YEAR
total_sum_2021 <- sum(all_month_future[
  substring(all_month_future$month,1,4)=="2021",]$delayed_sum_predict)

