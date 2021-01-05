
# Call libraries 
library(dplyr)
library(RMySQL)
library(openxlsx)
library(basicTrendline)
library(mltools)
library(ggplot2)
library(rworldmap)
library(dplyr)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Read entire dataframe
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")

# Read vintage data
vint <- read.xlsx(
 "C:\\Projects\\Ad_Hoc\\Scoring_Office_Assessment\\data\\vintage-08.2020_new.xlsx",
 3)
vint_bu <- vint

# Make date fields
vint$date <- as.Date(paste(substring(vint$Дата.на.отпускане,7,10),
                     substring(vint$Дата.на.отпускане,4,5),
                     substring(vint$Дата.на.отпускане,1,2),sep="-"))
vint$month <- substring(vint$date,1,7)

# Choose correct date 
vint <- subset(vint, vint$month<="2020-06" & vint$month>="2019-06")
vint <- subset(vint, !(vint$Офис %in% c("CreDirect","CreDirect-WebBroker",
    "CreDirect-Creditour")))

# Compute non-collected during 60 days
ragg <- as.data.frame(aggregate(vint$r60,by=list(vint$month),FUN=sum))
names(ragg) <- c("month","r60")
magg <- as.data.frame(aggregate(vint$m60,by=list(vint$month),FUN=sum))
names(magg) <- c("month","m60")
agg <- merge(ragg, magg,by.x = "month",by.y = "month",all.x = TRUE)
agg$noncollected <- round(1 - agg$r60/agg$m60,2)

# Subset according to number of credits per office
nb_office <- as.data.frame(table(vint$Офис))
nb_office <- subset(nb_office,nb_office$Freq>=100)
vint_h <- subset(vint,vint$Офис %in% nb_office$Var1)

# Put zeros instead of NA
vint_h[,c(26:41)] <- sapply(vint_h[,c(26:41)],function(x) ifelse(is.na(x),0,x))

# Create function for aggregate per office
gen_vint_office <- function(var){
  ragg_var <- as.data.frame(aggregate(var$r60,by=list(var$month),FUN=sum))
  names(ragg_var) <- c("month","r60")
  magg_var <- as.data.frame(aggregate(var$m60,by=list(var$month),FUN=sum))
  names(magg_var) <- c("month","m60")
  agg_var <- merge(ragg_var, magg_var,by.x = "month",by.y = "month",
                   all.x = TRUE)
  agg_var$noncollected <- round(1 - agg_var$r60/agg_var$m60,2)
  return(agg_var)
}
View(gen_vint_office(vint_h[vint_h$Офис=="Севлиево",]))

# Iterate for all offices
stats <- data.frame(matrix(ncol = 5, nrow = length(table(vint_h$Офис))))
names(stats) <- c("office","mean","std","q20","q80")
stats$office <- names(table(vint_h$Офис))
for(i in 1:nrow(stats)){
  stats$mean[i] <- round(mean(gen_vint_office(
    vint_h[vint_h$Офис==stats$office[i],])$noncollected),2)
  stats$std[i] <- round(sd(gen_vint_office(
    vint_h[vint_h$Офис==stats$office[i],])$noncollected),2)
  stats$q20[i] <- round(quantile(gen_vint_office(
    vint_h[vint_h$Офис==stats$office[i],])$noncollected, 0.2),2)
  stats$q80[i] <- round(quantile(gen_vint_office(
    vint_h[vint_h$Офис==stats$office[i],])$noncollected, 0.8),2)
  stats$max[i] <- round(max(gen_vint_office(
    vint_h[vint_h$Офис==stats$office[i],])$noncollected),2)
  stats$min[i] <- round(min(gen_vint_office(
    vint_h[vint_h$Офис==stats$office[i],])$noncollected),2)
}
stats$zvalue <- abs(round(stats$mean / stats$std,2))
stats <- merge(stats, nb_office, by.x = "office", by.y = "Var1", all.x = TRUE)
stats$minmax <- stats$max - stats$min

# Plot quantile 80% and 20% and mean
plot(stats$mean,stats$q20)
plot(stats$mean,stats$q80)
plot(stats$mean,stats$max)

# Check evolution of std of office vintage according to office counts 
suppressWarnings(trendline(stats$mean, stats$q80, model = "line2P", 
  plot = TRUE, linecolor = "red", lty = 1, lwd = 1))
suppressWarnings(trendline(stats$mean, stats$q20, model = "line2P", 
  plot = TRUE, linecolor = "red", lty = 1, lwd = 1))

# Count months
gen_count_months <- function(var){
  return(nrow(var))
}
for(i in 1:nrow(stats)){
  stats$count_months[i] <- gen_count_months(
  (gen_vint_office(vint_h[vint_h$Офис==stats$office[i],])))

}

# Make final dataframe with all offices
stats_all <- data.frame(matrix(ncol = 1, nrow = length(table(vint$Офис))))
names(stats_all) <- c("office")
stats_all$office <- names(table(vint$Офис))
stats_all <- merge(stats_all,stats[,c("office","mean","Freq","count_months")], 
                   by.x = "office", by.y = "office", all.x = TRUE)
stats_all$category <- ifelse(is.na(stats_all$count_months),"less_100_credits",
      ifelse(stats_all$count_months<5,"not_enough_months","OK"))
table(stats_all$category)

# Bin OK offices
stats_ok <- subset(stats_all,stats_all$category=="OK")
hist(stats_ok$mean)
stats_ok$band_nb <- as.numeric(cut(stats_ok$mean, 3))
stats_ok$band_nb <- bin_data(stats_ok$mean, bins=3, binType = "quantile")

# Make final dataframe
stats_final <- merge(stats_all[,c("office","category","mean")],
                     stats_ok[,c("office","band_nb")], 
                     by.x = "office", by.y = "office", all.x = TRUE)
stats_final$band_nb <- ifelse(is.na(stats_final$band_nb),0,stats_final$band_nb)

# Read data about offices
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)
office <- suppressWarnings(fetch(dbSendQuery(con, "
SELECT name, latitude, longitude, self_approve, id
FROM citycash_db.structure_offices"), n=-1))
Encoding(office$name) <- "UTF-8"

# Rejoin to main
stats_final <- merge(stats_final, office, by.x = "office", by.y = "name",
                     all.x = TRUE)

# Resubset to only interesting vintage offices
stats_select <- subset(stats_final, stats_final$band_nb %in% c(1,2,3))
stats_select <- subset(stats_select, stats_select$office!="Ипотеки")
stats_select <- subset(stats_select, stats_select$office!="Финстарт")

stats_select$latitude[stats_select$office=="Правец"] <- 42.898709
stats_select$longitude[stats_select$office=="Правец"] <- 23.914554

stats_select$latitude[stats_select$office=="Стамболийски"] <- 42.135665
stats_select$longitude[stats_select$office=="Стамболийски"] <- 24.537114

# Draw map of Bulgaria and put office vintage results
map <- read.xlsx(
  "C:\\Projects\\Ad_Hoc\\Scoring_Office_Assessment\\data\\BG_Map.xlsx")
rbPal <- colorRampPalette(c('blue','yellow','red'))
stats_select$color <- rbPal(10)[as.numeric(cut(stats_select$band_nb,
      breaks = 10))]
plot(stats_select$longitude,stats_select$latitude, 
     xlim=c(22.4,28.5),
     ylim=c(41.25,44.25), cex=4, 
     col = stats_select$color, pch = 19, axes=FALSE, frame.plot=FALSE,
     xlab = NA, ylab = NA)
legend(locator(1), c("Good Office","Medium Office","Bad Office"),
        col=c('blue','yellow','red'), pch = 19, cex = 1, ncol=3, bty = "n",
        x.intersp=0.5, text.width=1.1)
par(new=TRUE)
lines(map$longitude,map$latitude, col = "black", lwd = 3)

# Make final adjustments to final dataframe
stats_final$office_category <- 
  ifelse(stats_final$band_nb==0,"No_Info",
  ifelse(stats_final$band_nb==1,"Good Office",
  ifelse(stats_final$band_nb==2,"Medium Office",
  ifelse(stats_final$band_nb==3,"Bad Office","Other"))))
stats_final$self_approve_category <- 
  ifelse(stats_final$self_approve==0,"Normal","Self approval")

# Make output dataframe
stats_final <- subset(stats_final,!is.na(stats_final$self_approve))
write.xlsx(stats_final[,c("office","office_category","id", "band_nb",
                          "mean","self_approve_category")],
"C:\\Projects\\Ad_Hoc\\Scoring_Office_Assessment\\results\\results_for_scoring.xlsx")


