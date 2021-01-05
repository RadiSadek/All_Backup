
#######################################################
############# MONITOR SCORECARD EFFICIENcY  ###########
#######################################################


############ Read and set data ##########

# Call libraries 
library(RMySQL)
library(openxlsx)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Subset useful
df <- read.xlsx("C:\\Projects\\Apply_Scoring\\vintage_input.xlsx",3)
df$month_cor <- ifelse(nchar(df$month)==1,paste("0",df$month,sep=""),df$month)
df$month2 <- paste(df$`year(`,df$month_cor,sep="-")
df$company <- ifelse(substring(df$Тип.продукт,1,9)=="CreDirect",
                     "Credirect","City Cash")



############ Choose time window  ##########

# Choose time window for analysis
df <- subset(df,df$month2>="2019-07")



############ Create functions  ##########

gen_v60 <- function(input,score){
  
  if(score!="all"){
    input <- input[input$Скоринг==score,]
  }
  if(nrow(input)>0){
    agg <- as.data.frame(merge(
      aggregate(input$r60,by=list(input$month2),FUN=sum,na.rm=TRUE),
      aggregate(input$m60,by=list(input$month2),FUN=sum,na.rm=TRUE),
      by.x = "Group.1",by.y = "Group.1",all.x = TRUE))
    names(agg) <- c("month","r60","m60")
    agg <- subset(agg,agg$m60>0)
    result <- round(1-sum(agg$r60,na.rm = TRUE)/sum(agg$m60,na.rm = TRUE),3)
  } else {
    result <- NA
  }
  return(result)
}

gen_v60_per_score <- function(input){
  result <- cbind(gen_v60(input,"Good 4"),gen_v60(input,"Good 3"),
               gen_v60(input,"Good 2"),gen_v60(input,"Good 1"),
               gen_v60(input,"Indeterminate"))
  result <- round((result - mean(result,na.rm = TRUE))/
                    mean(result,na.rm = TRUE)*100,2)
  return(result)
}

gen_plot <- function(input){
  df1 <- input[input$Пореден.Компания=="Не" & input$company=="City Cash",]
  df2 <- input[input$Пореден.Компания=="Да" & input$Пореден.Марка=="Да" & 
                 input$company=="City Cash",]
  df3 <- input[input$Пореден.Компания=="Да" & input$Пореден.Марка=="Не" & 
                 input$company=="City Cash",]
  
  df4 <- input[input$Пореден.Компания=="Не" & input$company=="Credirect",]
  df5 <- input[input$Пореден.Компания=="Да" & input$Пореден.Марка=="Да" & 
              input$company=="Credirect",]
  df6 <- input[input$Пореден.Компания=="Да" & input$Пореден.Марка=="Не" & 
              input$company=="Credirect",]
  
  final <- as.data.frame(rbind(gen_v60_per_score(df1),
                               gen_v60_per_score(df2),
                               gen_v60_per_score(df3),
                               gen_v60_per_score(df4),
                               gen_v60_per_score(df5),
                               gen_v60_per_score(df6)))
  names(final) <- c("Good4","Good3","Good2","Good1","Indeterminate")
  row.names(final) <- c("App CityCash","Rep CityCash",
        "App CityCash old Credirect","App Credirect","Rep Credirect",
        "Rep Credirect old CityCash")
  return(final)
}



############ Create final plots by using functions  ##########

# Plot all-time vintage scorecard efficency 
res <- gen_plot(df)
par(mfrow=c(1,2))
plot(c(1:5), res[1,],type="l",col=c("red"),lty=c(1),lwd=c(3),
     ylim = c(-130,130), xlab = "Scoring Level",
     ylab = "(v60 - mean(all v60)) / mean(all v60)")
lines(c(1:5), res[2,],type="l",col=c("red"),lty=c(2),lwd=c(3))
lines(c(1:5), res[4,],type="l",col=c("blue"),lty=c(1),lwd=c(3))
lines(c(1:5), res[5,],type="l",col=c("blue"),lty=c(2),lwd=c(3))
legend(x= "topleft", legend=c("app City Cash","rep City Cash","app Credirect",
       "rep Credirect"),
       col=c("red","red","blue","blue"), lty=c(1,2,1,2), lwd=c(3,3,3,3))


# Plot vintage scorecard efficency of last 6 months
df_sub <- df[df$month2 %in% names(table(df$month2)[
 (length(table(df$month2))-3):(length(table(df$month2))-1)]),]
table(df_sub$month2)
res2 <- gen_plot(df_sub)
plot(c(1:5), res2[1,],type="l",col=c("red"),lty=c(1),lwd=c(3),
     ylim = c(-130,130), xlab = "Scoring Level",
     ylab = "(v60 - mean(all v60)) / mean(all v60)")
lines(c(1:5), res2[2,],type="l",col=c("red"),lty=c(2),lwd=c(3))
lines(c(1:5), res2[4,],type="l",col=c("blue"),lty=c(1),lwd=c(3))
lines(c(1:5), res2[5,],type="l",col=c("blue"),lty=c(2),lwd=c(3))
legend(x= "topleft", legend=c("app City Cash","rep City Cash","app Credirect",
       "rep Credirect"),
       col=c("red","red","blue","blue"), lty=c(1,2,1,2), lwd=c(3,3,3,3))




######### End  ##########


