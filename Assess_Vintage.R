
########## GENERATE VINTAGE REPORT ############


# Choose end date of study (not included) for profit analysis
end_date <- "2020-09-01"


# Get libraries
library(openxlsx)
library(RMySQL)
library(lubridate)
library(zoo)
library(matrixStats)


# Database specifications
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306


# Read and make connection
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, 
                 host=db_host, port = df_port)
load("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\big_dataframe.rdata")
data <- df

# Read data
setwd("C:\\Regular\\R\\Vintage\\")
df <- read.xlsx("vintage_template.xlsx",3)

# Make month field
df$month <- ifelse(nchar(df$month)==1,paste("0",df$month,sep=""),df$month)
df$month_cor <- paste(df$`year(`,df$month,sep="-")

# subset
df <- subset(df,df$month_cor %in% c("2018-08","2018-09","2018-10",
   "2018-11","2018-12","2019-01","2019-02","2019-03","2019-04",
   "2019-05","2019-06","2019-07","2019-08"))

# Define functions for computing vintage and profit per band
gen_vint <- function(input,n,prev){
  r_agg <- as.data.frame(aggregate(input[,n],by=list(input$month_cor),
                                   function(x) sum(x, na.rm=TRUE)))
  m_agg <- as.data.frame(aggregate(input[,n-1],by=list(input$month_cor),
                                   function(x) sum(x, na.rm=TRUE)))
  v_agg <- merge(r_agg,m_agg,by.x = "Group.1",by.y = "Group.1",all.x = TRUE)
  v_agg$vintage <- round(1 - v_agg$x.x/v_agg$x.y,3)
  v_agg <- v_agg[,c("Group.1","vintage")]
  names(v_agg)[1] <- "month"
  v_agg <- v_agg[complete.cases(v_agg), ]
  if(!is.na(prev)){
    v_agg <- merge(prev,v_agg,by.x = "month",by.y = "month",all.x = TRUE)
  }
  return(v_agg)
}
gen_prof <- function(input,n,prev){
  r_agg <- as.data.frame(aggregate(input[,n],by=list(input$month_cor),
                                   function(x) sum(x, na.rm=TRUE)))
  p_agg <- as.data.frame(aggregate(input$Сума,by=list(input$month_cor),
                                   function(x) sum(x, na.rm=TRUE)))
  v_agg <- merge(r_agg,p_agg,by.x = "Group.1",by.y = "Group.1",all.x = TRUE)
  v_agg$vintage <- ifelse(v_agg$x.x>0,round(v_agg$x.x/v_agg$x.y,3),NaN)
  v_agg <- v_agg[,c("Group.1","vintage")]
  names(v_agg)[1] <- "month"
  v_agg <- v_agg[complete.cases(v_agg), ]
  if(!is.na(prev)){
    v_agg <- merge(prev,v_agg,by.x = "month",by.y = "month",all.x = TRUE)
  }
  return(v_agg)
}

# Generate table functions 
gen_table_prof <- function(input){
  result <- gen_prof(input,41,
            gen_prof(input,39,
            gen_prof(input,37, 
            gen_prof(input,35,
            gen_prof(input,33,
            gen_prof(input,31,
            gen_prof(input,29,
            gen_prof(input,27,NA))))))))
  names(result) <- c("month","pr30","pr60","pr90","pr120",
                     "pr180","pr210","pr360","prm360")
  return(result)
}
gen_table_vint <- function(input){
  result <- gen_vint(input,41,
            gen_vint(input,39,
            gen_vint(input,37, 
            gen_vint(input,35,
            gen_vint(input,33,
            gen_vint(input,31,
            gen_vint(input,29,
            gen_vint(input,27,NA))))))))
  names(result) <- c("month","v30","v60","v90","v120",
                     "v180","v210","v360","vm360")
  return(result)
}

# Get stats on profit
get_stats_prof <- function(input){
  input[is.na(input)] <- 0
  input$breakeven <- NA
  for(i in 1:nrow(input)){
    for(j in 2:(ncol(input)-1)){
      if(input[i,j]>1){
        input$breakeven[i] <- j
        break
      } else {
        input$breakeven[i] <- NA}}
  }
  input$breakeven <- ifelse(is.na(input$breakeven),NA,
                            names(input)[input$breakeven])
  input$breakeven <- gsub("pr", "", input$breakeven)
  input$max_pr <- ifelse(is.na(input$pr360),NA,input$pr360)
  return(input[,c("month","breakeven","max_pr")])
}


# Create final table
gen_fin_table <- function(input,crit){
  d1 <- get_stats_prof(gen_table_prof(input))
  d1$breakeven <- as.numeric(d1$breakeven)
  d1$max_pr <- ifelse(d1$max_pr==0,NA,d1$max_pr)
  d2 <- gen_table_vint(input)[,c(1,3)]
  final <- merge(d2,d1,by.x = "month", by.y = "month",all.x = TRUE)
  stats <- cbind(rbind(max(final$v60,na.rm = TRUE),
                       min(final$v60,na.rm = TRUE),
                       round(mean(final$v60,na.rm = TRUE),3),
                       sd(final$v60,na.rm=TRUE)),
                 rbind(max(final$breakeven,na.rm = TRUE),
                       min(final$breakeven,na.rm = TRUE),
                       median(final$breakeven,na.rm = TRUE),
                       sd(final$breakeven,na.rm=TRUE)),
                 rbind(max(final$max_pr,na.rm = TRUE),
                       min(final$max_pr,na.rm = TRUE),
                       mean(final$max_pr,na.rm = TRUE),
                       sd(final$max_pr,na.rm=TRUE)))
  row.names(stats) <- c("max","min","mean","sd")
  colnames(stats) <- c("v60","breakeven","max_pr")
  return(stats)
}


# Create list for different products
df1 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
          df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City We",]
df2 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
          df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City Mo",]
df3 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
          df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="Пенсион",]
df4 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
          df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City 2-",]
df5 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
          df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City We",]
df6 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
          df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City Mo",]
df7 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
          df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="Пенсион",]
df8 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
          df$Зона!="CrediRect" & substring(df$Тип.продукт,1,7)=="City 2-",]
df9 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
          df$Зона=="CrediRect" & substring(df$Тип.продукт,1,15)=="CreDirect Потре",]
df10 <- df[df$Пореден.Марка=="Не" & df$Пореден.Компания=="Не" &
          df$Зона=="CrediRect" & substring(df$Тип.продукт,1,15)!="CreDirect Потре",]
df13 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
          df$Зона=="CrediRect" & substring(df$Тип.продукт,1,15)=="CreDirect Потре",]
df14 <- df[df$Пореден.Марка=="Да" & df$Пореден.Компания=="Да" &
          df$Зона=="CrediRect" & substring(df$Тип.продукт,1,15)!="CreDirect Потре",]

# Create final table for all products
all_table <- suppressWarnings(rbind(gen_fin_table(df1,3),
                       gen_fin_table(df2,3),
                       gen_fin_table(df3,3),
                       gen_fin_table(df4,3),
                       gen_fin_table(df5,3),
                       gen_fin_table(df6,3),
                       gen_fin_table(df7,3),
                       gen_fin_table(df8,3),
                       gen_fin_table(df9,3),
                       gen_fin_table(df10,3),
                       gen_fin_table(df13,3),
                       gen_fin_table(df14,3)))
row.names(all_table) <- c("new_CityWeek","new_CityMonth",
                          "new_Pensioner","new_City2-Week",
                          "rep_CityWeek","rep_CityMonth",
                          "rep_Pensioner","rep_City2-Week",
                          "new_Installments","new_PayDay",
                          "rep_Installments","rep_PayDay")

# Make correlation between vintage and profit
gen_max_cor <- function(input){
  a <- merge(gen_table_vint(input),
             gen_table_prof(input)[,c("month","pr360")],
             by.x = "month",by.y = "month",all.x = TRUE)
  max_cor <- max(
      abs(cor(a[1:20,2],a[1:20,10])),abs(cor(a[1:20,3],a[1:20,10])),
      abs(cor(a[1:20,4],a[1:20,10])),abs(cor(a[1:20,5],a[1:20,10])),
      abs(cor(a[1:20,6],a[1:20,10])),abs(cor(a[1:20,7],a[1:20,10])),
      abs(cor(a[1:20,8],a[1:20,10])),abs(cor(a[1:20,9],a[1:20,10])))
  return(max_cor)
}

gen_cor <- function(input){
  a <- merge(gen_table_vint(input),
             gen_table_prof(input)[,c("month","pr360")],
             by.x = "month",by.y = "month",all.x = TRUE)
  tab <- rbind(abs(cor(a[1:20,2],a[1:20,10])),abs(cor(a[1:20,3],a[1:20,10])),
               abs(cor(a[1:20,4],a[1:20,10])),abs(cor(a[1:20,5],a[1:20,10])),
               abs(cor(a[1:20,6],a[1:20,10])),abs(cor(a[1:20,7],a[1:20,10])),
               abs(cor(a[1:20,8],a[1:20,10])),abs(cor(a[1:20,9],a[1:20,10])))
  return(tab)
}

all_table2 <- rbind(suppressWarnings(gen_max_cor(df1)),
                                    suppressWarnings(gen_max_cor(df2)),
                                    suppressWarnings(gen_max_cor(df3)),
                                    suppressWarnings(gen_max_cor(df4)),
                                    suppressWarnings(gen_max_cor(df5)),
                                    suppressWarnings(gen_max_cor(df6)),
                                    suppressWarnings(gen_max_cor(df7)),
                                    suppressWarnings(gen_max_cor(df8)),
                                    suppressWarnings(gen_max_cor(df9)),
                                    suppressWarnings(gen_max_cor(df10)),
                                    suppressWarnings(gen_max_cor(df13)),
                                    suppressWarnings(gen_max_cor(df14)))
row.names(all_table2) <- c("new_CityWeek","new_CityMonth",
                          "new_Pensioner","new_City2-Week",
                          "rep_CityWeek","rep_CityMonth",
                          "rep_Pensioner","rep_City2-Week",
                          "new_Installments","new_PayDay",
                          "rep_Installments","rep_PayDay")

all_table3 <- cbind(gen_cor(df1),gen_cor(df2),gen_cor(df3),gen_cor(df4),
                    gen_cor(df5),gen_cor(df6),gen_cor(df7),gen_cor(df8),
                    gen_cor(df9),gen_cor(df10),gen_cor(df13),gen_cor(df14))
row.names(all_table3) <- c("v30","v60","v90","v120",
                           "v180","v210","v360","vm360")
colnames(all_table3) <- c("new_CityWeek","new_CityMonth",
                          "new_Pensioner","new_City2-Week",
                          "rep_CityWeek","rep_CityMonth",
                          "rep_Pensioner","rep_City2-Week",
                          "new_Installments","new_PayDay",
                          "rep_Installments","rep_PayDay")


