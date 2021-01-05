
########################
### Initial settings ###
########################

# Libraries
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))

# Read file
df <- read.xlsx(
  paste("C:\\Projects\\Score_New_Cases\\Terminated\\results\\",
        "Pensioner_and_More_60years_6months\\",
        "Pensioner_and_More_60years_6months_v2.xlsx",sep=""),1)


set.seed(7)
ss <- sample(1:2,size=nrow(df),replace=TRUE,prob=c(0.8,0.2))
df$group <- ss
df$group <- ifelse(df$group==1,"group_13","group_11")


# Output file
write.xlsx(df,
           paste("C:\\Projects\\Score_New_Cases\\Terminated\\results\\",
                 "Pensioner_and_More_60years_6months\\",
                 "Pensioner_and_More_60years_6months_v2_FINAL.xlsx",sep=""))
