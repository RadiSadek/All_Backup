
########################
### Initial settings ###
########################

# Libraries
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))


# Define main dir
main_dir <- "C:\\Projects\\Provisions_Modeled\\data\\income_expenses\\"

# Read Income_expense data from 2015
a1_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_012015.xlsx",sep=""),10,
                   colNames = FALSE)
a1_15$month <- "2015-01"
a2_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_022015.xlsx",sep=""),10,
                   colNames = FALSE)
a2_15$month <- "2015-02"
a3_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_032015.xlsx",sep=""),10,
                   colNames = FALSE)
a3_15$month <- "2015-03"
a4_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_042015.xlsx",sep=""),10,
                   colNames = FALSE)
a4_15$month <- "2015-04"
a5_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_052015.xlsx",sep=""),10,
                   colNames = FALSE)
a5_15$month <- "2015-05"
a6_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_062015.xlsx",sep=""),10,
                   colNames = FALSE)
a6_15$month <- "2015-06"
a7_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_072015.xlsx",sep=""),10,
                   colNames = FALSE)
a7_15$month <- "2015-07"
a8_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_082015.xlsx",sep=""),10,
                   colNames = FALSE)
a8_15$month <- "2015-08"
a9_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_092015.xlsx",sep=""),10,
                   colNames = FALSE)
a9_15$month <- "2015-09"
a10_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_102015.xlsx",sep=""),10,
                    colNames = FALSE)
a10_15$month <- "2015-10"
a11_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_112015.xlsx",sep=""),10,
                    colNames = FALSE)
a11_15$month <- "2015-11"
a12_15 <- read.xlsx(paste(main_dir,"2015\\Income_expense_122015.xlsx",sep=""),10,
                    colNames = FALSE)
a12_15$month <- "2015-12"


# Read Income_expense data from 2016
a1_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_012016.xlsx",sep=""),10,
                   colNames = FALSE)
a1_16$month <- "2016-01"
a2_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_022016.xlsx",sep=""),10,
                   colNames = FALSE)
a2_16$month <- "2016-02"
a3_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_032016.xlsx",sep=""),10,
                   colNames = FALSE)
a3_16$month <- "2016-03"
a4_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_042016.xlsx",sep=""),10,
                   colNames = FALSE)
a4_16$month <- "2016-04"
a5_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_052016.xlsx",sep=""),10,
                   colNames = FALSE)
a5_16$month <- "2016-05"
a6_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_062016.xlsx",sep=""),10,
                   colNames = FALSE)
a6_16$month <- "2016-06"
a7_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_072016.xlsx",sep=""),10,
                   colNames = FALSE)
a7_16$month <- "2016-07"
a8_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_082016.xlsx",sep=""),10,
                   colNames = FALSE)
a8_16$month <- "2016-08"
a9_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_092016.xlsx",sep=""),10,
                   colNames = FALSE)
a9_16$month <- "2016-09"
a10_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_102016.xlsx",sep=""),10,
                    colNames = FALSE)
a10_16$month <- "2016-10"
a11_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_112016.xlsx",sep=""),10,
                    colNames = FALSE)
a11_16$month <- "2016-11"
a12_16 <- read.xlsx(paste(main_dir,"2016\\Income_expense_122016.xlsx",sep=""),10,
                    colNames = FALSE)
a12_16$month <- "2016-12"

# Read Income_expense data from 2017
a1_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_012017.xlsx",sep=""),10,
                   colNames = FALSE)
a1_17$month <- "2017-01"
a2_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_022017.xlsx",sep=""),10,
                   colNames = FALSE)
a2_17$month <- "2017-02"
a3_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_032017.xlsx",sep=""),10,
                   colNames = FALSE)
a3_17$month <- "2017-03"
a4_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_042017.xlsx",sep=""),10,
                   colNames = FALSE)
a4_17$month <- "2017-04"
a5_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_052017.xlsx",sep=""),10,
                   colNames = FALSE)
a5_17$month <- "2017-05"
a6_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_062017.xlsx",sep=""),10,
                   colNames = FALSE)
a6_17$month <- "2017-06"
a7_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_072017.xlsx",sep=""),10,
                   colNames = FALSE)
a7_17$month <- "2017-07"
a8_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_082017.xlsx",sep=""),10,
                   colNames = FALSE)
a8_17$month <- "2017-08"
a9_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_092017.xlsx",sep=""),10,
                   colNames = FALSE)
a9_17$month <- "2017-09"
a10_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_102017.xlsx",sep=""),10,
                    colNames = FALSE)
a10_17$month <- "2017-10"
a11_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_112017.xlsx",sep=""),10,
                    colNames = FALSE)
a11_17$month <- "2017-11"
a12_17 <- read.xlsx(paste(main_dir,"2017\\Income_expense_122017.xlsx",sep=""),10,
                    colNames = FALSE)
a12_17$month <- "2017-12"


# Read Income_expense data from 2018
a1_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_012018.xlsx",sep=""),10,
                   colNames = FALSE)
a1_18$month <- "2018-01"
a2_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_022018.xlsx",sep=""),10,
                   colNames = FALSE)
a2_18$month <- "2018-02"
a3_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_032018.xlsx",sep=""),10,
                   colNames = FALSE)
a3_18$month <- "2018-03"
a4_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_042018.xlsx",sep=""),10,
                   colNames = FALSE)
a4_18$month <- "2018-04"
a5_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_052018.xlsx",sep=""),10,
                   colNames = FALSE)
a5_18$month <- "2018-05"
a6_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_062018.xlsx",sep=""),10,
                   colNames = FALSE)
a6_18$month <- "2018-06"
a7_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_072018.xlsx",sep=""),10,
                   colNames = FALSE)
a7_18$month <- "2018-07"
a8_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_082018.xlsx",sep=""),10,
                   colNames = FALSE)
a8_18$month <- "2018-08"
a9_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_092018.xlsx",sep=""),10,
                   colNames = FALSE)
a9_18$month <- "2018-09"
a10_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_102018.xlsx",sep=""),10,
                    colNames = FALSE)
a10_18$month <- "2018-10"
a11_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_112018.xlsx",sep=""),10,
                    colNames = FALSE)
a11_18$month <- "2018-11"
a12_18 <- read.xlsx(paste(main_dir,"2018\\Income_expense_122018.xlsx",sep=""),10,
                    colNames = FALSE)
a12_18$month <- "2018-12"


# Read Income_expense data from 2019
a1_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_012019.xlsx",sep=""),10,
                   colNames = FALSE)
a1_19$month <- "2019-01"
a2_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_022019.xlsx",sep=""),10,
                   colNames = FALSE)
a2_19$month <- "2019-02"
a3_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_032019.xlsx",sep=""),10,
                   colNames = FALSE)
a3_19$month <- "2019-03"
a4_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_042019.xlsx",sep=""),10,
                   colNames = FALSE)
a4_19$month <- "2019-04"
a5_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_052019.xlsx",sep=""),10,
                   colNames = FALSE)
a5_19$month <- "2019-05"
a6_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_062019.xlsx",sep=""),10,
                   colNames = FALSE)
a6_19$month <- "2019-06"
a7_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_072019.xlsx",sep=""),10,
                   colNames = FALSE)
a7_19$month <- "2019-07"
a8_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_082019.xlsx",sep=""),10,
                   colNames = FALSE)
a8_19$month <- "2019-08"
a9_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_092019.xlsx",sep=""),10,
                   colNames = FALSE)
a9_19$month <- "2019-09"
a10_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_102019.xlsx",sep=""),10,
                    colNames = FALSE)
a10_19$month <- "2019-10"
a11_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_112019.xlsx",sep=""),10,
                    colNames = FALSE)
a11_19$month <- "2019-11"
a12_19 <- read.xlsx(paste(main_dir,"2019\\Income_expense_122019.xlsx",sep=""),10,
                    colNames = FALSE)
a12_19$month <- "2019-12"



# Read Income_expense data from 2020
a1_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_012020.xlsx",sep=""),10,
                   colNames = FALSE)
a1_20 <- a1_20[2:nrow(a1_20),c(3,4)]
a1_20$month <- "2020-01"
names(a1_20) <- c("X1","X2","month")
a2_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_022020.xlsx",sep=""),10,
                   colNames = FALSE)
a2_20$month <- "2020-02"
a3_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_032020.xlsx",sep=""),10,
                   colNames = FALSE)
a3_20$month <- "2020-03"
a4_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_042020.xlsx",sep=""),10,
                   colNames = FALSE)
a4_20$month <- "2020-04"
a5_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_052020.xlsx",sep=""),10,
                   colNames = FALSE)
a5_20$month <- "2020-05"
a6_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_062020.xlsx",sep=""),10,
                   colNames = FALSE)
a6_20$month <- "2020-06"
a7_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_072020.xlsx",sep=""),10,
                   colNames = FALSE)
a7_20$month <- "2020-07"
a8_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_082020.xlsx",sep=""),10,
                   colNames = FALSE)
a8_20$month <- "2020-08"
a9_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_092020.xlsx",sep=""),10,
                   colNames = FALSE)
a9_20$month <- "2020-09"
a10_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_102020.xlsx",sep=""),10,
                   colNames = FALSE)
a10_20$month <- "2020-10"
a11_20 <- read.xlsx(paste(main_dir,"2020\\Income_expense_112020.xlsx",sep=""),10,
                   colNames = FALSE)
a11_20$month <- "2020-11"


final <- rbind(
  a1_15,a2_15,a3_15,a4_15,a5_15,a6_15,a7_15,a8_15,a9_15,a10_15,a11_15,a12_15,
  a1_16,a2_16,a3_16,a4_16,a5_16,a6_16,a7_16,a8_16,a9_16,a10_16,a11_16,a12_16,
  a1_17,a2_17,a3_17,a4_17,a5_17,a6_17,a7_17,a8_17,a9_17,a10_17,
  a3_18,a6_18,a8_18,a9_18,a10_18,a11_18,a12_18,
  a2_19,a3_19,a4_19,a5_19,a6_19,a7_19,a8_19,a9_19,a10_19,a11_19,a12_19,
  a1_20,a2_20,a3_20,a4_20,a5_20,a6_20,a7_20,a9_20,a10_20,a11_20)

# Output result
names(final) <- c("repay_amount","credit_number","month")
save(final,file = paste("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\",
"income_expenses.rdata",sep = ""))


