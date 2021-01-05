
# Libraries
library(openxlsx)
library(RMySQL)

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306

# Define renaming function
rename_fct <- function(var){
  names(var) <- c("credit_number","date_activation","deactivated_at","zone",
   "office","office_initial","judicial ","date_judicial","product", "status",
   "amount","lihva","neustoika","taksa","collected","paid_taksa",
   "paid_neustoika","paid_lihva","paid_principal",
   "saldo_beginning","saldo_end","taksa_beginning","neustoika_beginning",
   "lihva_beginning","principal_beginning","total_receivables_beginning",
   "taksa_end","neustoika_end","lihva_end","principal_end",
   "total_receivables_end","days_delay","balance_matured_principal",
   "amount_delayed","total_sum_left","collateral_value","accounting_removed")
  return(var)
}

# Define main dir
main_dir <- "C:\\Projects\\Provisions_Modeled\\data\\accounting\\"


# Read accounting data from 2016
a1_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_012016.xlsx",sep=""))
a1_16 <- rename_fct(a1_16)
a1_16$month <- "2016-01"
a2_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_022016.xlsx",sep=""))
a2_16 <- rename_fct(a2_16)
a2_16$month <- "2016-02"
a3_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_032016.xlsx",sep=""))
a3_16 <- rename_fct(a3_16)
a3_16$month <- "2016-03"
a4_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_042016.xlsx",sep=""))
a4_16 <- rename_fct(a4_16)
a4_16$month <- "2016-04"
a5_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_052016.xlsx",sep=""))
a5_16 <- rename_fct(a5_16)
a5_16$month <- "2016-05"
a6_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_062016.xlsx",sep=""))
a6_16 <- rename_fct(a6_16)
a6_16$month <- "2016-06"
a7_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_072016.xlsx",sep=""))
a7_16 <- rename_fct(a7_16)
a7_16$month <- "2016-07"
a8_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_082016.xlsx",sep=""))
a8_16 <- rename_fct(a8_16)
a8_16$month <- "2016-08"
a9_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_092016.xlsx",sep=""))
a9_16 <- rename_fct(a9_16)
a9_16$month <- "2016-09"
a10_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_102016.xlsx",sep=""))
a10_16 <- rename_fct(a10_16)
a10_16$month <- "2016-10"
a11_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_112016.xlsx",sep=""))
a11_16 <- rename_fct(a11_16)
a11_16$month <- "2016-11"
a12_16 <- read.xlsx(paste(main_dir,"2016\\Accounting_122016.xlsx",sep=""))
a12_16 <- rename_fct(a12_16)
a12_16$month <- "2016-12"

# Read accounting data from 2017
a1_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_012017.xlsx",sep=""))
a1_17 <- rename_fct(a1_17)
a1_17$month <- "2017-01"
a2_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_022017.xlsx",sep=""))
a2_17 <- rename_fct(a2_17)
a2_17$month <- "2017-02"
a3_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_032017.xlsx",sep=""))
a3_17 <- rename_fct(a3_17)
a3_17$month <- "2017-03"
a4_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_042017.xlsx",sep=""))
a4_17 <- rename_fct(a4_17)
a4_17$month <- "2017-04"
a5_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_052017.xlsx",sep=""))
a5_17 <- rename_fct(a5_17)
a5_17$month <- "2017-05"
a6_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_062017.xlsx",sep=""))
a6_17 <- rename_fct(a6_17)
a6_17$month <- "2017-06"
a7_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_072017.xlsx",sep=""))
a7_17 <- rename_fct(a7_17)
a7_17$month <- "2017-07"
a8_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_082017.xlsx",sep=""))
a8_17 <- rename_fct(a8_17)
a8_17$month <- "2017-08"
a9_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_092017.xlsx",sep=""))
a9_17 <- rename_fct(a9_17)
a9_17$month <- "2017-09"
a10_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_102017.xlsx",sep=""))
a10_17 <- rename_fct(a10_17)
a10_17$month <- "2017-10"
a11_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_112017.xlsx",sep=""))
a11_17 <- rename_fct(a11_17)
a11_17$month <- "2017-11"
a12_17 <- read.xlsx(paste(main_dir,"2017\\Accounting_122017.xlsx",sep=""))
a12_17 <- rename_fct(a12_17)
a12_17$month <- "2017-12"


# Read accounting data from 2018
a1_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_012018.xlsx",sep=""))
a1_18 <- rename_fct(a1_18)
a1_18$month <- "2018-01"
a2_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_022018.xlsx",sep=""))
a2_18 <- rename_fct(a2_18)
a2_18$month <- "2018-02"
a3_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_032018.xlsx",sep=""))
a3_18 <- rename_fct(a3_18)
a3_18$month <- "2018-03"
a4_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_042018.xlsx",sep=""))
a4_18 <- rename_fct(a4_18)
a4_18$month <- "2018-04"
a5_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_052018.xlsx",sep=""))
a5_18 <- rename_fct(a5_18)
a5_18$month <- "2018-05"
a6_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_062018.xlsx",sep=""))
a6_18 <- rename_fct(a6_18)
a6_18$month <- "2018-06"
a7_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_072018.xlsx",sep=""))
a7_18 <- rename_fct(a7_18)
a7_18$month <- "2018-07"
a8_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_082018.xlsx",sep=""))
a8_18 <- rename_fct(a8_18)
a8_18$month <- "2018-08"
a9_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_092018.xlsx",sep=""))
a9_18 <- rename_fct(a9_18)
a9_18$month <- "2018-09"
a10_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_102018.xlsx",sep=""))
a10_18 <- rename_fct(a10_18)
a10_18$month <- "2018-10"
a11_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_112018.xlsx",sep=""))
a11_18 <- rename_fct(a11_18)
a11_18$month <- "2018-11"
a12_18 <- read.xlsx(paste(main_dir,"2018\\Accounting_122018.xlsx",sep=""))
a12_18 <- rename_fct(a12_18)
a12_18$month <- "2018-12"


# Read accounting data from 2019
a1_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_012019.xlsx",sep=""))
a1_19 <- rename_fct(a1_19)
a1_19$month <- "2019-01"
a2_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_022019.xlsx",sep=""))
a2_19 <- rename_fct(a2_19)
a2_19$month <- "2019-02"
a3_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_032019.xlsx",sep=""))
a3_19 <- rename_fct(a3_19)
a3_19$month <- "2019-03"
a4_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_042019.xlsx",sep=""))
a4_19 <- rename_fct(a4_19)
a4_19$month <- "2019-04"
a5_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_052019.xlsx",sep=""))
a5_19 <- rename_fct(a5_19)
a5_19$month <- "2019-05"
a6_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_062019.xlsx",sep=""))
a6_19 <- rename_fct(a6_19)
a6_19$month <- "2019-06"
a7_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_072019.xlsx",sep=""))
a7_19 <- rename_fct(a7_19)
a7_19$month <- "2019-07"
a8_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_082019.xlsx",sep=""))
a8_19 <- rename_fct(a8_19)
a8_19$month <- "2019-08"
a9_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_092019.xlsx",sep=""))
a9_19 <- rename_fct(a9_19)
a9_19$month <- "2019-09"
a10_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_102019.xlsx",sep=""))
a10_19 <- rename_fct(a10_19)
a10_19$month <- "2019-10"
a11_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_112019.xlsx",sep=""))
a11_19 <- rename_fct(a11_19)
a11_19$month <- "2019-11"
a12_19 <- read.xlsx(paste(main_dir,"2019\\Accounting_122019.xlsx",sep=""))
a12_19 <- rename_fct(a12_19)
a12_19$month <- "2019-12"



# Read accounting data from 2020
a1_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_012020.xlsx",sep=""))
a1_20 <- rename_fct(a1_20)
a1_20$month <- "2020-01"
a2_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_022020.xlsx",sep=""))
a2_20 <- rename_fct(a2_20)
a2_20$month <- "2020-02"
a3_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_032020.xlsx",sep=""))
a3_20 <- rename_fct(a3_20)
a3_20$month <- "2020-03"
a4_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_042020.xlsx",sep=""))
a4_20 <- rename_fct(a4_20)
a4_20$month <- "2020-04"
a5_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_052020.xlsx",sep=""))
a5_20 <- rename_fct(a5_20)
a5_20$month <- "2020-05"
a6_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_062020.xlsx",sep=""))
a6_20 <- rename_fct(a6_20)
a6_20$month <- "2020-06"
a7_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_072020.xlsx",sep=""))
a7_20 <- rename_fct(a7_20)
a7_20$month <- "2020-07"
a8_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_082020.xlsx",sep=""))
a8_20 <- rename_fct(a8_20)
a8_20$month <- "2020-08"
a9_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_092020.xlsx",sep=""))
a9_20 <- rename_fct(a9_20)
a9_20$month <- "2020-09"
a10_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_102020.xlsx",sep=""))
a10_20 <- rename_fct(a10_20)
a10_20$month <- "2020-10"
a11_20 <- read.xlsx(paste(main_dir,"2020\\Accounting_112020.xlsx",sep=""))
a11_20 <- rename_fct(a11_20)
a11_20$month <- "2020-11"

# Make final dataframe
final <- rbind(
  a1_16,a2_16,a3_16,a4_16,a5_16,a6_16,a7_16,a8_16,a9_16,a10_16,a11_16,a12_16,
  a1_17,a2_17,a3_17,a4_17,a5_17,a6_17,a7_17,a8_17,a9_17,a10_17,a11_17,a12_17,
  a1_18,a2_18,a3_18,a4_18,a5_18,a6_18,a7_18,a8_18,a9_18,a10_18,a11_18,a12_18,
  a1_19,a2_19,a3_19,a4_19,a5_19,a6_19,a7_19,a8_19,a9_19,a10_19,a11_19,a12_19,
  a1_20,a2_20,a3_20,a4_20,a5_20,a6_20,a7_20,a8_20,a9_20,a10_20,a11_20)

# Output data
save(final,file = paste("C:\\Users\\rsadek.CC\\Desktop\\Generic\\output\\",
                        "accounting.rdata",sep = ""))


