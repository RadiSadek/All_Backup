

############ COMPUTE PROFITS AND EXPENSES FOR MARTIN #############


######## Libraries and functions #########
library(openxlsx)


########## Read data ###########
# Main dataframe
setwd("C:\\Misc\\Martin")
now <- read.xlsx("Accounting_Now.xlsx")
before <- read.xlsx("Accounting_Before.xlsx")


########## Work on data ###########
# Filter non used columns
now <- now [ , -which(names(now) %in% 
    c("����.���.���������","����������.�������"))]
before <- before[ , -which(names(before) %in% 
    c("����.���.���������","����������.�������"))]

# Calculate provisions
now$����� <- ifelse(now[,31]>180,4,
             ifelse(now[,31]>90,3,
             ifelse(now[,31]>30,2,1)))
now$��������� <- ifelse(now[,36]==1,now[,30]*0,
                 ifelse(now[,36]==2,now[,30]*0.1,
                 ifelse(now[,36]==3,now[,30]*0.5,now[,30]*1)))
now$��������� <- ifelse(is.na(now[,35]), now$���������,
                 ifelse(now[,35]>now[,30],0,
                 ifelse(now[,36]==1,(now[,30]-now[,35])*0,
                 ifelse(now[,36]==2,(now[,30]-now[,35])*0.1,
                 ifelse(now[,36]==3,(now[,30]-now[,35])*0.5,
                        (now[,30]-now[,35])*1)))))

before$����� <- ifelse(before[,31]>180,4,
                ifelse(before[,31]>90,3,
                ifelse(before[,31]>30,2,1)))
before$��������� <- ifelse(before[,36]==1,before[,30]*0,
                    ifelse(before[,36]==2,before[,30]*0.1,
                    ifelse(before[,36]==3,before[,30]*0.5,before[,30]*1)))
before$��������� <- ifelse(is.na(before[,35]), before$���������,
                    ifelse(before[,35]>before[,30],0,
                    ifelse(before[,36]==1,(before[,30]-before[,35])*0,
                    ifelse(before[,36]==2,(before[,30]-before[,35])*0.1,
                    ifelse(before[,36]==3,(before[,30]-before[,35])*0.5,
                           (before[,30]-before[,35])*1)))))

# Filter credirect
now_credirect <- subset(now, substring(now$���.����,0,5)=="CreDi")
before_credirect <- subset(before, substring(before$���.����,0,5)=="CreDi")

# Filter FinStart
now_fin <- subset(now, substring(now$���.����,0,5)=="�����"  | 
                       now$���.����=="�������� ��������")
before_fin <- subset(before, substring(before$���.����,0,5)=="�����" | 
                             before$���.����=="�������� ��������")

# Calculate results  
results <- matrix(NA, nrow = 4, ncol = 4)
results[1,1] <- sum(now$���������.�����.��.������ , 
                    na.rm = TRUE)
results[1,2] <- sum(now$���������.���������.��.������������.��.������ , 
                    na.rm = TRUE)
results[1,3] <- sum(now$���������.�������.��.����.��.������ , 
                    na.rm = TRUE)
results[1,4] <- sum(now$��������� , na.rm = TRUE) - sum(before$��������� , 
                                                        na.rm = TRUE)

results[2,1] <- sum(now_credirect$���������.�����.��.������ , 
                    na.rm = TRUE)
results[2,2] <- sum(now_credirect$���������.���������.��.������������.��.������, 
                    na.rm = TRUE)
results[2,3] <- sum(now_credirect$���������.�������.��.����.��.������ , 
                    na.rm = TRUE)
results[2,4] <- sum(now_credirect$��������� , na.rm = TRUE) - 
                sum(before_credirect$��������� , na.rm = TRUE)

results[3,1] <- sum(now_fin$���������.�����.��.������ , 
                    na.rm = TRUE)
results[3,2] <- sum(now_fin$���������.���������.��.������������.��.������ , 
                    na.rm = TRUE)
results[3,3] <- sum(now_fin$���������.�������.��.����.��.������ , 
                    na.rm = TRUE)
results[3,4] <- sum(now_fin$��������� , na.rm = TRUE) - 
                sum(before_fin$��������� , na.rm = TRUE)

results[4,1] <- results[1,1] - results[2,1] - results[3,1]
results[4,2] <- results[1,2] - results[2,2] - results[3,2]
results[4,3] <- results[1,3] - results[2,3] - results[3,3]
results[4,4] <- results[1,4] - results[2,4] - results[3,4]


# Preparing final dataframe result table
colnames(results) <- c("��������� �����","��������� ���������",
                       "��������� ������� �� �����","��������� (������)")
rownames(results) <- c("����","Credirect","FinStart","City Cash")


########## Output data ###########
# Output results
wb <- loadWorkbook("Result.xlsx")
writeData(wb, sheet = "Result", results, colNames = T, rowNames = T)
saveWorkbook(wb,"Result.xlsx", overwrite = T)


############## END ##############
