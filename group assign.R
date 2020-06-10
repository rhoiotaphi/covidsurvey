# Code to assign treatment groups to respondents
# Last Modified : 5/23/2020

library(readxl)
library(stringr)
library(tm)
Data <- read_excel("C:/Users/hemra/Downloads/Data.xlsx")
View(Data)

Data$group <- NA
Data$t1 <- NA
Data$t2 <- NA
Data$t3 <- NA
Data$t4 <- NA

#Group 1: Expert Fact Gain
Data[which(!is.na(Data$EFG1_1)),]$group <- 1
Data[which(!is.na(Data$EFG1_1)),]$t1 <- Data[which(!is.na(Data$EFG1_1)),]$EFG1_1
Data[which(!is.na(Data$EFG1_1)),]$t2 <- Data[which(!is.na(Data$EFG1_1)),]$EFG1_1
Data[which(!is.na(Data$EFG1_1)),]$t3 <- Data[which(!is.na(Data$EFG1_1)),]$EFG1_1
Data[which(!is.na(Data$EFG1_1)),]$t4 <- Data[which(!is.na(Data$EFG1_1)),]$EFG1_1

#Group 2: Expert Fact Loss
Data[which(!is.na(Data$EFL1_1)),]$group <- 2
Data[which(!is.na(Data$EFL1_1)),]$t1 <- Data[which(!is.na(Data$EFL1_1)),]$EFL1_1
Data[which(!is.na(Data$EFL1_1)),]$t2 <- Data[which(!is.na(Data$EFL1_1)),]$EFL2_1
Data[which(!is.na(Data$EFL1_1)),]$t3 <- Data[which(!is.na(Data$EFL1_1)),]$EFL3_1
Data[which(!is.na(Data$EFL1_1)),]$t4 <- Data[which(!is.na(Data$EFL1_1)),]$EFL4_1

#Group 3: Expert Experience Gain
Data[which(!is.na(Data$EEG1_1)),]$group <- 3
Data[which(!is.na(Data$EEG1_1)),]$t1 <- Data[which(!is.na(Data$EEG1_1)),]$EEG1_1
Data[which(!is.na(Data$EEG1_1)),]$t2 <- Data[which(!is.na(Data$EEG1_1)),]$EEG2_1
Data[which(!is.na(Data$EEG1_1)),]$t3 <- Data[which(!is.na(Data$EEG1_1)),]$EEG3_1
Data[which(!is.na(Data$EEG1_1)),]$t4 <- Data[which(!is.na(Data$EEG1_1)),]$EEG4_1

#Group 4: Expert Experience Loss
Data[which(!is.na(Data$EEL1_1)),]$group <- 4
Data[which(!is.na(Data$EEL1_1)),]$t1 <- Data[which(!is.na(Data$EEL1_1)),]$EEL1_1
Data[which(!is.na(Data$EEL1_1)),]$t2 <- Data[which(!is.na(Data$EEL1_1)),]$EEL2_1
Data[which(!is.na(Data$EEL1_1)),]$t3 <- Data[which(!is.na(Data$EEL1_1)),]$EEL3_1
Data[which(!is.na(Data$EEL1_1)),]$t4 <- Data[which(!is.na(Data$EEL1_1)),]$EEL4_1

#Group 5 : Nonexpert Fact Gain
Data[which(!is.na(Data$NFG1_1)),]$group <- 5
Data[which(!is.na(Data$NFG1_1)),]$t1 <- Data[which(!is.na(Data$NFG1_1)),]$NFG1_1
Data[which(!is.na(Data$NFG1_1)),]$t2 <- Data[which(!is.na(Data$NFG1_1)),]$NFG2_1
Data[which(!is.na(Data$NFG1_1)),]$t3 <- Data[which(!is.na(Data$NFG1_1)),]$NFG3_1
Data[which(!is.na(Data$NFG1_1)),]$t4 <- Data[which(!is.na(Data$NFG1_1)),]$NFG4_1

#Group 6 : Nonexpert Fact Loss
Data[which(!is.na(Data$NFL1_1)),]$group <- 6
Data[which(!is.na(Data$NFL1_1)),]$t1 <- Data[which(!is.na(Data$NFL1_1)),]$NFL1_1
Data[which(!is.na(Data$NFL1_1)),]$t2 <- Data[which(!is.na(Data$NFL1_1)),]$NFL2_1
Data[which(!is.na(Data$NFL1_1)),]$t3 <- Data[which(!is.na(Data$NFL1_1)),]$NFL3_1
Data[which(!is.na(Data$NFL1_1)),]$t4 <- Data[which(!is.na(Data$NFL1_1)),]$NFL4_1

#Group 7 : Nonexpert Experience Gain
Data[which(!is.na(Data$NEG1_1)),]$group <- 7
Data[which(!is.na(Data$NEG1_1)),]$t1 <- Data[which(!is.na(Data$NEG1_1)),]$NEG1_1
Data[which(!is.na(Data$NEG1_1)),]$t2 <- Data[which(!is.na(Data$NEG1_1)),]$NEG2_1
Data[which(!is.na(Data$NEG1_1)),]$t3 <- Data[which(!is.na(Data$NEG1_1)),]$NEG3_1
Data[which(!is.na(Data$NEG1_1)),]$t4 <- Data[which(!is.na(Data$NEG1_1)),]$NEG4_1

#Group 8 : Nonexpert Experience Loss

Data[which(!is.na(Data$NEL1_1)),]$group <- 8
Data[which(!is.na(Data$NEL1_1)),]$t1 <- Data[which(!is.na(Data$NEL1_1)),]$NEL1_1
Data[which(!is.na(Data$NEL1_1)),]$t2 <- Data[which(!is.na(Data$NEL1_1)),]$NEL2_1
Data[which(!is.na(Data$NEL1_1)),]$t3 <- Data[which(!is.na(Data$NEL1_1)),]$NEL3_1
Data[which(!is.na(Data$NEL1_1)),]$t4 <- Data[which(!is.na(Data$NEL1_1)),]$NEL4_1

#Group 9: Control

Data[which(!is.na(Data$C1_1)),]$group <- 9
Data[which(!is.na(Data$C1_1)),]$t1 <- Data[which(!is.na(Data$C1_1)),]$C1_1
Data[which(!is.na(Data$C1_1)),]$t2 <- Data[which(!is.na(Data$C1_1)),]$C2_1
Data[which(!is.na(Data$C1_1)),]$t3 <- Data[which(!is.na(Data$C1_1)),]$C3_1
Data[which(!is.na(Data$C1_1)),]$t4 <- Data[which(!is.na(Data$C1_1)),]$C4_1

#Saving the File
write.csv(Data,"Data_assign.csv")
