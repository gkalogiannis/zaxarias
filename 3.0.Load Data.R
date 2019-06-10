#Load data and save them in variables
#T-101 Header
Dataset.T101_Header <- read_excel("1.3.T101 Header Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 18)))

#T-150
Dataset.T150 <- read_excel("2.3.T150 Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 12)))

#T-102 & T-103
Dataset.T102_T103 <- read_excel("3.3.T102 - T103 Values.xlsx", col_types = c("date", rep("numeric", 39)))

#T-403 & T-405
Dataset.T403_T405 <- read_excel("4.3.T403 - T405 Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 18)))

#Light Ends
Dataset.LightEnds <- read_excel("5.3.Light Ends Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 70)))

#Isomerization
Dataset.Isomerization <- read_excel("6.3.Isomerization Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 37)))

#C.C.R.
Dataset.CCR <- read_excel("7.3.CCR Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 79)))

#T-409
Dataset.T409 <- read_excel("8.3.T409 Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 16)))

#DODD
Dataset.DODD <- read_excel("9.3.DODD Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 29)))

#ULSADO
Dataset.ULSADO <- read_excel("10.3.ULSADO Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 36)))

#Reactors
Dataset.Reactors <- read_excel("11.3.Reactors Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 47)))

#Sample Points
Dataset.SPs <- read_excel("12.3.SPs Values.xlsx", sheet = "Data", col_types = c("date", rep("numeric", 5)))


Dataset.Timestamp <- Dataset[,c(1)] #With Timestamp
Dataset <- Dataset[,c(-1)] #Without Timestamp

#Write Dataset to Excel
write.xlsx(Dataset, "E:\\Google Drive\\ÄÉÕËÉÓÔÇÑÉÏ\\Machine Learning\\Dataset.xlsx")


