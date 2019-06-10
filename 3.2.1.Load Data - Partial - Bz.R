#Make new features by averaging the Reactors TIs
Dataset.Reactors$TIsAvgsR105_1st <- rowMeans(subset(Dataset.Reactors, select = c("TI157_1.PV", "TI157_2.PV", "TI157_3.PV", "TI157_4.PV", "TI157_5.PV"))) #R-105 1st thermowell
Dataset.Reactors$TIsAvgsR105_4th <- rowMeans(subset(Dataset.Reactors, select = c("TI160_1.PV", "TI160_2.PV", "TI160_3.PV", "TI160_4.PV", "TI160_5.PV"))) #R-105 4th thermowell
Dataset.Reactors$TIsAvgsR170_TB <- rowMeans(subset(Dataset.Reactors, select = c("TI1728.PV", "TI1731.PV"))) #R-170 Top bed thermowells
Dataset.Reactors$TIsAvgsR170_BB <- rowMeans(subset(Dataset.Reactors, select = c("TI1734.PV", "TI1737.PV"))) #R-170 Bottom bed thermowells
Dataset.Reactors$TIsAvgsR171_TB <- rowMeans(subset(Dataset.Reactors, select = c("TI1742.PV", "TI1745.PV"))) #R-171 Top bed thermowells
Dataset.Reactors$TIsAvgsR171_BB <- rowMeans(subset(Dataset.Reactors, select = c("TI1748.PV", "TI1751.PV"))) #R-171 Bottom bed thermowells
Dataset.Reactors$TIsAvgsR404 <- rowMeans(subset(Dataset.Reactors, select = c("TI404B_4.PV", "TI404B_9.PV", "TI404B_14.PV", "TI404B_19.PV"))) #R-105 4th thermowellS
Dataset.Reactors$TIsAvgsR321A <- rowMeans(subset(Dataset.Reactors, select = c("TI342A.PV", "TI342B.PV", "TI342C.PV", "TI342D.PV", "TI342E.PV"))) #R-321A thermowellS
Dataset.Reactors$TIsAvgsR321B <- rowMeans(subset(Dataset.Reactors, select = c("TI343A.PV", "TI343B.PV", "TI343C.PV", "TI343D.PV", "TI343E.PV"))) #R-321B thermowellS
Dataset.Reactors$TIsAvgsR1901_1stB <- rowMeans(subset(Dataset.Reactors, select = c("TI1914H.PV", "TI1915H.PV"))) #R-1901 1st Bed thermowellS
Dataset.Reactors$TIsAvgsR1901_2ndB <- rowMeans(subset(Dataset.Reactors, select = c("TI1916H.PV", "TI1917H.PV"))) #R-1901 1st Bed thermowellS
Dataset.Reactors$TIsAvgsR1901_3rdB <- rowMeans(subset(Dataset.Reactors, select = c("TI1918H.PV", "TI1919H.PV"))) #R-1901 1st Bed thermowellS
Dataset.Reactors$TIsAvgsR1401 <- rowMeans(subset(Dataset.Reactors, select = c("TI14014.PV", "TI14017.PV", "TI14020.PV", "TI14022.PV"))) #R-1401 thermowellS
#Exclude columns from Reactors dataset
Dataset.Reactors <- subset(Dataset.Reactors, select = -c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 
                                                         19, 20, 21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 
                                                         33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 
                                                         48))


#Join the dataframes
Dataset <- cbind(Dataset.T101_Header, 
                 Dataset.T150[,c(-1)],
                 Dataset.T102_T103[,c(-1)],
                 Dataset.T403_T405[,c(-1)],
                 Dataset.LightEnds[,c(-1)],
                 Dataset.Isomerization[,c(-1)],
                 Dataset.CCR[,c(-1)],
                 Dataset.T409[,c(-1)],
                 Dataset.DODD[,c(-1)],
                 Dataset.ULSADO[,c(-1)],
                 Dataset.Reactors[,c(-1)],
                 #Dataset.SPs[,c(-1)]) #Include all SPs
                 Dataset.SPs[,c(-1, -2, -4, -5, -6)]) #Exclude all SPs - Feature Selection for Benzene


Dataset <- Dataset[,c(-1)] #Without Timestamp

#Remove Datasets from workspace
rm(Dataset.T101_Header, 
   Dataset.T150,
   Dataset.T102_T103,
   Dataset.T403_T405,
   Dataset.LightEnds,
   Dataset.Isomerization,
   Dataset.CCR,
   Dataset.T409,
   Dataset.DODD,
   Dataset.ULSADO,
   Dataset.Reactors,
   Dataset.SPs)

############# Convert 0 or negative values to NAs - Missing Values - To all dataset ############# 
#Dataset.NAs <- replace_with_na_all(Dataset, condition = ~.x <= 0)
#Reorder the tags
#Dataset.NAs <- Dataset.NAs[ , order(names(Dataset.NAs))]
Dataset.NAs <- Dataset[ , order(names(Dataset))]

############# Convert 0 or negative values to NAs - Missing Values - Part of dataset ############# 
Dataset.NAs <- replace_with_na_all(Dataset[1:1515,], condition = ~.x <= 0)
Dataset.NAs <- rbind(Dataset.NAs, Dataset[1516:nrow(Dataset),])



############# Make a dataset with missing values ############# 
#Make outliers as NA - To part of the records 
Dataset.NAs <- as.data.frame(sapply(Dataset[1:1515,], replace_outlier_with_missing))
Dataset.NAs <- rbind(Dataset.NAs, Dataset[1516:nrow(Dataset),])
#Make outliers as NA - To all records
Dataset.NAs <- as.data.frame(sapply(Dataset, replace_outlier_with_missing))
Dataset.NAs <- Dataset.NAs[ , order(names(Dataset.NAs))]
#Reorder the tags
Dataset.NAs <- Dataset.NAs[ , order(names(Dataset.NAs))]
