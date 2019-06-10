############# Plot missing values ############# 

plot_missing(Dataset.NAs[,1:50])    
plot_missing(Dataset.NAs[,51:100]) 
plot_missing(Dataset.NAs[,101:150]) 
plot_missing(Dataset.NAs[,151:200]) 
plot_missing(Dataset.NAs[,201:250]) 
plot_missing(Dataset.NAs[,251:300]) 
plot_missing(Dataset.NAs[,301:353])


#Plot Correlation matrix for train dataset
plot_correlation(na.omit(Dataset.trainData[ , order(names(Dataset.trainData))]), maxcat = 5L)


plot_boxplot(Dataset.trainData[ , order(names(Dataset.trainData))], by = "SP8801_BENZENE") 


#Make Feature selected variables a dataset
Dataset.trainData.graphs <- Dataset.trainData.Imp[, c(RFEresults, "SP8801_BENZENE")]
Dataset.trainData.graphs <- Dataset.trainData.graphs[ , order(names(Dataset.trainData.graphs))]


############# AC-14479 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$AC14479.PV, 
        horizontal = TRUE , 
        ylim = c(2, 9), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$AC14479.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "AC-14479",
     xlim = c(2, 9),
     ylab = "O2 %")

############# FC-14012 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC14012.PV, 
        horizontal = TRUE , 
        ylim = c(35000, 55000), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC14012.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-14012",
     xlim = c(35000, 55000),
     ylab = "Flow m3/h")

############# FC-14023 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC14023.PV, 
        horizontal = TRUE , 
        ylim = c(3, 46), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC14023.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-14023",
     xlim = c(3, 46),
     ylab = "Flow m3/h")

############# FC-14024 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC14024.PV, 
        horizontal = TRUE , 
        ylim = c(0, 10), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC14024.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-14024",
     xlim = c(0, 10),
     ylab = "Flow m3/h")

############# FC-315 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC315.PV, 
        horizontal = TRUE , 
        ylim = c(0, 54), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC315.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-315",
     xlim = c(0, 54),
     ylab = "Flow m3/h")

############# FC-327A ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC327A.PV, 
        horizontal = TRUE , 
        ylim = c(0, 5000), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC327A.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-327A",
     xlim = c(0, 5000),
     ylab = "Flow m3/h")

############# FC-328 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC328.PV, 
        horizontal = TRUE , 
        ylim = c(0, 3.5), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC328.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-328",
     xlim = c(0, 3.5),
     ylab = "Flow m3/h")


############# FC-338 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC338.PV, 
        horizontal = TRUE , 
        ylim = c(0.5, 60), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC338.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-338",
     xlim = c(0.5, 60),
     ylab = "Flow m3/h")


############# FC-484 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC484.PV, 
        horizontal = TRUE , 
        ylim = c(11, 35), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC484.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-484",
     xlim = c(11, 35),
     ylab = "Flow m3/h")


############# FC-485 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FC485.PV, 
        horizontal = TRUE , 
        ylim = c(3, 23), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FC485.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FC-485",
     xlim = c(3, 23),
     ylab = "Flow m3/h")


############# FI-14070 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$FI14070.PV, 
        horizontal = TRUE , 
        ylim = c(7000, 20000), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$FI14070.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "FI-14070",
     xlim = c(7000, 20000),
     ylab = "Flow m3/h")


############# PC-302 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$PC302.PV, 
        horizontal = TRUE , 
        ylim = c(0.5, 3), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$PC302.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "PC-302",
     xlim = c(0.5, 3),
     ylab = "Temperature C")


############# PC-481 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$PC481.PV, 
        horizontal = TRUE , 
        ylim = c(0.4, 1.6), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$PC481.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "PC-481",
     xlim = c(0.4, 1.6),
     ylab = "Pressure Kgr/cm2")


############# TC-14070 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TC14070.PV, 
        horizontal = TRUE , 
        ylim = c(66, 90), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TC14070.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "TC-14070",
     xlim = c(66, 90),
     ylab = "Temperature C")


############# TC-340 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TC340.PV, 
        horizontal = TRUE , 
        ylim = c(110, 160), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TC340.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "TC-340",
     xlim = c(110, 160),
     ylab = "Temperature C")


############# TI-14061 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TI14061.PV, 
        horizontal = TRUE , 
        ylim = c(165, 205), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TI14061.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "TI-14061",
     xlim = c(165, 205),
     ylab = "Temperature C")


############# TI-14064 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TI14064.PV, 
        horizontal = TRUE , 
        ylim = c(245, 265), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TI14064.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "TI-14064",
     xlim = c(245, 265),
     ylab = "Temperature C")


############# TI-14077 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TI14077.PV, 
        horizontal = TRUE , 
        ylim = c(250, 280), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TI14077.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "TI-14077",
     xlim = c(250, 280),
     ylab = "Temperature C")


############# TI-360 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TI360.PV, 
        horizontal = TRUE , 
        ylim = c(100, 150), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TI360.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "TI-360",
     xlim = c(100, 150),
     ylab = "Temperature C")


############# TI-482 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TI482.PV , 
        horizontal = TRUE , 
        ylim = c(70, 120), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TI482.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "TI-482",
     xlim = c(70, 120),
     ylab = "Temperature C")


############# TI-485 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TI485.PV, 
        horizontal = TRUE , 
        ylim = c(55, 95), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TI485.PV,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "TI-485",
     xlim = c(55, 95),
     ylab = "Temperature C")


############# TI-487 ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$TI487.PV , 
        horizontal = TRUE , 
        ylim = c(130, 170), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$TI487.PV,
     breaks = 40,
     col = rgb(0.2, 0.8, 0.5, 0.5),
     border = F,
     main = "",
     xlab = "TI-487",
     xlim = c(130, 170),
     ylab = "Temperature C")


############# SP-8801 Bz ############# 
# Layout to split the screen
layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE), height = c(1, 8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Dataset.trainData.graphs$SP8801_BENZENE, 
        horizontal = TRUE , 
        ylim = c(0, 5), 
        xaxt = "n" , 
        col = rgb(0.8, 0.8, 0, 0.5), 
        frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(Dataset.trainData.graphs$SP8801_BENZENE,
     breaks = 40,
     col = rgb(0.2, 0.8 ,0.5, 0.5),
     border = F,
     main = "",
     xlab = "SP-8801 Bz",
     xlim = c(0, 5),
     ylab = "ppm")
 



