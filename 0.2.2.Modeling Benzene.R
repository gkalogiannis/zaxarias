############# Calculate number of missing values ############# 
na_count <- sapply(Dataset.NAs, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count*100/nrow(Dataset.NAs))
#Remove columns that have more than 20% NAs
Dataset.NAs <- Dataset.NAs[, colMeans(is.na(Dataset.NAs)) <= .20] 


############# Impute Missing Values ############# 
#Perform missForest Imputation
Dataset.Imp <- missForest(Dataset.NAs, verbose = TRUE)
Dataset.Imp <- Dataset.Imp$ximp

#Impute missing values using Hmsic package
# Set up formula
impute_n <- names(Dataset.NAs)
impute_f <- as.formula(paste("SP8801_BENZENE~", paste(impute_n[!impute_n %in% c("SP8801_BENZENE")], collapse = " + ")))

start.time <- Sys.time() #measure time duration for Hmsic

impute_arg <- aregImpute(impute_f, 
                         data = Dataset.NAs,
                         nk = 0,
                         type = "pmm",
                         n.impute = 5)

end.time <- Sys.time()
time.taken.imp <- end.time - start.time

completed <- Dataset.NAs
imputed <-as.data.frame(impute.transcan (impute_arg, imputation = 1, 
                                         data = Dataset.NAs, 
                                         list.out = TRUE,
                                         pr = FALSE, 
                                         check=FALSE))

completed[names(imputed)] <-as.numeric(unlist(imputed))
Dataset.Imp <- data.frame(completed)


############# Correlation Matrix ############# 
#Correlation Matrix
CorrMatrix.Dataset <- cor(Dataset.Imp) #Hmisc Imputetion

#Finding high correlated features
highlyCorrelated = findCorrelation(CorrMatrix.Dataset, cutoff = 0.80)
featuresToRemove <- names(Dataset)[highlyCorrelated]
Dataset.New <- Dataset.Imp[, -highlyCorrelated]

#Write Correlation Matrix to Excel
filename = paste(gsub(":", "-", Sys.Date()), "_", 
                 nrow(Dataset), "obs", "_", 
                 ncol(Dataset), "vars", 
                 "_DatasetCorrMatrix.xlsx",sep="")
write.xlsx(CorrMatrix.Dataset, file = paste0("E:\\Google Drive\\диукистгяио\\Machine Learning\\Results\\2.Benzene\\", filename))

#Use funmodeling package to find correlations
Correlations.Benzene <- correlation_table(data = Dataset.New, target = "SP8801_BENZENE")

#Change column names
names(Correlations.Benzene)[1] <- "BENZENE_Fetaures"
names(Correlations.Benzene)[2] <- "BENZENE_CorrValues"

#Write the Correlations into an excel file
filename = paste(gsub(":", "-", Sys.Date()), "_", 
                 nrow(Dataset.New), "obs", "_", 
                 ncol(Dataset.New), "vars", 
                 "_Correlations.xlsx",sep="")

#Benzene
write.xlsx(Correlations.Benzene, 
           file = paste0("E:\\Google Drive\\диукистгяио\\Machine Learning\\Results\\2.Benzene\\", filename), 
           sheet = "Benzene")

############# Features Selection ############# 
#Load Features
Features.CorrValues.Benzene <- subset(Correlations.Benzene, 
                                        Correlations.Benzene$BENZENE_CorrValues <= -0.10 | 
                                          Correlations.Benzene$BENZENE_CorrValues >= 0.10)

filename = paste(gsub(":", "-", Sys.Date()), "_", 
                 nrow(Dataset), "obs", "_", 
                 ncol(Dataset), "vars",  
                 "_FeaturesCorr_BENZENE.xlsx",sep="")

write.xlsx(Features.CorrValues.Benzene, 
           file = paste0("E:\\Google Drive\\диукистгяио\\Machine Learning\\Results\\2.Benzene\\", filename), 
           sheet = "Benzene")

#Make a vector with correlated features
Features.Benzene <- Features.CorrValues.Benzene$BENZENE_Fetaures
#Make new dataset with selected features
Dataset.FS.Benzene <- Dataset.New[, Features.Benzene]

############# Split Dataset ############# 
# Create Training and Test data
set.seed(1006)  # setting seed to reproduce results of random sampling

ind <- sample(2, nrow(Dataset.FS.Benzene), replace = TRUE, prob = c(0.70, 0.30))
Dataset.trainData.FS <- Dataset.FS.Benzene[ind == 1,] # model training data
Dataset.testData.FS <- Dataset.FS.Benzene[ind == 2,] # test data

#Make a normalized dataset
normParam <- preProcess(Dataset.trainData.FS, method = c("range")) #c("center", "scale"))
Dataset.trainData.Norm <- predict(normParam, Dataset.trainData.FS)
Dataset.testData.Norm <- predict(normParam, Dataset.testData.FS)


############# Feature Selection Algorithm #############
# Ensure the results are repeatable
set.seed(1006)

# define the control using a random forest selection function
train.control <- rfeControl(functions = rfFuncs, 
                            method = "cv", 
                            number = 10)

#Run the RFE algorithm
start.time <- Sys.time() #measure time duration for RFE
#Parallel Processing
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

RFEresults.Benzene <- rfe(Dataset.trainData.Norm[,2:ncol(Dataset.trainData.Norm)], 
                          Dataset.trainData.Norm[,1], 
                          sizes = c(2:ncol(Dataset.trainData.Norm)), 
                          rfeControl = train.control)
end.time <- Sys.time()
time.taken.RFE <- end.time - start.time
#When the procedure ends
stopCluster(cl)

# summarize the results
print(RFEresults.Benzene)
# list the chosen features
predictors(RFEresults.Benzene)
# plot the results
plot1 <- plot(RFEresults.Benzene, type = c("g", "o"))
plot2 <- plot(RFEresults.Benzene, type = c("g", "o"), metric = "Rsquared")
print(plot1, split=c(1,1,1,2), more = TRUE)
print(plot2, split=c(1,2,1,2))

#Select different number of features, instead of best tuned
RFEresults <- RFEresults.Benzene$optVariables[1:19]

#Make Feature selected variables a dataset
Dataset.trainData.Fsed <- Dataset.trainData.Norm[, c(RFEresults, "SP8801_BENZENE")]
Dataset.testData.Fsed <- Dataset.testData.Norm[, c(RFEresults,"SP8801_BENZENE")]
#Dataset.trainData.Fsed <- Dataset.trainData.Norm[, c(predictors(RFEresults.RON),"SP8801_BENZENE")]
#Dataset.testData.Fsed <- Dataset.testData.Norm[, c(predictors(RFEresults.RON),"SP8801_BENZENE")]

#If not use outliers treatment
Dataset.trainData <- Dataset.trainData.Fsed
Dataset.testData <- Dataset.testData.Fsed


############# Visualize Feature Selected Dataset ############# 
#Find Correlations
CorrMatrix.Dataset.FSed.Benzene <- cor(Dataset.trainData)
#Plot Correlations
corrplot(cor(Dataset.trainData[,c(names(Dataset.trainData[1:19]), "SP8801_BENZENE")]),  
         title = "Correlation Matrix",
         method = "number",
         mar = c(0,0,1,0),
         tl.cex = 0.7,
         type = "lower", 
         order = "alphabet", 
         tl.col = "black", 
         tl.srt = 45)
#Finding high correlated features
highlyCorrelated = findCorrelation(Dataset.trainData, cutoff = 0.85)
featuresToRemove <- names(Dataset.trainData)[highlyCorrelated]
Dataset.trainData <- Dataset.trainData[, -highlyCorrelated]
Dataset.testData.Outliers <- Dataset.testData.Outliers[, -highlyCorrelated]

#Visualize correlations
pairs.panels(Dataset.trainData[,c(names(Dataset.trainData[1:6]), "SP8801_BENZENE")], 
             lm = TRUE,
             cex.labels = 1.1, 
             cex.cor = 1,
             ellipses = TRUE,
             density = TRUE)
pairs.panels(Dataset.trainData[,c(names(Dataset.trainData[7:12]), "SP8801_BENZENE")], 
             lm = TRUE,
             cex.labels = 1.1, 
             cex.cor = 1,
             ellipses = TRUE,
             density = TRUE)
pairs.panels(Dataset.trainData[,c(names(Dataset.trainData[13:19]), "SP8801_BENZENE")], 
             lm = TRUE,
             cex.labels = 1.1, 
             cex.cor = 1,
             ellipses = TRUE,
             density = TRUE)


############# SVM Modeling ############# 
# Set up formula
n <- names(Dataset.trainData)
formula<- as.formula(paste("SP8801_BENZENE ~", paste(n[!n %in% c("SP8801_BENZENE")], collapse = " + ")))

#Define training control
train.control <- trainControl(method = 'repeatedcv', 
                              number = 10, 
                              repeats = 3, 
                              savePredictions = T)
#Define grid
svm.grid <- expand.grid(C = 2**(-13:12))

start.time <- Sys.time() #measure time duration
#Parallel Processing
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

svm.model <- train(formula, 
                   data = Dataset.trainData, 
                   method = 'svmLinear', 
                   tuneGrid = svm.grid, 
                   trControl = train.control)
end.time <- Sys.time()
time.taken.svm <- end.time - start.time
#When the procedure ends
stopCluster(cl)

#Save workspace
filename = paste(gsub(":", "-", Sys.Date()), 
                 " SVM.RData",sep="")
save.image(file = paste0("C:\\Users\\ztsouralakis\\Google Drive\\диукистгяио\\Machine Learning\\Results\\2.Benzene\\", filename))


print(svm.model)

#Make a prediction
svm.prediction <- predict(svm.model, Dataset.testData[,-c(ncol(Dataset.testData))]) # Make a prediction
#Calculate residuals
svm.model.residuals <- Dataset.testData$SP8801_BENZENE - svm.prediction

#Calculate R2, RMSE & MAE
R2 = R2(svm.prediction, Dataset.testData[,c(ncol(Dataset.testData))])
R2adj = 1-(1-R2)*(nrow(Dataset.testData))/(nrow(Dataset.testData)-length(Dataset.testData[,-c(ncol(Dataset.testData))])-1)
RMSE = RMSE(Dataset.testData[,c(ncol(Dataset.testData))], svm.prediction)
MAE = MAE(Dataset.testData[,c(ncol(Dataset.testData))], svm.prediction)

print(R2)
print(R2adj)
print(RMSE)
print(MAE)

# Observed values versus predicted values
plot(svm.prediction, 
     Dataset.testData$SP8801_BENZENE,
     col = 'blue',
     main = 'Real vs Predicted - SVM Modeling',
     pch = 4,
     cex = 0.7,
     xlab = 'predicted',
     ylab = 'observed')
abline(0,1, col = 'black')

# Predicted values versus residuals
plot(svm.prediction, 
     svm.model.residuals, 
     col = 'blue',
     main = 'Residuals Plot - SVM Modeling',
     pch = 1,
     cex = 0.7,     
     xlab = 'prediction',
     ylab = 'residual')
abline(h = 0, col = "darkgrey", lty = 2)

qqnorm(svm.prediction, pch = 1, frame = FALSE)
qqline(svm.prediction, col = "steelblue", lwd = 2)

############# NNET Modeling ############# 
# Set up formula without outliers treatment - Normalized Data
n <- names(Dataset.trainData)
formula <- as.formula(paste("SP8801_BENZENE ~", paste(n[!n %in% c("SP8801_BENZENE")], collapse = " + ")))

#Using Best Model for Prediction

set.seed(1006)

#Define training control
train.control <- trainControl(method = 'repeatedcv', 
                              number = 10, 
                              repeats = 3, 
                              savePredictions = T)
#trainControl(method = "cv", number = 10)

my.grid <- data.frame(layer1 = 5:30, 
                      layer2 = 1:26, 
                      layer3 = 0:12)

#Train the model
start.time <- Sys.time() #measure time duration for training
#Parallel Processing
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

nnet.model <- train(formula, 
                    data = Dataset.trainData, 
                    trControl = train.control,
                    tuneGrid = my.grid,
                    threshold = 0.01,        
                    stepmax = 1e+07,
                    method = "neuralnet")
end.time <- Sys.time()
time.taken.nnet <- end.time - start.time
#When the procedure ends
stopCluster(cl)

#Save workspace
filename = paste(gsub(":", "-", Sys.Date()), 
                 " Final.RData",sep="")
save.image(file = paste0("C:\\Users\\ztsouralakis\\Google Drive\\диукистгяио\\Machine Learning\\Results\\2.Benzene\\", filename))


print(nnet.model)

#Make a prediction
nnet.prediction <- predict.train(nnet.model, Dataset.testData[,-c(ncol(Dataset.testData))]) 
#Calculate residuals
nnet.model.residuals <- Dataset.testData$SP8801_BENZENE - nnet.prediction

#Calculate R2, RMSE & MAE
R2 = R2(nnet.prediction, Dataset.testData[, c(ncol(Dataset.testData))])
R2adj = 1 - (1 - R2) * (nrow(Dataset.testData)) / (nrow(Dataset.testData) - length(Dataset.testData[, -c(ncol(Dataset.testData))]) - 1)
RMSE = RMSE(Dataset.testData[, c(ncol(Dataset.testData))], nnet.prediction)
MAE = MAE(Dataset.testData[, c(ncol(Dataset.testData))], nnet.prediction)

print(R2)
print(R2adj)
print(RMSE)
print(MAE)

# Observed values versus predicted values
plot(nnet.prediction, 
     Dataset.testData$SP8801_BENZENE, 
     col = 'blue',
     main = 'Real vs Predicted - NNET Modeling',
     pch = 4,
     cex = 0.7,
     xlab = 'predicted',
     ylab = 'observed')
abline(0,1, col = 'black')

# Predicted values versus residuals
plot(nnet.prediction, 
     nnet.model.residuals, 
     col = 'blue',
     main = 'Residuals Plot - NNET Modeling',
     pch = 1,
     cex = 0.7,     
     xlab = 'prediction',
     ylab = 'residual')
abline(h = 0, col = "darkgrey", lty = 2)

qqnorm(nnet.prediction, pch = 1, frame = FALSE)
qqline(nnet.prediction, col = "steelblue", lwd = 2)


############# Measuring models performances ############# 

modelResults <- resamples(list("SVM" = svm.model, "NNET" = nnet.model))

parallelplot(modelResults, metric = "RMSE")
parallelplot(modelResults , metric = "Rsquared")

# boxplots of results
bwplot(modelResults)
# dot plots of results
dotplot(modelResults)

#Plotting NNET 
layers <- paste(nnet.model$results$layer1, "-", nnet.model$results$layer2, "-", nnet.model$results$layer3)
#RMSE
plot1 <- ggplot(data = data.frame(layers, nnet.model$results$RMSE), 
                aes(x = layers, y = nnet.model$results$RMSE, group=1)) +
  geom_line(color = "green") +
  geom_point() +
  labs(title="RMSE plot for every layer",x="Layers", y = "RMSE") +
  theme_classic()
plot1 + theme(axis.text.x.bottom = element_text(size = rel(.9), angle = 45))
#R2
plot1 <- ggplot(data = data.frame(layers, nnet.model$results$Rsquared), 
                aes(x = layers, y = nnet.model$results$Rsquared*100, group=1)) +
  geom_line(color = "green") +
  geom_point() +
  labs(title="R2 plot for every layer",x="Layers", y = "R2") +
  theme_classic()
plot1 + theme(axis.text.x.bottom = element_text(size = rel(.9), angle = 45))

