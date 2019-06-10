#Set working directory
#setwd("E:/Google Drive/ƒ…’À…”‘«—…œ/Machine Learning")
setwd("C:/Users/ztsouralakis/Google Drive/ƒ…’À…”‘«—…œ/Machine Learning")

#Set Java directory
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_31') # home pc for 64-bit version
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201') # laptop pc for 64-bit version
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_211') # elpe pc for 64-bit version

#Load Libraries
library(readxl) #Read dataset from excel sheet
library(xlsx) #Write dataset from excel sheet
library(rJava) #Using xlsx library
library(corrplot) #Plotting correlations
library(caret)
library(psych) #pairs.panel correlation method
library(ggplot2) #Visualize data
library(Hmisc) #Impute data
library(doParallel) #Parallel processing
library(funModeling) #Scale data
library(naniar) #Replace 0 or negative values with NAs
library(missForest) #Impute NA values
library(olsrr) #Measures of influence using Cook's distance
library(outliers) #Outliers treatment
library(dplyr)
library(ggplot2) # Ggplot2 library
library(DataExplorer) #Data Visualization

library(mi) #Impute NA values
library(mice) #Impute NA values

library(dplyr)
library(ranger)
library(Boruta)

library(randomForest)

library(FSelector) #Feature Selection
library(DMwR) #knnImputation
library(DAAG) #Cross Validation

library(e1071) #SVM modeling
library(neuralnet) #Neural Net
library(boot) #Neural Net for CV
library(plyr) #Neural Net for CV
library(nnet) # Neural Network 
library(NeuralNetTools)
library(kernlab) #SVM with caret
library(mice) #Impute data

library(MASS) #BoxCox transformation
library(rcompanion) #BoxCox plottings
library(VIM) #Missing Values Plot
library(parallel) #Parallel processing

library(dataPreparation) #Scaling datasets
library(RANN) #knn imputation using caret