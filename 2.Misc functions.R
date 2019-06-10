#Function to normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Outliers treatment - Replace outliers with NA's
replace_outlier_with_missing <- function(x, na.rm = TRUE, ...) {
  
  #Find position of 1st and 3rd quantile not including NA's
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)  # get %iles
  
  H <- 1.5 * IQR(x, na.rm = na.rm)  # outlier limit threshold
  
  y <- x
  
  y[x < (qnt[1] - H)] <- NA  # replace values below lower bounds
  
  y[x > (qnt[2] + H)] <- NA  # replace values above higher bound
  
  y  # returns treated variable
  
}

#Outliers treatment - Remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  
  #Find position of 1st and 3rd quantile not including NA's
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)  # get %iles
  
  H <- 1.5 * IQR(x, na.rm = na.rm)  # outlier limit threshold
  
  y <- x
  
  y[x < (qnt[1] - H)] <- NA  # replace values below lower bounds
  
  y[x > (qnt[2] + H)] <- NA  # replace values above higher bound
  
  x[!is.na(x)]  # remove NA's
  
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

