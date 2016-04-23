# Name: Sharath Chand PV
# ID: vp4pa
# Data Mining Case Study2 - Evaluating Crime Predictions Using KDE
# -------------------------------------------------------------------
setwd('/Users/homw/Documents/MSDS16/DataMining/CS1')
library(maptools)
library(rgdal)
library(lubridate)
library(ks)
library(zoo)

rm(list=ls())
#-- Create city boundary from the shapefile
# read shapefile
city.boundary = readShapePoly("/Users/homw/Documents/MSDS16/DataMining/CS1/City_20Boundary/City_Boundary")

# set projection
proj4string(city.boundary) = "+init=epsg:3435"

# reproject to meters
city.boundary = spTransform(city.boundary, CRS("+init=epsg:26971"))

# display shapefile
plot(city.boundary, axes=TRUE, border="black", asp=1)
bbox(city.boundary)  # show bounding box coordinates
resolution = 800

# considering Theft cases to evaluate kde 
data_full <- read.csv('/Users/homw/Documents/MSDS16/DataMining/CS1/2014_THEFT.csv', header = TRUE)
#Preprocessing
# Step1: Coordinate Ref system of city boundary in meters. Let us convert the location coordinates of our data to the same CRS
latlong <- data_full[,c("Longitude", "Latitude")]
latlong <- project(as.matrix(latlong), proj="+init=epsg:26971")

# Step2: parse the date (ex: 12/04/2014 11:30:00 PM)
date <- strptime(data_full$Date, format = "%m/%d/%Y %I:%M:%S %p") 
#If 'H' is used for hour, AM / PM variations will be lost. 'I' should be used

# Step3: Add Extracted features to the data for further analysis
data_full$x <- latlong[,1] # reprojected longitude (E-W)
data_full$y <- latlong[,2]

data_full$Month <- month(date)
data_full$Weekday <- wday(date)
data_full$Hour <- hour(date)
data_full$Date <- format(date)
# ****************************************************************
# Crime is of cyclical nature which can be predicted from past occurrence. But the cyclical nature might be more obserevd in some specific periods when comapred to other periods. It is similar to saying that KDE can better estimate some specific durations of day, week or month from its corresponding historical values. If that is the case, let us check for various scenarios!

#Checking KDE performance for various periods of day
nrow(data_full[data_full$Hour == 0,])

Hour <- as.data.frame(matrix(data = NA, nrow = 24, ncol = 4))
names(Hour) <- c("Hr", "test1", "test2", "test3")

#Surveillance plots for every hour of day
for (i in 0:23){
  Hour$Hr[i+1] <- i
  # For each day, Divide the full data in to 3 sets. Jan-Apr, May-Aug, Sept-Dec
  set1 <- data_full[(data_full$Month<=4 & data_full$Hour == i),]
  set2 <- data_full[(data_full$Month<=8 & data_full$Month>=5
                     & data_full$Hour == i),]
  set3 <- data_full[(data_full$Month>=9
                     & data_full$Hour == i),]
 
  # kde on Set1, eval Set2: kde on Set2, eval Set3: kde on Set1+Set2, eval Set3 
  #test1: 
  #set train and validation sets
  estimates <- mykde(set1, 500)
  eval <- set2[,c("x","y")]
  eval <- eval[complete.cases(eval),]
  #Get KDE estimates
  fname <- paste('Hr', i, "auc1", sep = '_')
  jpeg(fname)
  Hour$test1[i+1] <- kde_eval(estimates, eval)
  dev.off()
  #test2: 
  #set train and validation sets
  estimates <- mykde(set2, 500)
  eval <- set3[,c("x","y")]
  eval <- eval[complete.cases(eval),]
  #Get KDE estimates
  fname <- paste('Hr', i, "auc2", sep = '_')
  jpeg(fname)
  Hour$test2[i+1] <- kde_eval(estimates, eval)
  dev.off()
  #test3: 
  #set train and validation sets
  estimates <- mykde(as.data.frame(rbind(set1,set2)), 500)
  eval <- set3[,c("x","y")]
  eval <- eval[complete.cases(eval),]
  #Get KDE estimates
  fname <- paste('Hr', i, "auc3", sep = '_')
  jpeg(fname)
  Hour$test3[i+1] <- kde_eval(estimates, eval)
  dev.off()
}
# Compare KDE performance for different Hours of day
plot(x = Hour$Hr, y = Hour$test3, type = "o", 
     ylim = c(0.6,0.8), pch = 20, cex = 0.5,
     col = "blue", xlab = "Hour of day",
     ylab = "AUC value",
     main = "KDE Perfomance on Hour of Day")

#Checking KDE performance for various days of week
nrow(data_full[data_full$Weekday == 7,])

Day <- as.data.frame(matrix(data = NA, nrow = 7, ncol = 4))
names(Day) <- c("Day", "test1", "test2", "test3")

for (i in 1:7){
  Day$Day[i] <- i
  set1 <- data_full[(data_full$Month<=4 & data_full$Hour == i),]
  set2 <- data_full[(data_full$Month<=8 & data_full$Month>=5
                     & data_full$Hour == i),]
  set3 <- data_full[(data_full$Month>=9
                     & data_full$Hour == i),]
  
  #test1: 
  #set train and validation sets
  estimates <- mykde(set1, 500)
  eval <- set2[,c("x","y")]
  eval <- eval[complete.cases(eval),]
  #Get KDE estimates
  fname <- paste('Day', i, "auc1", sep = '_')
  jpeg(fname)
  Day$test1[i] <- kde_eval(estimates, eval)
  dev.off()
  #test2: 
  #set train and validation sets
  estimates <- mykde(set2, 500)
  eval <- set3[,c("x","y")]
  eval <- eval[complete.cases(eval),]
  #Get KDE estimates
  fname <- paste('Day', i, "auc2", sep = '_')
  jpeg(fname)
  Day$test2[i] <- kde_eval(estimates, eval)
  dev.off()
  #test3: 
  #set train and validation sets
  estimates <- mykde(as.data.frame(rbind(set1,set2)), 500)
  eval <- set3[,c("x","y")]
  eval <- eval[complete.cases(eval),]
  #Get KDE estimates
  fname <- paste('Day', i, "auc3", sep = '_')
  jpeg(fname)
  Day$test3[i] <- kde_eval(estimates, eval)
  dev.off()
}
# Compare KDE performance for different Hours of day
plot(x = Day$Day, y = Day$test1, type = "o", 
     ylim = c(0.6,0.8), pch = 20, cex = 0.5,
     col = "red", xlab = "Day of week",
     ylab = "AUC value",
     main = "KDE Perfomance on Days of week")


#Checking KDE performance for various Months
nrow(data_full[data_full$Month == 11,])

Month <- as.data.frame(matrix(data = NA, nrow = 11, ncol = 3))
names(Month) <- c("Month", "test1", "test2")
for (i in 2:12){
  Month$Month[i-1] <- i
  set1 <- data_full[(data_full$Month == i-1),] # Previous month data only
  set2 <- data_full[(data_full$Month < i),] # Available past data corresponding to ith month
  set3 <- data_full[(data_full$Month == i),] # Current Month for evaluation
  
  #test1: 
  #set train and validation sets
  estimates <- mykde(set1, 500)
  eval <- set3[,c("x","y")]
  eval <- eval[complete.cases(eval),]
  #Get KDE estimates
  fname <- paste('Month', i, "auc1", sep = '_')
  jpeg(fname)
  Month$test1[i-1] <- kde_eval(estimates, eval)
  dev.off()
  
  #test2: 
  #set train and validation sets
  estimates <- mykde(set2, 500)
  eval <- set3[,c("x","y")]
  eval <- eval[complete.cases(eval),]
  #Get KDE estimates
  fname <- paste('Month', i, "auc2", sep = '_')
  jpeg(fname)
  Month$test2[i-1] <- kde_eval(estimates, eval)
  dev.off()
}
  
# Compare KDE performance for different Months.
plot(x = Month$Month, y = Month$test1, type = "o", 
     ylim = c(0.6,0.8), pch = 20, cex = 0.5,
     col = "red", xlab = "Month",
     ylab = "AUC value",
     main = "KDE Perfomance on different Months")



#--------------Functions------------------------
#Function for KDE construction
mykde <- function(data, n){
  range <- bbox(city.boundary) #Get the boundary range
  x_axis <- seq(range[1],range[3], resolution) #Set x-axis range
  y_axis <- seq(range[2],range[4], resolution) #Set y-axis range
  axis <- expand.grid(x_axis, y_axis) #
  names(axis) = c("x", "y")
  
  #Points to be plotted
  n <- ifelse(n>nrow(data), nrow(data),n) # no. of points as per the input parameter
  sample = sample(nrow(data),n) #Take a sample from the data
  points_data = data[sample,c("x","y")] 
  points_data <- points_data[complete.cases(points_data),]
  
  # compute optimal KDE bandwidth - Ensure no NAs
  h = Hpi(points_data, pilot="dscalar")
  
  # run KDE
  est = kde(points_data, H=h, eval.points=axis)$estimate
  
  #Build a dataframe of kde estimates
  estimates <- as.data.frame(cbind(axis, est))
  names(estimates) <- c("x", "y", "threat")
  # sort estimates by threat level
  estimates <- estimates[order(estimates$threat, decreasing = TRUE),]
  
  return(estimates)
}
# Check if the to be evaluated point falls with in a square of size resolution around any of the estimated point. Get all those squares in which our evaluation points have fallen.
square.crime = function(crime.point, estimates){
  indices = estimates[!(estimates$lx > crime.point["x"] |
                          estimates$ux < crime.point["x"] |
                          estimates$ly > crime.point["y"] |
                          estimates$uy < crime.point["y"]), "id"]
  
  if(length(indices) != 0)
    return (indices[1])
  else
  {
    return(NA)
  }
}
# Evaluate given points based kde estimates 
kde_eval <- function(estimates, eval){
  # step1: filter out points outside the study area (boundary considered)
  # for estimated points
  points = SpatialPoints(estimates[,c("x", "y")], proj4string=city.boundary@proj4string)
  filtered = !is.na(over(points, city.boundary)$OBJECTID)
  estimates = estimates[filtered,]
  
  # for eval points
  points = SpatialPoints(eval[,c("x", "y")], proj4string=city.boundary@proj4string)
  filtered = !is.na(over(points, city.boundary)$OBJECTID)
  eval = eval[filtered,]
  
  # Create a square around each estimated point and later we will count how many square into which each eval point is falling
  
  # Create a square box for estimates
  width <- resolution/2
  estimates <- cbind(seq(1, nrow(estimates)),
                     estimates$threat,
                     estimates$x - width,
                     estimates$x + width,
                     estimates$y - width,
                     estimates$y + width,
                     rep(0, nrow(estimates)))
  
  estimates <- as.data.frame(estimates)
  colnames(estimates) <- c('id', 'threat', 'lx','ux','ly','uy','captured.crimes')
  
  # Get all the squares in which to be evaluated points are falling
  eval.squares = apply(eval, 1, square.crime, estimates)
  #sum(is.na(eval.squares))
  
  #Agrregate the number of crimes captured in each square
  for(i in eval.squares){
    if(!is.na(i))
    {
      estimates$captured.crimes[i] = estimates$captured.crimes[i] + 1
    }
  }  
  
  #get the cummulative sum to plot aggregated propbability for the portion of area covered
  estimates$captured.crimes = cumsum(estimates$captured.crimes)
  
  x.points = estimates$id / nrow(estimates) #proportion of area covered by each square
  y.points = estimates$captured.crimes / nrow(eval) # Aggregated probablity
  
  plot(x.points,y.points, type="l", xlab="% Area Surveilled", ylab = "% Crime Captured", main="Surveillance Plot")
  
  # show random guess
  abline(0,1,lty="dashed")
  
  #idx <- order(x.points)
  auc <- sum(diff(x.points)*rollmean(y.points,2))
  label <- paste('AUC', as.character(round(auc,2)),sep = ' = ')
  text(0.6,0.4,labels = label)
  return(auc)
  #segments(x0 = 0.2, y0 = 0, y1=0.45, col = 'red')
}

Hour[Hour$Hr%%6 == 0,]

