# Name: Sharath Chand PV
# ID: vp4pa
# Date: 10-23-2015
# Data Mining Case Study4 - Non-Linear Crime Models
# -------------------------------------------------------------------
setwd('/Users/homw/Documents/MSDS16/DataMining/CS3/')
library(e1071)
library(kernlab)

source("/Users/homw/Documents/MSDS16/DataMining//Examples/CrimeUtil.R")
#Create Chicago city boundary from its shapefile
city.boundary = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")
# set prediction resolution
prediction.resolution.meters = 600

#----------------------------------------
# considering Theft cases to build classification models
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
data_full <- data_full[complete.cases(data_full),]
base <- data_full[data_full$Month == 3, ] # Predictors to buld LR model
train <- data_full[data_full$Month == 4, ] # Responses to train the model
test <- data_full[data_full$Month == 5, ] #Validation on ground thruths

#Predict the theft
# get negative observations within chicago 
non.crime.points = cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] = "response"
plot(non.crime.points[,c("x","y")], pch=".", asp=1)

# get positive observations from train dataset within chicago
training.crime.points = cbind(1, train[,c("x","y")])
names(training.crime.points)[1] = "response"
plot(training.crime.points[,c("x","y")], pch=".", asp=1)

#prepare a train data - combining positive + negative obs
train_full <- rbind(training.crime.points,non.crime.points)
# Our train data should have x,y coordinates, kde estimate, hypothesized drivers of crime, actual crime status
# Get kde estimate for the train_full data (Positive + negative points) from base data
crime.density <- run.spatial.kde(base[,c('x','y')], train_full[,c('x','y')], 1000)

# hypothesized crime drivers 
#proximity to CTA Bus stops, police stn, parks
#   CTA_Bus.points = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/CTA_BusStops/CTA_BusStops", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
#   Bus.min.distance = get.min.distances(train_full[,c("x","y")], CTA_Bus.points)

#It is hypothesized that locations close to Metra stations to have higher theft risk and the risk decreases non-linearly for the locations as we move farther from metra stations 
#proximity to Metra stations 
street.points = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/Major_20Streets/Major_Streets", "lines", "+init=epsg:3435", "+init=epsg:26971")
lines <- slot(street.points, "lines")
points <- as.data.frame(matrix(0,nrow = length(lines),ncol = 2))
names(points) <- c('coord.x', 'coord.y')
for (i in 1:length(lines)) {
  #print(paste("Polygon #",i))
  t <- (slot(slot(lines[[i]],"Lines")[[1]],"coords"  ))
  points[i,"coord.x"] <- mean(t[,1])
  points[i,"coord.y"] <- mean(t[,2])
}

street.points <- points[sample(nrow(points), 1000),]
Street.min.distance = get.min.distances(train_full[,c("x","y")], street.points)

#proximity to parks
park.points = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/Parks_Aug2012/Parks_Aug2012", "poly", "+init=epsg:3435", "+init=epsg:26971")
polys <- slot(park.points,"polygons")
points <- as.data.frame(matrix(0,nrow = length(polys),ncol = 2))
names(points) <- c('coord.x', 'coord.y')
for (i in 1:length(polys)) {
  #print(paste("Polygon #",i))
  t <- (slot(slot(polys[[i]],"Polygons")[[1]],"coords"  ))
  points[i,"coord.x"] <- mean(t[,1])
  points[i,"coord.y"] <- mean(t[,2])
}
park.points <- points
Park.min.distance = get.min.distances(train_full[,c("x","y")], park.points)

#proximity to police stations
PoliceStn.points = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/PoliceStations/PoliceStationsDec2012", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
PS.min.distance = get.min.distances(train_full[,c("x","y")], PoliceStn.points)

#Proximity to railways stations
CTA_Rail.points = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/CTA_Stations/CTA_Stations", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
Rail.min.distance = get.min.distances(train_full[,c("x","y")], CTA_Rail.points)

#Add crime density, crime driveres to train data
train_full <- cbind(train_full, crime.density, Park.min.distance, PS.min.distance, Rail.min.distance, Street.min.distance)
#--------------------------------------------------
#Predict crime using the above classification model
# Create test data
prediction.points = get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
crime.density = run.spatial.kde(train[,c("x","y")], prediction.points, max.sample.size=1000)
Park.min.distance = get.min.distances(prediction.points, park.points)
PS.min.distance = get.min.distances(prediction.points, PoliceStn.points)
Rail.min.distance = get.min.distances(prediction.points, CTA_Rail.points)
Street.min.distance = get.min.distances(prediction.points, street.points)
# Build test df
test_full <- as.data.frame(cbind(prediction.points, crime.density,Park.min.distance,PS.min.distance,Rail.min.distance, Street.min.distance))

#Spatial alone
# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit = glm(response ~ ., data = train_full[,c(1,4:8)], family=binomial)
summary(glm.fit)

#Prediction
threat.glm <- predict(glm.fit, newdata = as.data.frame(test_full[,c(3:7)]), type = 'response')
# threat.glm <- ifelse(threat.glm >= 0.5, "1", "0")
# table(threat.glm)
#Buld prediction data frame for evaluation
prediction.theft<- cbind(prediction.points, threat.glm)
names(prediction.theft) <- c('x','y','threat')

#prediction.theft <- predict.crime(train)
# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test[,c("x","y")], prediction.resolution.meters, city.boundary)

#Predictors - Only spatial factors
#Build SVM classification model - linear kernel
start <- proc.time()
svm.e1 <- svm(response ~ ., data = train_full[,c(1,5:8)], type="C-classification", kernel="linear", cost=50, probability = TRUE)
(proc.time() - start)[3]
summary(svm.e1)
threat.svm <- predict(svm.e1, newdata = test_full[,c(4:7)], probability = TRUE)
threat.svm <- attr(threat.svm, "probabilities")
prediction.theft<- cbind(prediction.points, threat.svm[,1])
names(prediction.theft) <- c('x','y','threat')
# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test[,c("x","y")], prediction.resolution.meters, city.boundary)

# Polynomial kernel
start <- proc.time()
svm.e1 <- svm(response ~ ., data = train_full[,c(1,5:8)], type="C-classification", kernel="polynomial", degree = 2, cost=50, probability = TRUE)
(proc.time() - start)[3]
summary(svm.e1)
threat.svm <- predict(svm.e1, newdata = test_full[,c(4:7)], probability = TRUE)
threat.svm <- attr(threat.svm, "probabilities")
prediction.theft<- cbind(prediction.points, threat.svm[,1])
names(prediction.theft) <- c('x','y','threat')
#prediction.theft <- predict.crime(train)
# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test[,c("x","y")], prediction.resolution.meters, city.boundary)

# Radial basis kernel
start <- proc.time()
svm.e1 <- svm(response ~ ., data = train_full[,c(1,5:8)], type="C-classification", kernel="radial", cost=10, probability = TRUE)
(proc.time() - start)[3]
summary(svm.e1)
threat.svm <- predict(svm.e1, newdata = test_full[,c(4:7)], probability = TRUE)
threat.svm <- attr(threat.svm, "probabilities")
prediction.theft<- cbind(prediction.points, threat.svm[,1])
names(prediction.theft) <- c('x','y','threat')
# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test[,c("x","y")], prediction.resolution.meters, city.boundary)


#------------------------------------------------------------
# predict.crime <- function(train){
#   return(prediction.theft)
# }
#------------------------------------------------------------
#Predictors - KDE + spatial factors
#Build SVM classification model - linear kernel
start <- proc.time()
svm.e1 <- svm(response ~ ., data = train_full[,c(1,4:8)], type="C-classification", kernel="linear", cost=50, probability = TRUE)
(proc.time() - start)[3]
summary(svm.e1)
threat.svm <- predict(svm.e1, newdata = test_full[,c(3:7)], probability = TRUE)
threat.svm <- attr(threat.svm, "probabilities")
prediction.theft<- cbind(prediction.points, threat.svm[,1])
names(prediction.theft) <- c('x','y','threat')
# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test[,c("x","y")], prediction.resolution.meters, city.boundary)

# Polynomial kernel
start <- proc.time()
svm.e1 <- svm(response ~ ., data = train_full[,c(1,4:8)], type="C-classification", kernel="polynomial", degree = 2, cost=50, probability = TRUE)
(proc.time() - start)[3]
summary(svm.e1)
threat.svm <- predict(svm.e1, newdata = test_full[,c(3:7)], probability = TRUE)
threat.svm <- attr(threat.svm, "probabilities")
prediction.theft<- cbind(prediction.points, threat.svm[,1])
names(prediction.theft) <- c('x','y','threat')
#prediction.theft <- predict.crime(train)
# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test[,c("x","y")], prediction.resolution.meters, city.boundary)

# Radial basis kernel
start <- proc.time()
svm.e1 <- svm(response ~ ., data = train_full[,c(1,4:8)], type="C-classification", kernel="radial", cost=50, probability = TRUE)
(proc.time() - start)[3]
summary(svm.e1)
threat.svm <- predict(svm.e1, newdata = test_full[,c(3:7)], probability = TRUE)
threat.svm <- attr(threat.svm, "probabilities")
prediction.theft<- cbind(prediction.points, threat.svm[,1])
names(prediction.theft) <- c('x','y','threat')
# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test[,c("x","y")], prediction.resolution.meters, city.boundary)

plot(train_full$Street.min.distance, train_full$crime.density, col=c("blue","red")[train_full$response+1], xlab='Min distance Major streets (mtrs)', ylab = 'KDE Est', main = 'Distribution crime - April')

plot(train_full$Rail.min.distance, train_full$crime.density, col=c("blue","red")[train_full$response+1], xlab='Min distance rail stns (mtrs)', ylab = 'KDE est', main = 'Distribution crime - April')

