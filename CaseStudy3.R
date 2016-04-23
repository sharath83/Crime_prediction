# Name: Sharath Chand PV
# ID: vp4pa
# Date: 10-10-2015
# Data Mining Case Study3 - Linear Crime Models
# -------------------------------------------------------------------
setwd('/Users/homw/Documents/MSDS16/DataMining/CS3/')
source("/Users/homw/Documents/MSDS16/DataMining//Examples-2/CrimeUtil.R")

#Create Chicago city boundary from its shapefile
city.boundary = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")
# set prediction resolution
prediction.resolution.meters = 200

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
base <- data_full[data_full$Month == 4, ] # Predictors to buld LR model
train <- data_full[data_full$Month == 5, ] # Responses to train the model
test <- data_full[data_full$Month == 6, ] #Validation on ground thruths

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

#proximity to landmarks 
land.points = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/Landmarks/LandmarksNationalRegister_nov2012", "poly", "+init=epsg:3435", "+init=epsg:26971")
polys <- slot(land.points,"polygons")
points <- as.data.frame(matrix(0,nrow = length(polys),ncol = 2))
names(points) <- c('coord.x', 'coord.y')
for (i in 1:length(polys)) {
  #print(paste("Polygon #",i))
  t <- (slot(slot(polys[[i]],"Polygons")[[1]],"coords"  ))
  points[i,"coord.x"] <- mean(t[,1])
  points[i,"coord.y"] <- mean(t[,2])
}
land.points <- points
Land.min.distance = get.min.distances(train_full[,c("x","y")], land.points)

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
train_full <- cbind(train_full, crime.density, Park.min.distance, PS.min.distance, Rail.min.distance, Land.min.distance)
# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit = glm(response ~ . -x -y, data = train_full, family=binomial)
summary(glm.fit)
#--------------------------------------------------
#Predict crime using the above classification model
# Create test data
prediction.points = get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
crime.density = run.spatial.kde(train[,c("x","y")], prediction.points, max.sample.size=1000)
Park.min.distance = get.min.distances(prediction.points, park.points)
PS.min.distance = get.min.distances(prediction.points, PoliceStn.points)
Rail.min.distance = get.min.distances(prediction.points, CTA_Rail.points)
Land.min.distance = get.min.distances(prediction.points, land.points)
# Build test df
test_full <- as.data.frame(cbind(prediction.points, crime.density,Park.min.distance,PS.min.distance,Rail.min.distance, Land.min.distance))

#Prediction
threat <- predict(glm.fit, newdata = test_full, type = 'response')
#Buld prediction data frame for evaluation
prediction.theft<- cbind(prediction.points, threat)
names(prediction.theft) <- c('x','y','threat')

#prediction.theft <- predict.crime(train)
# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test[,c("x","y")], prediction.resolution.meters, city.boundary)

#------------------------------------------------------------
# predict.crime <- function(train){
#   return(prediction.theft)
# }
#------------------------------------------------------------

