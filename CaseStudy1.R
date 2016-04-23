# Name: Sharath Chand PV
# ID: vp4pa
# Data Mining Case Study1 - Analyzing Crime in Chicago
# -------------------------------------------------------------------
setwd('/Users/homw/Documents/MSDS16/DataMining/CS1')
library(maptools)
library(rgdal)
library(lubridate)
library(ks)
library(OpenStreetMap)
library(scales)

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

#Q1 & 2--- Is theft distributed uniformly across the city? Does it depend on time, day and month?

#--------------- Theft Cases ---------------
theft_data <- read.csv('/Users/homw/Documents/MSDS16/DataMining/CS1/2014_THEFT.csv', header = TRUE)
sort(table(theft_data$Description), decreasing = T)[1:6]
#--- My hypothesis is, small theft (500 to 1000 USD) should be more concentrated in crowded places. downtown malls, metro stations, populated street etc. Big amount theft should be in sparsely populated areas.
#--- About 50% of the cases related to the theft $500 and under, which are small. So the distribution should not be uniform across the city



#--- To understand the distribution, let us plot the locations where theft happened on city boundary map using kernel density estimation
#1000 No. of records for generating kde 
data <- concentration_analysis(theft_data, 'T', 1000)

#--- hourwise histogram - thefts 
hourwise <- table(theft_data$Hour)
barplot(hourwise, xlab = 'Hour', ylab = 'No. of thefts', main = 'Hourly distribution of Thefts', col = 'gray')


#--- monthwise histogram - thefts 
monthwise<-table(data$Month) # only 13% of the average monthly cases in December (vacation time for thieves!?)
barplot(monthwise, xlab = "Months", ylab = "No of Thefts", 
        main = 'Monthwise distribution', col = "gray")

#--- histogram as per Day of the week - thefts 
daywise <- table(data$Weekday) # week days have a bit higher num of theft cases
barplot(daywise, xlab = "Sunday to Saturday", ylab = "No of Thefts", 
        main = 'Daywise distribution', col = "gray")


#Q3------------ Assualt Cases ----------------------
assault_data <- read.csv('/Users/homw/Documents/MSDS16/DataMining/CS1/2014_ASSAULT.csv', header = TRUE)
table(assault_data$Description)
#--- My hypothesis is, assaults should be more concentrated in isolated areas which are scarcely populated. 

#--- To understand the distribution, let us plot the locations where Assaults happened on city boundary map using kernel density estimation

#1000 No. of records for generating kde 
data <- concentration_analysis(assault_data, "A", 1000)
sort(table(data$Description), decreasing = T)[1:6]

#--- hourwise histogram - Assaults 
hourwise <- table(data$Hour)
barplot(hourwise, xlab = 'Hour', ylab = 'No. of Assaults', main = 'Hourly distribution of Assaults', col = 'blue')


#--- monthwise histogram - Assaults
monthwise<-table(data$Month) # only 13% of the average monthly cases in December (vacation time for thieves!?)
barplot(monthwise, xlab = "Months", ylab = "No of Assaults", 
        main = 'Monthwise distribution', col = "blue")

#--- histogram as per Day of the week - Assaults 
daywise <- table(data$Weekday) # week days have a bit higher num of Assault cases
barplot(daywise, xlab = "Sunday to Saturday", ylab = "No of Assaults", 
        main = 'Daywise distribution', col = "blue")


# --------- Functions ----------------------------------------------------

# Function to subset data as required and plot kde estimates for the each subset
concentration_analysis <- function(data, Id, samples){
  data <- data[complete.cases(data),]
  # Step1: Coordinate Ref system of city boundary in meters. Let us convert the location coordinates of our data to the same CRS
  latlong <- data[,c("Longitude", "Latitude")]
  latlong <- project(as.matrix(latlong), proj="+init=epsg:26971")
  
  # Step2: parse the date (ex: 12/04/2014 11:30:00 PM)
  date <- strptime(data$Date, format = "%m/%d/%Y %I:%M:%S %p") 
  #If 'H' is used for hour, AM / PM variations will be lost. 'I' should be used
  
  # Step3: Add Extracted features to the data for further analysis
  data$x <- latlong[,1] # reprojected longitude (E-W)
  data$y <- latlong[,2]
  
  data$Month <- month(date)
  data$Weekday <- wday(date)
  data$Hour <- hour(date)
  data$Date <- format(date)
  
  # Step4: Generate a KDE plot and save as jpeg
  fname = paste(Id,'kde.jpg', sep = '_' )
  jpeg(fname)
  mykde(data, samples)
  dev.off()
  
  #--- Do the concentrations look different depending on the time of day, day of week, and month of year?
  # 1 - sunday, 1 - January, 1 - 1AM
  
  #--- Time of the day ~ Variation in concentration of crimes
  data$Hour[data$Hour == 0] <- 24
  EM <- filter_hour(data, 1, 6) #Early morning hours
  BN <- filter_hour(data, 7, 12) #Before Noon hours
  AN <- filter_hour(data, 13, 18) #After Noon hours
  NT <- filter_hour(data, 19, 24) #Night hours
  
  #--- #Early morning hours - Concentration of crimes
  fname = paste(Id,'earlyHours.jpg', sep = '_' )
  jpeg(fname)
  mykde(EM, samples)
  dev.off()
  
  #--- #Morning hours - Concentration of crimes
  fname = paste(Id,'beforeNoon.jpg', sep = '_' )
  jpeg(fname)
  mykde(BN, samples)
  dev.off()
  
  #--- #Afternoon hours - Concentration of crimes
  fname = paste(Id,'afterNoon.jpg', sep = '_' )
  jpeg(fname)
  mykde(AN, samples)
  dev.off()
  
  #--- #Night hours - Concentration of crimes
  fname = paste(Id,'nightHours.jpg', sep = '_' )
  jpeg(fname)
  mykde(NT, samples)
  dev.off()
  
  #--- Month wise ~ Variation in concentration of crimes
 
  i <- 1
  for (i in 1:12){
    data1 <- data[data$Month == i,]
    fname <- paste(Id,"month", i, ".jpg",sep = '_')
    jpeg(fname)
    mykde(data1, samples)
    dev.off()
  }
  
  #--- Day of the week ~ Variation in concentration of crimes
  i <- 1
  for (i in 1:7){
    data1 <- data[data$Month == i,]
    fname <- paste(Id,"weekday", i, ".jpg", sep = '_')
    jpeg(fname)
    mykde(data1, samples)
    dev.off()
  }
  
  return(data)
  
}


# to filter the data as per the hour range
filter_hour <- function(data,min, max){return(data[(data$Hour>=min & data$Hour<=max),])}

#KDE Estimation and plotting
mykde <- function(data,n){
  resolution = 200
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
  
#   mp <- openmap(c(42.05, -87.96), c(41.62, -87.5), type="osm", minNumTiles=16)
#   mp <- openproj(mp, projection = "+init=epsg:26971")
#   plot(mp)
  
  
  #par(bg = 'transparent')
  x_sorted = sort(unique(axis[,1]))
  y_sorted = sort(unique(axis[,2]))
  image(x = x_sorted, 
        y = y_sorted,
        z = matrix(est, ncol=length(y_sorted), byrow=FALSE),
        col = colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),
        xlab="West-to-East (M)", ylab="South-to-North (M)", asp=1)
  
  # add city boundary to KDE plot for interpretability
  plot(city.boundary, add=TRUE)
  
}

#-------------- Extra code: To plot the kde on Chicago street map -----------
mp <- openmap(c(42.05, -87.96), c(41.62, -87.5), type="osm", minNumTiles=16)
mp <- openproj(mp, projection = "+init=epsg:26971")
plot(mp)
plot(city.boundary, add= T)

#to plot kde for resolution 1000
jpeg("A_kde_Res1000.jpg")
mykde(data, 1000)
dev.off()

