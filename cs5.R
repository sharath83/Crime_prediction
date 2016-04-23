# Name: Sharath Chand PV
# ID: vp4pa
# Date: 10-10-2015
# Data Mining Case Study3 - Linear Crime Models
# -------------------------------------------------------------------
setwd('/Users/homw/Documents/MSDS16/DataMining/Examples/CS5/')
library(maptools)
library(rgdal)
library(lubridate)
library(ks)
library(OpenStreetMap)
library(scales)
library(RColorBrewer)
library(tm)
library(topicmodels)

source("/Users/homw/Documents/MSDS16/DataMining/Examples/CrimeUtil.R")

# -------------------------
# Functions
# -------------------------
# Calculates cosine similarity between a and b
cos.sim=function(a, b){
  return(sum(a*b)/sqrt(sum(a^2))*sqrt(sum(b^2)))
}

# Builds topic model for a given a vector of text documents, 
getTopics <- function(tweets, t){
  #Transform tweets into a corpus
  corpus <- as.data.frame(tweets[,'text'])
  #corpus = as.data.frame(gsub('chicago','',corpus))
  corpus <- VCorpus(DataframeSource(corpus))
  
  # clean and compute tfidf
  
  corpus.clean = tm_map(corpus, stripWhitespace)                # remove extra whitespace
  corpus.clean = tm_map(corpus.clean, removeNumbers)                    # remove numbers
  corpus.clean = tm_map(corpus.clean, removePunctuation)              # remove punctuation
  corpus.clean = tm_map(corpus.clean, content_transformer(tolower))       # ignore case
  corpus.clean = tm_map(corpus.clean, removeWords, stopwords("english"))# remove stop words
  corpus.clean = tm_map(corpus.clean, stemDocument)                       # stem all words
  corpus.clean.tf = DocumentTermMatrix(corpus.clean, control = list(weighting = weightTf))
  
  #inspect(corpus.clean[1:2])
  #Remove empty documents
  row.sum <- apply(corpus.clean.tf,1, "sum")
  corpus.clean.tf <- corpus.clean.tf[row.sum>0,]
  corpus.clean <- corpus.clean[row.sum>0]
  #tweets_full <- tweets_full[row.sum>0,]
  
  # train topic model using LDA
  topic.model = LDA(corpus.clean.tf, t)
  return(topic.model)
}

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

# ------------- Main Code -------
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

# Are tweets distributed uniformly across Chicago? 
tweets = read.csv("/Users/homw/Documents/MSDS16/DataMining/Examples/CS5/tweets_large_sample.csv", header = TRUE, stringsAsFactors = FALSE)
tweets$timestamp[1:10] # format is "2012-12-11 13:35:59-05"
tweets$timestamp <- strptime(tweets$timestamp, "%Y-%m-%d %H:%M:%S")
# Reproject longitude and latitudes
# Coordinate Ref system of city boundary in meters. Let us convert the location coordinates of our data to the same CRS
latlong <- tweets[,c("longitude", "latitude")]
latlong <- project(as.matrix(latlong), proj="+init=epsg:26971")
tweets$x <- latlong[,1]
tweets$y <- latlong[,2]
# Keep only finite location tweets
tweets <- tweets[is.finite(tweets$x) & is.finite(tweets$y),]
# Keep tweets that are within Chicago
in.chicago = points.within.boundary(tweets[,c("x", "y")], "+init=epsg:26971", city.boundary)
tweets <- tweets[in.chicago,]
mykde(tweets, 1000) # To understand spatial distribution of tweets

# Topic Modelling on tweets
tweets$year <- year(tweets$timestamp)
tweets$month <- month(tweets$timestamp)
tweets$day <- weekdays(tweets$timestamp)
tweets$hour <- hour(tweets$timestamp)

tweets$day <- factor(tweets$day, levels=c('Monday', 'Tuesday', 'Wednesday',
                                  'Thursday','Friday','Saturday',
                                  'Sunday'))

unique(tweets$year)
table(tweets$day)
barplot(table(tweets$day))
summary(tweets$timestamp)

# Split into weekend and weekday tweets
tweets.weekend <- tweets[tweets$day %in% c("Sunday","Saturday"),] # Weekends
tweets.weekday <- tweets[!(tweets$day %in% c("Sunday","Saturday")),] # Weekdays

set.seed(102)
#Get topics of Weekends
topics.we <- getTopics(tweets.weekend, 100)
sum(as.integer(topics.we@documents))
terms(topics.we,10)

#Get topics of Weekdays
#sum(is.na(tweets.weekday$text))
tweets.weekday <- tweets.weekday[sample(nrow(tweets.weekday),25000),]
topics.wd <- getTopics(tweets.weekday, 100)
sum(as.integer(topics.wd@documents))
terms(topics.wd,10)


#Check how many twets are within our RoI (region of interest)
# Farmers Markets
# Get tweets originated with in 50 mtrs radius around Farmer's Market
FarmersMarket.points = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/farmers_markets_2012/farmers_markets_2012", "points", "+init=epsg:3435", "+init=epsg:26971")@coords

FarmersMarket.points <- as.data.frame(FarmersMarket.points)
names(FarmersMarket.points) <- c('xc','yc')
tweets.fm <- apply(FarmersMarket.points, 1, function(p){tweets[(tweets$x-p['xc'])^2+(tweets$y-p['yc'])^2 < 50^2,'text']})
tweets.fm<- as.data.frame(unlist(tweets.fm))
names(tweets.fm) <- 'text'
#Topics discussed near Farmers Markets
topics.fm <- getTopics(tweets.fm, 10)
sum(as.integer(topics.fm@documents))
terms(topics.fm,20)

# Hospitals
# Get tweets originated with in 50 mtrs radius around Farmer's Market
Hospital.points = read.shapefile("/Users/homw/Documents/MSDS16/DataMining/CS3/Data/Hospitals/Hospitals", "points", "+init=epsg:3435", "+init=epsg:26971")@coords

Hospital.points <- as.data.frame(Hospital.points)
names(Hospital.points) <- c('xc','yc')
tweets.hos <- apply(Hospital.points, 1, function(p){tweets[(tweets$x-p['xc'])^2+(tweets$y-p['yc'])^2 < 50^2,'text']})
tweets.hos<- as.data.frame(unlist(tweets.hos))
names(tweets.hos) <- 'text'
#Topics discussed near Hospitals
topics.hos <- getTopics(tweets.hos, 8)
sum(as.integer(topics.hos@documents))
terms(topics.hos,20)


# Bonus Problem
# Hypothesis is topics discussed 1 month prior to a given day have correlation with the occurrence of theft.
#27th Oct 2012 to 15th April 2014 - Tweets availability
# Consider crimes occurred on 1st March 2014 for training a model
# All the tweets originated in the 1 month prior to 1st Mar 2014 are considered for topic modeling
# predict on 1st Apr 2014 - Predictors are the topics derived from the tweets originated in the 1 month prior to 1st Apr 2014

set.seed(102)
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
data_full$Day <- day(date)
# ****************************************************************
data_full <- data_full[complete.cases(data_full),]
base <- data_full[data_full$Month == 2, ] # August 2013 data
train <- data_full[data_full$Month == 3 & data_full$Day == 1, ] # Responses to train the model
test_ground <- data_full[data_full$Month == 4 & data_full$Day == 1, ] #Validation on ground thruths of 1st,Apr 2014
# We need tweets history of March as predictors for that

# set prediction resolution
prediction.resolution.meters = 1000
# get negative observations within chicago 
non.crime.points = cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] = "response"

# get positive observations from train dataset within chicago
training.crime.points = cbind(1, train[,c("x","y")])
names(training.crime.points)[1] = "response"

#prepare a train data - combining positive + negative obs
train_full <- rbind(training.crime.points,non.crime.points)

#get tweets one month prior to train date
tweets.train <- tweets[tweets$year == 2014 & tweets$month == 2,]

# Create a equally spaced grid across chicago
# resolution = 1000
# range <- bbox(city.boundary) #Get the boundary range
# x_axis <- seq(range[1],range[3], resolution) #Set x-axis range
# y_axis <- seq(range[2],range[4], resolution) #Set y-axis range
# axis <- expand.grid(x_axis, y_axis) #
# names(axis) = c("x", "y")
# 
# tweets.doc <- apply(axis,1, tweets.square)
# tweets.square <- function(point){
#   return(tweets.train[!( tweets.train$x < point['x']-500 |
#                     tweets.train$x > point['x']+500 |
#                     tweets.train$y < point['y']-500 |
#                     tweets.train$y > point['y']+500),'text'])
# }

#c(1,10^6) < train_full[1,c('x','y')]+1000

# Get all the tweets with in 500 meters radius of training point.
tweets.trdoc <- apply(train_full, 1, function(p){tweets.train[(tweets.train$x-p['x'])^2 +(tweets.train$y-p['y'])^2 < 500^2,'text']})
tweets.trdoc[2]
#tweets.trdoc<- as.data.frame(unlist(tweets.trdoc))

train_full$text <- tweets.trdoc
train_full$text <- sapply(train_full$text, function(p){paste(unlist(p, recursive = T), collapse = " ")})
train_full$text <- trimws(train_full$text)
train_full <- train_full[train_full$text!='',]
train_full$text <- sapply(train_full$text, function(p){gsub('chicago','',p, ignore.case = TRUE)})
train_full <- train_full[nchar(train_full$text)>10,]
#Topics discussed in train period
set.seed(1000)
topics = 20
topics.train <- getTopics(train_full, topics)
sum(as.integer(topics.train@documents))
t <- terms(topics.train,10)
p <- topics(topics.train)
# get document probabilities
trdoc.probabilities = topics.train@gamma

# Create train data for model building
train.model <- train_full
trdoc.probabilities <- as.data.frame(trdoc.probabilities)
train.model <- cbind(train.model,trdoc.probabilities)
#train.model[1,54]
# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit = glm(response ~ ., data = train.model[,-c(2:4,ncol(train.model))], family=binomial)
summary(glm.fit)

##Predict crime using the above classification model
# Create test data
prediction.points = get.grid.points(city.boundary, prediction.resolution.meters, TRUE)

#get tweets around prediction points
#get tweets one month prior to test date - Apr1st 2014
tweets.test <- tweets[tweets$year == 2014 & tweets$month == 3,]
tweets.tsdoc <- apply(prediction.points, 1, function(p){tweets.test[(tweets.test$x-p['x'])^2 +(tweets.test$y-p['y'])^2 < 500^2,'text']})

prediction.points$text <- tweets.tsdoc
prediction.points$text <- sapply(prediction.points$text, function(p){paste(unlist(p, recursive = T), collapse = " ")})
prediction.points$text <- trimws(prediction.points$text)
prediction.points <- prediction.points[prediction.points$text!='',]
prediction.points$text <- sapply(prediction.points$text, function(p){gsub('chicago','',p, ignore.case = TRUE)})
prediction.points <- prediction.points[nchar(prediction.points$text)>15,]
#Topics discussed in train period - Apply LDA
topics.test <- getTopics(prediction.points, topics)
sum(as.integer(topics.test@documents))
terms(topics.test,10)


# get document probabilities
tsdoc.probabilities = topics.test@gamma
tsdoc.probabilities <- as.data.frame(tsdoc.probabilities)

#Arrange topics in test as per the train topics
# In LDA topics assignment is done arbitrarily, so rearrange the test topics according to train topic sequence using cosine similarity
tr <- terms(topics.train,20)
ts <- terms(topics.test,20)

tr <- apply(tr,2,function(p){paste(p,collapse = ' ')})
tr1 <- VCorpus(DataframeSource(as.data.frame(tr)))
tr.dtm <- DocumentTermMatrix(tr1, control = list(weighting = weightTfIdf))

ts <- apply(ts,2,function(p){paste(p,collapse = ' ')})
ts1 <- VCorpus(DataframeSource(as.data.frame(ts)))
ts.dtm <- DocumentTermMatrix(ts1, control = list(weighting = weightTfIdf))
#cos.sim(ts.dtm[1,]$v, tr.dtm[1,]$v)

ts.new <- ts
ts.topics <- tsdoc.probabilities
maxi <- 1:20
for (itr in 2:topics){
  for (its in 1:topics){
    maxi[its] <- cos.sim(ts.dtm[its,]$v, tr.dtm[itr,]$v)
  }
  which.max(maxi)
  ts.topics[,itr] <- tsdoc.probabilities[,which.max(maxi)]
  ts.new[,itr] <- ts[,which.max(maxi)]
}

prediction.points <- cbind(prediction.points,ts.topics)

#Prediction
threat <- predict(glm.fit, newdata = prediction.points[,-c(1:3,ncol(prediction.points))], type = 'response')
#Buld prediction data frame for evaluation
prediction.theft<- cbind(prediction.points[,c('x','y')], threat)
names(prediction.theft) <- c('x','y','threat')

# evaluate prediction on ground-truth crime records from test
auc <- plot.surveillance.curve(prediction.theft, test_ground[,c("x","y")], prediction.resolution.meters, city.boundary)



