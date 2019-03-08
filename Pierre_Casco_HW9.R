library('arules')
library('kernlab')
library('dplyr')

#Load the air quality dataset
aq <- na.omit(airquality)

#Study data set
str(aq)

#Prepare train and test data sets
numrows <- nrow(aq)
cutoff <- (numrows/3*2)
randIndex <- sample(1:numrows[1])

aq.train <- aq[randIndex[1:cutoff],]
aq.test <- aq[randIndex[(cutoff+1):numrows],]
