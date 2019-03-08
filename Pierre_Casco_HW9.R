library('arules')
library('kernlab')

#Load the air quality dataset
aq <- airquality
aq[is.na(aq)] <- 0

#Study data set
str(aq)

#Prepare train and test data sets
numrows <- nrow(aqt)
cutoff <- (numrows/3*2)
randIndex <- sample(1:numrows[1])

aq.train <- aq[randIndex[1:cutoff],]
aq.test <- aq[randIndex[(cutoff+1):numrows],]

#Build model using KSVM
model <- ksvm(Ozone ~ Solar.R + Wind + Temp, data = aq.train, kernel = "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
