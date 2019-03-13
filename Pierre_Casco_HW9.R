library('arules')
library('kernlab')

#Load the air quality dataset
aq <- airquality
aq[is.na(aq)] <- 0

#Study data set
str(aq)

#Prepare train and test data sets
numrows <- nrow(aq)
cutoff <- (numrows/3*2)
randIndex <- sample(1:numrows[1])

aq.train <- aq[randIndex[1:cutoff],]
aq.test <- aq[randIndex[(cutoff+1):numrows],]

#Build model using KSVM
model <- ksvm(Ozone ~ Solar.R + Temp, data = aq.train)

#Test the model on the testing dataset and compute RMSE
svmPred <- predict(model, aq.test, type="votes")
RMSE <- sqrt(mean(aq.test$Ozone - svmPred)^2)

#Plot
ggplot(aq.test,aes(x=Temp,y=Wind)) + 
  geom_point(aes(),size=(abs(aq.test$Ozone - svmPred)), colour = (abs(aq.test$Ozone - svmPred))) + 
  scale_colour_manual("Ozone",values = abs(svmPred - aq.test$Ozone)) + scale_fill_manual(values = abs(svmPred - aq.test$Ozone))

