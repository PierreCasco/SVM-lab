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
g1 <- ggplot(aq.test,aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=(abs(aq.test$Ozone - svmPred)), colour = (abs(aq.test$Ozone - svmPred)))) 

#Build model using SVM in the e1071 package
library('e1071')
model2 <- svm(Ozone ~ Solar.R + Temp, data = aq.train)

#Test model2 on the testing dataset and compute RMSE
svmPred2 <- predict(model2, aq.test, type = "votes")
RMSE2 <- sqrt(mean(aq.test$Ozone - svmPred2)^2)

#Plot
g2 <- ggplot(aq.test,aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=(abs(aq.test$Ozone - svmPred2)), colour = (abs(aq.test$Ozone - svmPred2)))) 

#Build model using LM
model3 <- lm(formula = Ozone ~ Temp + Wind, data = aq.train)
summary(model3)

#Test model2 on the testing dataset and compute RMSE
svmPred3 <- predict(model3, aq.test, type = "response")
RMSE3 <- sqrt(mean(aq.test$Ozone - svmPred3)^2)

#Plot
g3 <- ggplot(aq.test,aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=(abs(aq.test$Ozone - svmPred3)), colour = (abs(aq.test$Ozone - svmPred3)))) 

#Plot all 3 charts
library('gridExtra')
grid.arrange(g1,g2,g3)

#Create good Ozone variable, 0 if < average, 1 if >= average
goodCutoff = mean(aq$Ozone)

aq2 <- aq
aq2$goodOzone <- ifelse(aq2$Ozone >= goodCutoff, 1, 0)

#Prepare train and test data sets with new data set
aq2.train <- aq2[randIndex[1:cutoff],]
aq2.test <- aq2[randIndex[(cutoff+1):numrows],]
  
#Build model using KSVM with new good Ozone data
model4 <- ksvm(goodOzone ~ Solar.R + Temp, data = aq2.train)

#Test the model on the testing dataset
svmPred4 <- predict(model4, aq2.test, type="votes")

#Plot
g4 <- ggplot(aq2.test,aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=(abs(aq2.test$Ozone - svmPred4)), colour = goodOzone)) 

#Build model using SVM with new good Ozone data
model5 <- svm(goodOzone ~ Solar.R + Temp, data = aq2.train)

#Test the model on the testing dataset
svmPred5 <- predict(model5, aq2.test, type="votes")

#Plot
g5 <- ggplot(aq2.test,aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=(abs(aq2.test$Ozone - svmPred5)), colour = goodOzone)) 

#Build model using Naive Bayes with new good Ozone data
model6 <- naiveBayes(goodOzone ~ Solar.R + Temp, data = aq2.train)

#Test the model on the testing dataset
svmPred6 <- predict(model6, aq2.test)

#Plot
g6 <- ggplot(aq2.test,aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=(abs(aq2.test$Ozone - svmPred5)), colour = goodOzone)) 

#Plot all 3 charts
grid.arrange(g4,g5,g6)
