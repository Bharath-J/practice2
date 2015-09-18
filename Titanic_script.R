#Set working directory
setwd("C:\\Users\\Bharath\\Desktop\\Titanic")

#Read training data
trainSet <- read.table("train.csv", sep = ",", header = TRUE)

#Read test data
testSet <- read.table("test.csv", sep=",", header = T)

#Crosstabs between survived and passenger class
table(trainSet[c("Survived", "Pclass")])

library(caret)
library(fields)
library(randomForest)
library(e1071)
bplot.xy(trainSet$Survived, trainSet$Age)
bplot.xy(trainSet$Survived, trainSet$Fare)

#Convert survived to factor
trainSet$Survived <- factor(trainSet$Survived)
set.seed(42)

#Train model using random forest algorithm
model <- train(Survived ~ Pclass + Sex + SibSp + 
                 Embarked + Parch + Fare,
               
               data = trainSet,
               method = "rf",
               trControl = trainControl(method = "cv",
                                        number = 5)
               )


summary(testSet)
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = T), testSet$Fare)
testSet$Survived <- predict(model, newdata=testSet)
submission <- testSet[c("PassengerId","Survived")]
write.table(submission, file = "submission.csv", col.names = T, row.names = F, sep = "," )