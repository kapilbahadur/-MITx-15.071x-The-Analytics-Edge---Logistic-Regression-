#Logistic Regression: framingham Study
#26/5/2015

#Loading Data set
framingham <- read.csv("D:/Data Science/MIT Analytics Edge Course/
        Unit 3 Logistic Regression/Framingham Heart Study/framingham.csv")
str(framingham)

#Installing Libraries
library(caTools)

#Splitting Dataset into trainig and testing
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == FALSE)

#Making Model

framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

#Making Predictions
PredictTest <- predict(framinghamLog, newdata = test, type = "response")
summary(PredictTest)

table(test$TenYearCHD, PredictTest > 0.4)

#Accuracy of baseline Model
(1069+6)/(1069+187+6+11)

#Calculating AUC of ROC Curves
library(ROCR)

ROCRpred = prediction(PredictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred,"auc")@y.values)

