#Kunal Agarwal Analytics Edge Course
#28/5/2015 , UNIT 3: Recitation (Logistic Regression)
#Polling Problem

#Loading the DataSet
polling <- read.csv("PollingData.csv") 

#Inspecting the Dataset
str(polling)
summary(polling)
cor(polling$Rasmussen, polling$SurveyUSA)

#Installing Packages and Libraries
install.packages("mice")
library(mice)

#Imputing the Missing Values
simple <- polling[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR")]
summary(simple)

set.seed(144)
imputed <- complete(mice(simple))

summary(imputed)

#Copying Imputed Values to Original DataSet
polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA

summary(polling)

# Creating Training (2004 and 2008) and Testing DataSets (2012)
train <- subset(polling, Year == 2004 | Year == 2008)
test <- subset(polling, Year == 2012)

str(test)

#Inpecting Baseline Model
table(train$Republican)

#Baseline Accuracy = 53/100 =0.53 #Where we predicted all repbuclians to win 47 Mistakes

#Smart Baseline Model using sign function and Ramussen Variable
smartbaseline <-  sign(train$Rasmussen)

table(smartbaseline)

#Comparing Against Actual Mistakes
table(train$Republican, smartbaseline) #Smart Baseline is making just 4 mistakes

str(train)
cor(train[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR", "Republican")])

#Making Model with PropR as independent variable
RegProp <- glm(Republican ~ PropR, data = train, family = binomial)

summary(RegProp)

table(train$PropR)

#Making Predictions on training set
pred1 <- predict(RegProp, train, type = "response")

table(train$Republican, pred1 > 0.5) #Making 4 mistakes same as smart baseline model

#Making Model with 2 variables which are least corelated
mod2 <- glm(Republican ~ DiffCount+Rasmussen, data = train, family = "binomial")

pred2 <- predict(mod2, train, type = "response")

table(train$Republican, pred2 > 0.5) #making 3 mistakes
summary(mod2)

##Using test data set
#Checking accuracy of smart baseline on test set

table(test$Republican, sign(test$Rasmussen)) #Making 4 mistakes

#Making Prediction using model 2(of 2 variables)
Testprediction <- predict(mod2, newdata = test, type = "response")
table(test$Republican, Testprediction > 0.5) #Making 1 mistake

#Inspecting Mistake
subset(test, Testprediction >0.5 & Republican == 0)
#Can't blame the model considering the variables


