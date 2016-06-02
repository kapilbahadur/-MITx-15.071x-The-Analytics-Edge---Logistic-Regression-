#Kunal Agarwal 28/5/2016 
#Analytics Edge #Uni3 Logistic Regression
#Parole problem

#Loading DataSets
parole <- read.csv("parole.csv")
str(parole)

table(parole$violator)

#Conerting into factor
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

str(parole)

#Splitting into training and testing dataset
library(caTools)

set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)

train <- subset(parole, split ==TRUE)
test <- subset(parole, split == FALSE)

mod <- glm(violator ~ ., data = train, family = "binomial")
summary(mod)

table(parole$multiple.offenses)

##Making Predictions
modpredict <- predict(mod, newdata = test, type = "response")
table(test$violator, modpredict >= 0.5)

max(modpredict)

table(test$violator)

#Accuracy of model
(167+12)/202 #0.8861

#Acurracy of baseline model
179/202 #0.8861

table(test$violator, modpredict >= 0.2)

#Calculating AUC for ROC curve
library(ROCR)

# Prediction function
ROCRpred = prediction(modpredict, test$violator)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Calculating AUC
auc.tmp <- performance(ROCRpred,"auc") 
auc <- as.numeric(auc.tmp@y.values)
auc



