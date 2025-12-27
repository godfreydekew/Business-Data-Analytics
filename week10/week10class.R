library(datadr)
df <- adult
df$income <- NULL

df$incomebin <- as.factor(df$incomebin)
set.seed(999)

install.packages("caret")
library(caret)
#As always, split the data 
inTrain <- createDataPartition(df$incomebin, p = 0.8, list = F)
trainData <- df[inTrain,]
testData <- df[-inTrain,]

fit <- train(incomebin ~ ., data = trainData, method = "rpart")
library(verification)
install.packages("rpart")
library(rpart)
library(rpart.plot)
rpart.plot(fit$finalModel, type = 2, fallen.leaves = F)

# For Logistic Regression (outputs probabilities between 0 and 1)
pred_probs <- predict(fit, newdata = testData, type = "response")
pred_probs <- predict(fit, newdata = testData, type = "prob")
#Prob is for probabilities while raw is for classification

roc.plot(testData$incomebin == 1, pred_probs[, 2])
# true postqive rates and false alarms 
