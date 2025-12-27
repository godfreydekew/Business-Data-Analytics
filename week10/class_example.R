library(datadr)
library(rpart)
library(rpart.plot)

df <- adult

df$income <- NULL

# The outcome variable should always be a factor
df$incomebin <- as.factor(df$incomebin)
set.seed(999)

# As always lets split the data
inTrain <- createDataPartition(df$incomebin, p = 0.8, list = F)
trainData <- df[inTrain,]
testData <- df[-inTrain,]
fit <- train(incomebin ~ ., data = trainData, method = "rpart")
rpart.plot(fit$finalModel, type = 2, fallen.leaves = F)

library(verification)
roc.plot(testData$incomebin == 1, pred[,2])
