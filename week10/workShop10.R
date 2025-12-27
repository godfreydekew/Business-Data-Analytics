
set.seed(999)

trainData <- read.csv("train.csv", head = T, stringsAsFactors = TRUE)
testData <- read.csv("test.csv", head = T, stringsAsFactors = TRUE)

testData$X <- NULL
trainData$X <- NULL

fit <- train(salesBin ~ ., data = trainData, method = "rpart")
fit$finalModel
rpart.plot(fit$finalModel, type = 2, fallen.leaves = F)

rf_fit <- train(salesBin ~ ., method = "rf", importance = T, data = trainData)

rf_fit$finalModel

fit$results
summary(fit)

pred <- predict.train(fit, testData)

print(confusionMatrix(as.factor(pred),as.factor(testData$salesBin)))
# Positive argument should only be used when working with binary classuficaton
# I can see from the results cp increased as the tree become less ocmplex
# Kappa decreased which means the model was less confident in the split 

library(verification)
roc.plot(testData$incomebin == 1, pred[,2])

# Why is this a perfect ideal problem for CARTS.
# Because volume was the only predictator and the tree becomes simple.
# Cart removed all other variables that could have made the tree more complex for no reason
