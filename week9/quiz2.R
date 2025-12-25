
trainData <- read.csv("train.csv", header = T, stringsAsFactors = TRUE)
testData <- read.csv("test.csv", header = T, stringsAsFactors = TRUE)

testData$X <- NULL
trainData$X <- NULL

# Stepwise to check how many variables do we need 
fit <- glm(status ~ ., data = trainData, family = "binomial")
fit_step <- step(fit, k = log(nrow(trainData)))

# Question 2
pred <- predict(fit_step, testData, type = "response")
head(pred)
# pred <- predict(fit, testData, type = "response")
# head(pred)

# Question 3
pred_class <- ifelse(pred > 0.7, "D","A")
print(confusionMatrix(as.factor(pred_class),as.factor(testData$status), positive = "D"))

library(verification)
roc.plot(testData$status == "D", pred)
