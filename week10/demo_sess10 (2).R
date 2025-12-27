library(MASS)

df <- Melanoma

?Melanoma

library(caret)
#Change response variable into factor
df$ulcer <- as.factor(df$ulcer)
#Split data into train and test
inTrain <- createDataPartition(df$ulcer, p = 0.8, list = F)

trainDF <- df[inTrain,]
testDF <- df[-inTrain,]

#Train a rpart model (classification tree)

fit <- train(ulcer ~ ., data = trainDF, method = "rpart")

fit

yhat <- predict(fit, testDF)
table(yhat)

testDF$yhat <- yhat

print(confusionMatrix(as.factor(testDF$yhat), as.factor(testDF$ulcer),positive = "1"))

#Compare it to logit

fit_GLM <- glm(ulcer ~ ., data = trainDF, family = "binomial")
summary(fit_GLM)
stepFit <- step(fit_GLM, k = log(nrow(trainDF)))

#Look at last week's lecture to see how to build the logit.
#You can compare:
#Confusion Matrix
#Area under the curve
#Others?

