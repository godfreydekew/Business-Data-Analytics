library(car)
Salaries

df <- Salaries
#Full model
fit <- lm(salary ~ ., data = df)
summary(fit)

plot(fit)


library(car)
vif(fit)
#reduce parameters
step_fit <- step(fit, k = log(nrow(df)))
summary(step_fit)

#To test on unseen data

inTrain <- createDataPartition(df$salary, p = 0.8, list = F)
trainData <- df[inTrain,]
testData <- df[-inTrain,]

#Full model on train data
fit <- lm(salary ~ ., data = trainData)
summary(fit)

step_fit <- step(fit, k = log(nrow(trainData)))
summary(step_fit)

plot(step_fit)
#Let's see how it performs on unseen data (1 itteration)
pred <- predict.lm(step_fit, testData)
testData$pred <- pred

#How well did it do in this itteration?
cor(testData$pred, testData$salary)^2

