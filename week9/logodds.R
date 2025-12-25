library(car)
df <- Salaries
head(df)

fit <- glm(discipline ~ ., data = df, family = "binomial")
summary(fit)

exp(fit$coefficients[4])
exp(fit$coefficients[7])

# For every salary increase there is a 99% likelihood that an academic will be 
# But we can also look at the p values and see what are they saying.
# If the value is lesss than 0.05 then that probability is not signifcan
# IF we were to look at sexMale, it means female is used as the baseline. Hence an 
# academic is likely 6% higher to be a male than female.

df <- read.csv("GermanCredit.csv", head = T)

library(caret)
inTrain <- createDataPartition(df$credit_risk, p = 0.8, list = F)
trainData <- df[inTrain,]
testData <- df[-inTrain,]

fit <- glm(credit_risk ~ ., data = trainDsata, family = "binomial")
summary(fit)

fitstep <- step(fit, k = log(nrow(trainData)))

pred <- predict(fitstep, testData, type = "response")
head(pred)
# Result here is just probabilities and we need to convert them using a certain threshold
# We can use a threshold of 0.5.
# Change the cutoff and then it will help you decide accuracy of hte model
#We can use a loop to go through each cutoff and decide which one is the best 
pred <- ifelse(pred > 0.5, 1,0)
print(confusionMatrix(as.factor(pred),as.factor(testData$credit_risk), positive = "1"))

library(verification)
pred <- predict(fitstep, testData, type = "response")
roc.plot(testData$credit_risk == 1, pred)
