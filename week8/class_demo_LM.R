library(MASS)


df <- Melanoma

#convert factors into factors
df$ulcer <- as.factor(df$ulcer)
df$sex <- as.factor(df$sex)
df$status <- as.factor(df$status)


?Melanoma

#Full model
fit <- lm(thickness ~ ., data = df)
summary(fit)

#There is redundancy in the model

#Stepwise regression to reduce the number of parameters (variables)
fit_step <- step(fit, k = log(nrow(df)))
summary(fit_step)

#Time to do some serious work. Split the data to avoid overfitting

library(caret)
trainDF <- createDataPartition(df$thickness, p = 0.8, list = F)
trainData <- df[trainDF,]
testData <- df[-trainDF,]
# They are quite flexible they can take categorical values, 

#Repeat the process BUT on the train set only!
#Full model
fit <- lm(thickness ~ ., data = trainData)
summary(fit)

#Reduce the model to remove redundancies
step_fit <- step(fit, k = log(nrow(trainData)))

#Make predictions on unseen data
pred <- predict.lm(fit, testData)

head(pred)

testData$pred <- pred

#How well is this performing?
print(cor(testData$thickness, testData$pred)^2)


