
set.seed(999)
trainData <- read.csv("workshop_trainData.csv", header = T, stringsAsFactors = TRUE)
testData <- read.csv("workshop_testData.csv", header = T, stringsAsFactors = TRUE)

testData$X <- NULL
trainData$X <- NULL
full_model <- glm(incomebin ~ ., data = trainData, family = "binomial")

fitNull <- glm(incomebin ~ 1, data = trainData, family = "binomial")
pseudoR <- 1 - (logLik(full_model)/logLik(fitNull))
pseudoR

fitstep <- step(full_model, k = log(nrow(trainData)))

pseudoR <- 1 - fitstep$deviance/fitstep$null.deviance
pseudoR

BFworse_better <- exp((BIC(full_model) - BIC(fitstep))/2)
BFworse_better

options(scipen = 999)
pred <- predict(fitstep, testData, type = "response")
head(pred)


# Calculating the best cuttoff
library(pROC)

roc_obj <- roc(testData$incomebin, pred)
best_coords <- coords(roc_obj, "best", ret = "threshold", transpose = FALSE)
best_coords$threshold
library(caret)
# Use cut-off to give labels
best_pred <- ifelse(pred > 0.35, 1, 0)
print(confusionMatrix(as.factor(best_pred),as.factor(testData$incomebin), positive = "1"))

library(verification)
roc.plot(testData$incomebin == "1", pred)



