# install.packages ("titanic")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("e1071")
library(titanic)
library(rpart)
library(rpart.plot)
library(caret)

# Load data
df <- titanic_train

# Keep only variables we need
df <- df[, c("Survived","Sex","Age","Pclass")]

# Convert categorical variables
df$Survived <- factor(df$Survived, levels=c(0,1), labels=c("No","Yes"))
df$Sex      <- factor(df$Sex)
df$Pclass   <- factor(df$Pclass)

# Remove missing age
df <- na.omit(df)

# use seed to prevent randomness
set.seed(123)

###               TRAIN/TEST SAMPLING METHOD        ###################



trainIndex <- createDataPartition(df$Survived, p=0.7, list=FALSE)
trainDF <- df[trainIndex, ]
testDF  <- df[-trainIndex, ]



logit_model <- glm(Survived ~ Sex + Age + Pclass,
                   data = trainDF,
                   family = binomial)

summary(logit_model)
# odds and their interpretation
exp(coef(logit_model))
## Predict the propability for the testing data
logit_pred_prob <- predict(logit_model, newdata=testDF, type="response")

# Convert probability into 0 and 1
logit_pred <- ifelse(logit_pred_prob > 0.5, "Yes", "No")
#Convert the predictit variable into factor
logit_pred <- factor(logit_pred, levels=c("No","Yes"))


# Decision tree
tree_model <- rpart(Survived ~ Sex + Age + Pclass,
                    data = trainDF,
                    method = "class")

# The visualisation of the tree
rpart.plot(tree_model)


# Rules 
rpart.rules(tree)

# Prediction of the unseen data
tree_pred <- predict(tree_model, newdata=testDF, type="class")


# compare models


#logistic regression performance 
logit_cm <- confusionMatrix(logit_pred, testDF$Survived)
logit_cm$overall["Accuracy"]

# Decision Tree performance
tree_cm <- confusionMatrix(tree_pred, testDF$Survived)
tree_cm$overall["Accuracy"]



###               K- CROSS-vALIDATION SAMPLING METHOD        ###################


set.seed(123)  # for reproducibility

ctrl <- trainControl(
  method = "cv",      # k-fold cross-validation
  number = 10         # 10 folds (default)
)

### Logistic regression in cross_validation
logit_fit <- train(
  Survived ~ Sex + Age + Pclass,
  data = trainDF,
  method = "glm",
  family = "binomial",
  trControl = ctrl
)

logit_fit


### Decision Tree with cross-validation

tree_fit <- train(
  Survived ~ Sex + Age + Pclass,  # same formula as before
  data       = trainDF,
  method     = "rpart",         # tell caret to use rpart
 trControl  = ctrl
)

# See the tuning results and best cp
tree_fit
tree_fit$bestTune



# caret stores the rpart object in $finalModel
final_tree <- tree_fit$finalModel

# Visualisation of the final tree

rpart.plot(final_tree)


# Rules from the tree
rpart.rules(final_tree)


# Prediction on unseen data

## Prediction of unseen data for logistic regression model
logit_pred <- predict(logit_fit, newdata = testDF)
## Prediction of unseen data for logistic regression model
tree_pred  <- predict(tree_fit, newdata = testDF)


# Confussion matrix for logistic regression
confusionMatrix(logit_pred, testDF$Survived)
# Confussion matrix for decission tree
confusionMatrix(tree_pred,  testDF$Survived)


# Compare and interprete the accuracies 
logit_acc <- confusionMatrix(logit_pred, testDF$Survived)$overall["Accuracy"]
tree_acc  <- confusionMatrix(tree_pred,  testDF$Survived)$overall["Accuracy"]







