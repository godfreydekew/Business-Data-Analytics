df <- mtcars
head(df)
?mtcars

fit <- lm(mpg ~ ., data = df)
summary(fit)

#How many significant predicators are there 
# AIC Or BIC

step_fit <- step(fit, k = log(nrow(df)))
summary(step_fit)
plot(step_fit)

aic_step_fit <- step(fit)
summary(aic_step_fit)

library(car)

# Calculating VIF
vif_values <- vif(fit)
vif_values
#Checking multicolinearity

#Quiz 3
?Salaries

fit <- lm(salary ~ ., data = Salaries)
summary(fit)

dfTest <- read.csv("test.csv", header = T)  
dfTrain <- read.csv("train.csv", header = T)

fit <- lm(salary ~ ., data = dfTrain)
summary(fit)

# BIC stepwise
fit_step <- step(fit, k = log(nrow(dfTrain)))
summary(fit_step)

#prediction 
pred <- predict.lm(fit_step, dfTest)
dfTest$pred = pred

cor(dfTest$pred, dfTest$salary)^2

vif_values <- vif(fit_step)
vif_values
# Variance inflation factor. If values are gretaer than threshold hence model suyfffer 