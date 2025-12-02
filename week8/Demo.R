library(MASS)
?Aids2

fit <- lm(death ~ ., data = Aids2)
summary(fit)
# There are so many redundant variables that will end up inflating Rsquared
# For multiple linear regression use adjusted r because it doesnt inflate rsquared 
# R squared - how much variance is explained by the model 
# Adjusted R-squared:  0.7452 

#stepwise model :Akaike Information Criterion (AIC)
fit_step <- step(fit,k = log(nrow(Aids2)))
# This will checck which variables to keep
summary(fit_step)
# Adjusted R-squared:  0.7432 
# we can see that r didnt suffer that much 

# But how much better is the first model compared to this one :Bayesian Information Criterion (BIC)
# exp((BIC1 - BIC2) / 2)
BF <- exp((32778.73 - 32771.12)/2)
BF
# 6 BILLIOIN likely to fit the data than full model
Step:  AIC=32778.73
death ~ sex + diag + status + age

Step:  AIC=32771.12
death ~ diag + status + age

# Here the answer is 44, which means the last model is 44 times better

# Checking parametric assumptions
# Its enough to use plots and graphs
plot(fit_step)
# The graph shows we have to use multilenear 

#Lets give it a try on Malonam data 
library(caret)
set.seed(999)

df <- Melanoma

inTrain <- createDataPartition(df$thickness,p = 0.8,list = F)
dfTrain <- df[inTrain,]
dfTest <- df[-inTrain,]

fit <- lm(thickness ~ ., data = dfTrain)
summary(fit)
plot(fit)

fit_step <- step(fit, k = log(nrow(dfTrain)))
# $\text{BIC}$ score (since $k = \log(n)$ in the step() function applies the $\text{BIC}$ penalty).
summary(fit_step)
# YOu can report the analysis and put it into a table 
pred <- predict.lm(fit_step, dfTest)
pred

#Check how well the model is performing 
cor(pred, dfTest$thickness)^2
