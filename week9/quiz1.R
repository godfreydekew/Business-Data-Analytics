
trainData <- read.csv("train.csv", header = T)
testData <- read.csv("test.csv", header = T)

new_df <- Aids2

fit <- glm(status ~ ., data = new_df, family = "binomial")
summary(fit)

fit <- glm(status ~ state + sex + diag + T.categ + age, data = new_df, family = "binomial")
summary(fit)

fit_step <- step(fit, k = log(nrow(new_df)))

# Question 4 BIC analysis
model1 <- glm(status ~ age + diag, data = new_df, family = "binomial")
model2 <- glm(status ~ age + diag + sex + state, data = new_df, family = "binomial")

bf21 <- exp((BIC(model2) - BIC(model1))/2)
print(bf21)

summary(model2)
model3 <- glm(status ~ age, data = new_df, family = "binomial")
summary(model3)

exp(model3$coefficients[2])
(1 - exp(0.0211284)) * 100

# Find the MCFADDENS PSEUDO-R2
fitNull <- glm(status ~ 1, data = new_df, family = "binomial")
pseudoR <- 1 - (logLik(model1)/logLik(fitNull))
pseudoR


