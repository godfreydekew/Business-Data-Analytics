# A population of people - how much do they spend?

# The population has a mean of 60 and standard deviation of 30
pop <- rnorm(100000, 60, 30)


sampleMeans <- c()
sampleSDs <- c()

# Sample size of 30
samp <- sample(pop, 30)
mean(samp)
# 59.55 which is close to the actual mean (60)
# This is called sampling error
# If you increase the sample size, sampling error decreases 
 
# What if we have 1000 samples
for (i in 1:1000) {
  samp <- sample(pop, 30)
  sampleMeans[i] <- mean(samp)
  sampleSDs[i] <- sd(samp)
}

# Sample means will always create a normal distribution 
plot(density(sampleMeans))

mean(sampleMeans)
print(sd(sampleMeans))
print(sd(pop))


# Most of the time (95%) the sample mean will contain the population mean
sdSampleDist <- sd(sampleMeans)
succesRate <- 0

for (i in 1:length(sampleMeans)) {
  if (sampleMeans[i] - 1.96 * sdSampleDist <= mean(pop) & sampleMeans[i] + 1.96 * sdSampleDist >= mean(pop)) {
    succesRate <- succesRate + 1
  }
}
# Success rate is 954
# So 95% of sample means contain the population mean 

# What if I don't know the standard deviation of the sampling distribution
pop <- rnorm(100000, 200, 20)

sample <- sample(pop, 60)
ci_lower <- mean(sample) - 1.96*(sd(sample) / sqrt(60))
ci_lower
ci_upper <- mean(sample) + 1.96*(sd(sample) / sqrt(60))
ci_upper

success <- 0
for (i in 1:1000) {
  sample <- sample(pop, 60)
  ci_lower <- mean(sample) - 1.96*(sd(sample) / sqrt(60))
  ci_upper <- mean(sample) + 1.96*(sd(sample) / sqrt(60))
  if (ci_lower <= 200 & ci_upper >= 200) {
    success <- success + 1
  }
}

# How to construct confidence intervals

library(MASS)
df <- Melanoma

library(Rcmdr)
print(sample(MASS, 60))

df <- Salaries
df2 <- Salaries
?Salaries



