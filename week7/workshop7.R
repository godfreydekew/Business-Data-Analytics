df <- read.csv("pcaExample.csv", header = T)

df_omit <- na.omit(df)

df_items <- df_omit[,5:30]

#Full model
fit <- prcomp(df_items)

screeplot(fit, type = "lines")

#Use parallel analysis
library(paran)

paran(df_items)
#3 items!

df_items_fullNames <- df_items

names(df_items) <- 1:26
#Renaming the columns

#What item is number 5?
names(df_items_fullNames[1])

#Create a new PCA with only three components
library(psych)
fit <- pca(df_items, 3)

fit

library(psych)
#Check sampling adequacy (> .5 is acceptable, > 0.8 is best)
KMO(df_items)

#How did participant 5 score on the components?
fit$scores[5,]
fit$scores[,1]
fit$scores

#Get the scores
df_omit$RC2 <- fit$scores[,1]
df_omit$RC1 <- fit$scores[,2]
df_omit$RC3 <- fit$scores[,3]

#Make a copy of our dataframe for future use
write.csv(df_omit,"dfFinal.csv")

#We have our three components, and their scores!

#H0: people's perceptions of whether enterpreneurs are born or made has no effect on RC1
#H1: people's perceptions of whether enterpreneurs are born or made has an effect on RC1
table(df_omit$Are.entrepreneurs.born.or.made.)
df_temp <- df_omit[df_omit$Are.entrepreneurs.born.or.made. != "No response",]

t.test(df_temp$RC3 ~ df_temp$Are.entrepreneurs.born.or.made.)

table(df_omit$Was.the.reason.for.leaving.your.previous.organization.employment.an.end.of.contract.)

t.test(df_omit$RC2 ~ df_omit$Was.the.reason.for.leaving.your.previous.organization.employment.an.end.of.contract.)

#MANOVA on all three components with two IVs (and interactions)
fit <- manova(cbind(df_temp$RC2, df_temp$RC1, df_temp$RC3) ~ df_temp$Are.entrepreneurs.born.or.made. * df_temp$Was.the.reason.for.leaving.your.previous.organization.employment.an.end.of.contract.)
summary(fit)

# According to the p values produced neither IV has a significant effect on the set of personality trait
# So first I tested for PCA , qualitatively 
# Then qualitativel7 
# Also run KMO to check if PCA will work 
# Create a new csv file with pca
# 


