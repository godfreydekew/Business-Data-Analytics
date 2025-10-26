
############b###############Part 1 - Independent Samples##############


####Step 1 - load the data set 'profit.csv' make sure header is set as true
#assign it to the variable: df. Also make sure to set your working directory to the current
#folder

df <- read.csv("profit.csv", header = T)
df$X <- NULL

####Step 2 - Explore the data anyway you like. 
#The data contains the profit of two startups (A and B) over a 12 month period
#You'll notice that the dataset is in wide format. It's worth turning it to long
#otherwise Levene's becomes difficult. The easiest way to do this is to
#keep just the two variables you need and discard the 'month'

df$month <- NULL
#Now use the melt function
library(reshape2)
df_long <- melt(df)

#Great! so much easier now.

###Step 3 - Normality tests. Use Shapiro-Wilk to check the normality of your dependent variable
#this is named 'value' by default. i.e., df_long$value

################Q1: What is the p-value of shapiro test?
shapiro.test(df_long$value)
# 0.1263

################Q2: What is the W statistic?
# 0.93503
# W is close to 1: This indicates that the sample distribution is very close to the normal distribution.

################Q3: The data is normally distributed, right?
# Yes 

###Step 4 - Homogeneity of variance. Use Levene's test found in the 'car' library
# Check if the variance is equal
library(car)
leveneTest(value ~ variable, data = df_long)
#Your code here
################Q4: What is the F value of the test?
# 2.425
#If the $F$-value is close to $1$: This means the variance between the groups' spreads is small relative to the variance within the groups. 

################Q5: What is the p-value?
# 0.1251

################Q6: The data satisfies the homogeneity of variance assumption, right?
# Yes 
t.test(df_long$value ~ df_long$variable, var.equal = T)
##Step 5 - Given the results of the tests, run a two-tailed test to see if
#the means of the groups are significantly different at a = 0.05

#Your code here
################Q7: Report the t statistic
# t = -3.3434
# T value > 2 indicates significant difference which is not due to random error

################Q8: What is the p-value?
# p-value = 0.002943
################Q9: The result is significant, right?
# Yes

#Now check the effect size (use effSize library)
install.packages("effsize")
library(effsize)
################Q10: What is Cohen's d?
cohen.d(df$companyA, df$companyB)
cohen.d(value ~ variable, data = df_long)
################Q11: What is the magnitude of the effect?
# d estimate: -1.364928 (large)

##############################Part 2###################################
##########################Paired Samples##############################

#a data frame with variable name df. This survey looks at employee
#satisfaction with a legacy (old) system vs a new CRM. The same
#employees were used in both conditions
df <- read.csv("satisfaction.csv", header = T)
df$X <- NULL

#Step 1 - Explore the data however you like
#You will have noticed that the data is in long format, we'll have
#to reshape it to wide.

df_wide <- reshape(df, idvar = "employeeID", timevar = "sysMerge", direction = "wide")


#Step 2 - Normality of differences between the samples

#Your code here
################Q12: What is the p-value for the Shapiro test?
shapiro.test(df_wide$scores.CRM - df_wide$scores.LEGACY)

#So regardless of the results, there is still a parametric violation, can
#you guess what that is?


################Q13: What is the parametric violation?
# Satisfaction as a ordinal type of data

#No worries, this is where wilcoxon signed rank test comes in handy
#We haven't learned how ranked tests work yet, but it doesn't matter
#let's run it anyway

#You hypothesise that satisfaction with the CRM will be significantly
#different than satisfaction with the legacy system

wilcox.test(df_wide$scores.CRM,df_wide$scores.LEGACY, paired = T)

################Q14: Are there significant differences in the scores
#of the two groups? I.e., did the employees prefer one system over the
#other?
#  p-value = 0.8135, since p > 0.05 hence we fail to reject the null hypothesis
# The observed differences is likely due to random error



