# Put the answers into the quiz in Week4 - Workshop

# turn this off if you want scientific notation
options(scipen = 999)

#We're going to use the Salaries data, located in the library 'car'
library(car)
?Salaries

#Our IV is rank, our DV is salary
Salaries$rank
str(Salaries$rank)

#Check whether the salaries are normally distributed for each group

#Your code here (use Shapiro-Wilk)

shapiro.test(Salaries[Salaries$rank == "Prof",]$salary)

#Just check normality of DV, that's enough
shapiro.test(Salaries$salary)
#Shapiro is significant, which is bad news, violation of normality

#The ANOVA can be quite robust. However, equal sample sizes are a must
#if there are parametric violations. What are the sizes of each group?
#Here is an example to check the size for the 'professor' group
nrow(Salaries[Salaries$rank == "Prof",])

#Q1:What is the group size for 'professor'
nrow(Salaries[Salaries$rank == "Prof",])
#Q2:What is the group size for 'assistant professor'
nrow(Salaries[Salaries$rank == "AsstProf",])
#Q3:What is the group size for 'associate professor'
nrow(Salaries[Salaries$rank == "AssocProf",])
#Q4:The data is fairly normal (check SW and the QQnorm), right?

#Let's try using a log transformation on Salaries.
df <- Salaries #Let's make a copy of the data frame so we can modify it

df$logSalary <- log(df$salary)

shapiro.test(df$logSalary)
shapiro.test(df$salary)

#First plot
qqnorm(df$salary)
#Log plot
# This data should show a normal distribution 
qqnorm(df$logSalary)

#Q5: Assuming you want to see if there are differences in the salaries between different faculty ranks, what is the null hypothesis?
#  H0: Rank does not have an effect on salary
#  H1: Rank has an effect on salary

#Let's check homogeneity using Levene's test (use the levenTest() function in the car library)
# Variences are equal , if p > 0.05 run the Anova else the anpva assumption is violated
leveneTest(df$salary ~ df$rank, df)
# P is fairly small
#Q6: What is the F-statistic for the test?

#Q7: What should we do now?
# Run non paramteric test 
#Q8: Was the test significant?
# Yes
#Q9: What is the Chi-squared value?
kruskal.test(df$salary ~ df$rank, data = df)
# Kruskal-Wallis chi-squared = 194.01, df = 2, p-value < 2.2e-16
# Hence indicating signifcant deifference between two or more of the group means

#Q10: What is the partial effect size (let's be lazy and just use partial eta-squared = chi-squared / N-1) 
nrow(df)
#Now it's time to see where the true differences in the groups are
#Let's just use Dunn test, since it's the gold standard post hoc following KW
#Hint: the Dunn test can be found in the library FSA
#Double hint: the syntax for the Dunn test is the same as Kruskal-Wallis, which is the same as ANOVA, which is the same for linear models in general
library(FSA)  
dunnTest(logSalary ~ rank, data = df)
#Q11: Looking at just the z values, where do you think the strongest difference is in the salaries?

#Q12: What do the results tell us?

#Finally, I want you to ignore everything I've taught you, and go ahead and analyse the data using ANOVA. Then run post-hoc tests using Tukey HSD
fit_AOV <- aov(df$logSalary ~ df$rank)
summary(fit_AOV)
TukeyHSD(fit_AOV)
#Q13: Were the results pretty much the same?

#Q14: What does that tell us about ANOVA?

#you could probably present results with caution

#Optional
#What if we did the comparisons manually using multiple t-tests and then adjusting?
test1 <- t.test(Salaries[Salaries$rank == "AssocProf",]$salary,Salaries[Salaries$rank == "AsstProf",]$salary)
test2 <- t.test(Salaries[Salaries$rank == "AssocProf",]$salary,Salaries[Salaries$rank == "Prof",]$salary)
test3 <- t.test(Salaries[Salaries$rank == "Prof",]$salary,Salaries[Salaries$rank == "AsstProf",]$salary)

p.adjust(c(test1$p.value, test2$p.value, test3$p.value), method = "holm")

