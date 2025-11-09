  
  ##############################Part 1 - Two-way ANOVA##########################
  
  #We'll use the SLID data set. Let's read it carefuly and see what kind
  #of awesome data we've been lucky enough to have for free
  library(car)
  ?SLID
  
  
  #So wow, what a large sample size! 
  #It's at this point I should mention that large n is more likely to lead 
  #NB: to significant results. This is why effect size is doubly important here
  
  #Anyway, our DV is wages. With such a large n, shapiro will fail for sure 
  #let's do it anyway, for fun:
  #CHill we are just checking for normality here 
  shapiro.test(SLID$wages)
  
  #Ok, we expected that. So let's go qualitative
  qqnorm(SLID$wages)
  #hmmm, I can see a little bit of a belly. I think a logarithmic transform 
  #would make this more normal
  qqnorm(log(SLID$wages))
  #better. So let's make a copy of the data set into our own dataset so we
  #can play around with it
  mySLID <- SLID
  
  #Repeat the transformation and assign it to a new variable in the mySLID
  #called logWages
  mySLID$logWage <- log(mySLID$wages)

  #We will use two factors: sex and language as IVs
  #Build a two-way ANOVA to determine whether there are any main effect 
  #or interactions between sex and language. Assign the fitted model to the
  #variable fit

  #You code goes here
#Q1: Which of the terms are significant? Sex
  fit <- aov(mySLID$logWage ~ mySLID$sex * mySLID$language)
  summary(fit)
  
  #We should check for homogeneity of variances
  #You many need to put the formula straight into LeveneTest() if you have problems
  #for example LeveneTest(A ~ B * C)
#Q2: What is the F-statistic for Levene's
  leveneTest(logWage ~ sex * language, data=mySLID)
  # F[1,5] = 1.6302 p=0.1484
   #Q3: Homogeneity of Variances is violated, right?
  #No
  #Q4: Who is earning more, men or women? (don't guess this one, actually check)
  TukeyHSD(fit, conf.level=.95) 
  plot(TukeyHSD(fit, conf.level=.95))
  
  library(ggpubr)
  ggline(mySLID, x = "language", y = "logWage", color = "sex", add = c("mean_ci"))
  # Used to visualize 
  # diff in their mean is + hence men earning more money
  # There is a huge variance in N/A group

  #Carrying on om before, let's say we hypothesise that the gender pay
  #gap increases further with time. To investigate this, we'll add
  #age as a covariate
  qqnorm(mySLID$age)

  #Oh dear... A huge tail
  plot(density(mySLID$age))

  #Right skewed. We can assume that after 65 we are running into pensioners
  #let's add a cut-off of 60
  mySLIDUnder <- mySLID[mySLID$age < 61,]
  qqnorm(log(mySLIDUnder$wages))

  #Better! 
  mySLIDUnder$logAge <- log(mySLIDUnder$age)
  
  #Let's make sure that didn't mess with normality of wage
  qqnorm(mySLIDUnder$logWage)
  
  
  #Nope, it's fine. Now we can look for any effects and interactions (use the logarithmic transformation of the variables, not the original variables)

  #Your code here. Make sure to use mySLIDUnder as the data set
  #Q5: There is no significant effect of age on wages, right (true/false)?
  fit <- aov(mySLIDUnder$logWage ~ mySLIDUnder$sex + mySLIDUnder$logAge)
  summary(fit)
  # False p < 0.05 when checking age against wage hence we reject null hypothesis
  
  # The shaded area on the plot represents the confidence intervals
  # THe lines do not overlap hence there is no interaction
  # Which means the effect of covariate on the factors is the same
  library(sjPlot)
  fit <- lm(logWage ~ sex * logAge, data = mySLIDUnder)
  # Make sure to use * to test moderation
  plot_model(fit, type = "pred", terms = c("logAge", "sex" ))
  #false
  #Q6: There is a signifiant interaction between age and sex, right? (true/false)
  # There is an interaction the gap increases with time 
  
  #Let's plot this and see what's happening
  library(sjPlot)
  fit_plot <- lm(logWage ~ sex * logAge, data = mySLIDUnder)
  plot_model(fit_plot,type = "pred",terms = c("logAge", "sex"))
  
  #Q7: What does the moderating term show us in this context?
  #load the lsr library and use the etaSquared() method to get the effect sizes 
  #(look at the eta.sq column)

  #Q8: What is the effect size (eta-squared) of age?
  library(lsr)
  etaSquared(fit)
  # Result of the omnibus test 
  # We found that sex has an effect with f[1, 4016] = 243.8, p < 0.05 with eta.sq = 0.04
  # We found that age has an effecct with f[1, 4016] = 1218, p< 0.005 with eta.sq = 0.22 which shows significant effect of age on wages
  

  ############################Part 3 - MAN(C)OVA#################
  #let's use the 'oranges' data set found in emmeans
  library(emmeans)
  ?oranges
  
  #RQ: Do the prices of two seperate varieties of oranges, as well as the day and the store impact sales for the two varieties of oranges?
  #IVs: price1 (numeric), price2 (numeric), day (factor), store (factor)
  #DVs: sales1 (numeric), sales2(numeric)
  fit <- manova(cbind(sales1,sales2) ~ day + store + price1 + price2, data = oranges)
  summary(fit)
  summary.aov(fit)

#Build a MANCOVA with the IVs and DVs (remember to use summary.aov for the individual anova)
  #Q13: Does price1 have a significant effect somewhere?
  # yes p < 0.05 for summary(fit)
  #Q14: Does price2 have a significant effect somewhere?
  # yes p < 0.05 for summary(fit)
  #Q15: Does day have a significant effect somewhere?
  # yes p < 0.05 for summary(fit)
  #Q16: Does store have a significant effect somewhere?
  # yes p < 0.05 for summary(fit)
  #Q17: Does price1 significantly affect sales1?
  # THe omnibus teest(summary.aov(fit)) f[1, 23] = 35, p < 0.05 hence price has significant effect on sales1
  #Q18: Does price2 significantly affect sales1?
  # THe omnibus teest(summary.aov(fit)) f[1, 23] = 2.2, p > 0.05 hence price has no significant effect on sales1
  #Q19: What is the p-value for the main effect of day on sales2?
  #0.002661
  #Q20: What is the F-statistic for the main effect of store on sales1?
  #3.5310
  
  #Create a aov model separately for just sales1
  fit <- aov(sales1 ~ store + day + price1 + price2, data = oranges)
  summary(fit)
  #Q21: Does the p-value for the main effect of store on sales1 change?
  # No
  etaSquared(fit)
  #Q22: What is the effect size of the main effect of price1 on sales1?
  #  0.32942206

