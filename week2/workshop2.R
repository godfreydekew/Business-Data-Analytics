
#Short description:
#A logistics company faces a dilema. The current method of planning
#routes for pickup and delivery relies on a human operator. Even though
#this method served them quite well for 20 or so years, the competition
#has automated the process using software packages that allegedly 
#optimise the tours and lower costs.

#The company agreed to use a demo of the software that lasts one week
#and then evaluate whether there is any difference (they don't know if it 
#will be faster or slower just yet).


df <- read.csv("/home/dekew/Downloads/R/week2/logistics.csv", header = T)

  #Q1: What is the mean distance travelled?
mean_distance <- mean(df$Distance_Traveled)
  
  #Q2: What is the standard deviation of the distance travelled?
  s <- sd(df$Distance_Traveled)
  
  #Q3: What is the median distance travelled?
  median_distance <- median(df$Distance_Traveled)
  
  #Q4: What is the lower bound of the 95% confidence interval for distance traveled?
  #This is possible if N > 30 or standard deviation is known and in this case we have both
  # Its basically X_mean +/- Margin of error
  n <- length(df$Distance_Traveled)
  ME <- qt(0.975,df=n-1) * s/sqrt(n)
  lower_b <- mean_distance - ME
  #Q5: What is the upper bound of the confidence interval for distance traveled?
  upper_b <- mean_distance + ME
  #Q6: Suppose in the same week, some vans used the 'old system', and some used
  # the new system. You have already worked out the confidence interval for the
  # new system. If the old system had a mean distance traveled of 505, and sd of
  # 55, and again 20 vans were samples (n = 20), is there enough evidence to
  # suggest that the difference in the performance of the two systems is 
  # significant? Use a 95% confidence interval for the second group as well.

  old_ME <- qt(0.975,df=20-1) * 55/sqrt(20)
  
  old_lower_b <- 505 - ME
  #Q5: What is the upper bound of the confidence interval for distance traveled?
  old_upper_b <- mean_distance + ME
  #There is no much difference in the means and then CI range which means they likely come from the same group[
  #Which supports the null hypothesis 
  
  df <- data.frame(
    Variable = c("New System", "Old System"),
    mean = c(521.594, 505),
    me = c(29.2557, 25.74079)
  )
  
  # load the ggplot package (needed for plotting)
  library(ggplot2)
  
  # Create the plot
  ggplot(df, aes(x = Variable, y = mean, fill = as.factor(Variable))) +
    geom_bar(stat = "identity", show.legend = F) +
    geom_errorbar(aes(ymin = mean - me, ymax = mean + me), width = 0.4) +
    labs(x = "Variable", y = "Mean", title = "Confidence Interval Plot")
  
  # The confidence intervals do overlap, hence no significant difference in the means
##########################PART 3, plotting CIs of two groups############
install.packages("sjPlot")
install.packages("ggplot2")
library(MASS)
library(sjPlot) #Download this library from tools -> Install packages
?mtcars

#in 1974, was there a difference in weights of automatics and manual cars?
#Let's plot the weights with the 95% CIs

df <- mtcars
df$am <- as.factor(df$am)

# Now to plot the data. The easiest way is to use linear models. It doesn't matter
# as you will learn in the following weeks:
fit <- lm(wt ~ am, data = df)  # Fit linear model: weight ~ transmission type
plot_model(fit, type = "pred")  # Plot predicted values with confidence intervals

#Hopefully you can see that the confidence intervals don't overlap. 


