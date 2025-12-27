

library(car)
df <- WeightLoss
set.seed(999)

shapiro.test(df$wl3)

library(boot)

findMean <- function(d, i){
  f <- d[i]
  return(mean(f))
}
control_data <- df$wl3[df$group == "Control"]
b_control <- boot(control_data, statistic = findMean, R = 2000)
boot.ci(b_control, type = "bca") # BCa is the "Gold Standard"

diet_data <- df$wl3[df$group == "Diet"]
b_diet <- boot(diet_data, statistic = findMean, R = 2000)
boot.ci(b_diet, type = "bca")

dietex_data <- df$wl3[df$group == "DietEx"]
b_dietex <- boot(dietex_data, statistic = findMean, R = 2000)
boot.ci(b_dietex, type = "bca")
