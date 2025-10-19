#Simulation 1- Not using descriptive stats to generalize

#Generate a random population 
Pop <- rnorm(100000,100,15)

#Sample 1
Drug <- sample(Pop, 30)

#Placebo sample - pill with no effect 
Placebo <- sample(Pop, 30)

# IF you check the means they are different from the population
# If you use descriptive stats you might say placebo is better
mean(Drug)
mean(Placebo)

# How about we do it multiple times

# A list of averages
mDrug <- c()
mPlacebo <- c()

for (i in 1:50){
  Drug <- sample(Pop, 30)
  
  #Placebo sample - pill with no effect 
  Placebo <- sample(Pop, 30)
  
  #Add the averages to the list
  mDrug[i] <- mean(Placebo)
  mPlacebo[i] <- mean(Drug)
}

# Now lets see what happens, we can just plot the graph
plot(mDrug)
lines(mDrug, col = "red")
lines(mPlacebo, col = "green")