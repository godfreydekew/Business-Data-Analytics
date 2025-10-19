
#Load the dataset
df <- read.csv("/home/dekew/Downloads/R/week1/Roger.csv", header = T)
#Remove annoying first column that is constructed by R
df$X <- NULL

#Only keep FEED A group
dfA <- df[df$Diet == 'A',]
dfB <- df[df$Diet == 'B',]

mean(dfA$weight)
mean(dfB$weight)
#Randomly create two sub-groups
pick <- sample(seq_len(nrow(dfA)), 60)
dfA1 <- dfA[pick,]
dfA2 <- dfA[-pick,]

#Get the means of the two Random Groups
mean(dfA1$weight)
#My random group 1 was 149.62
mean(dfA2$weight)
#My random group 2 was 136.28