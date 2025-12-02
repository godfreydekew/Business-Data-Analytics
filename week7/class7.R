library(psych)
??pysch

bfi_no_na <- na.omit(bfi)
fit <- prcomp(bfi[,1:25])

x <- c(13, 13, 15, 16, 9, 6, 15)
y <- c(16, 6, 6, 11, 6, 12, 13)

cor.test(x,y)
cor(x,y, method = 'spearman')
cor.test(x,y, method = 'kendall')
