library(questionr)
library(corrplot)
#http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r#nature-of-the-dependence-between-the-row-and-the-column-variables
#Chi-square test examines whether rows and columns of a contingency table are statistically significantly associated.

#Null hypothesis (H0): the row and the column variables of the contingency table are independent.
#Alternative hypothesis (H1): row and column variables are dependent

dfchi <- read.csv( "outputdata/anosVvsE.csv", stringsAsFactors = FALSE)


set.seed(122)
dfchi$X = NULL
dfchi$group = NULL
df_t <- t(dfchi)
test <- chisq.test(df_t)

test$observed
test$expected
test$residuals
test$stdres
test$p.value
test$p.value < 0.001

round(36.90678,2)

## Standardized residuals
res <- chisq.residuals(df_t, std = TRUE)
res
corrplot(res, is.cor = FALSE)


#Times

dfchi <- read.csv( "outputdata/teams.csv", stringsAsFactors = FALSE)
set.seed(122)
dfchi$X = NULL
dfchi$group = NULL
dfchi$Specialist = as.numeric(dfchi$Specialist) 
dfchi$Versatile = as.numeric(dfchi$Versatile) 
df_t <- t(dfchi)
test <- chisq.test(df_t, simulate.p.value=TRUE)
summary(test)
test$observed
test$expected
test$residuals
test$stdres
test$p.value
test$p.value < 0.001

round(33.074822,2)

## Standardized residuals
res <- chisq.residuals(df_t, std = TRUE)
res
corrplot(res, is.cor = FALSE)







a <- c(6, 15)
b <- c(26, 171)
m <- matrix(c(a, b-a), ncol=2)
fisher.test(m, alternative = 'greater') 

chisq.test(m)
chisq.test(m)$expected