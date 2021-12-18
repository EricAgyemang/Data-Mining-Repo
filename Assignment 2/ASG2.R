########HOMEWORK1######

###QUESTION 1##########
college <- read.csv(file.choose(), header=T)

rownames(college) = college[, 1]
rownames(college) = college[, 1]
fix(college)
college = college[, -1]
fix(college)

###QUESTION2.8(c)i##########
summary(college)

###QUESTION2.8(c)ii##########
pairs(college[, 1:10])

###QUESTION2.8(c)iii##########
boxplot(college$Outstate ~ college$Private, col = c("cyan", "magenta"), main = "Outstate versus Private",  xlab = "Private", ylab = "Outstate")

###QUESTION2.8(c)iv##########
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
fix(college)
summary(college$Elite)

###QUESTION2.8(c)v##########
par(mfcol = c(2, 3))
hist(college$Accept, breaks = 6, freq = TRUE, col = "gray", main = "Histogram", xlab = "Accept", ylab = "Value")
hist(college$Accept, breaks = 10, freq = TRUE, col = "purple", main = "Histogram",xlab = "Accept", ylab = "Value")
hist(college$Enroll, breaks = 6, freq = TRUE, col = "gray", main = "Histogram",xlab = "Enroll", ylab = "Value")
hist(college$Enroll, breaks = 10, freq = TRUE, col = "purple", main = "Histogram", xlab = "Enroll", ylab = "Value")
hist(college$Top10perc, breaks = 10, freq = TRUE, col = "purple", main = "Histogram",xlab = "Top10perc", ylab = "Value")
     
###QUESTION2.8(c)vi##########
summary(college$PhD)
     
plot(fitdist(college$Grad.Rate, "norm", method = "mle"), demp = T,breaks =quantile(college$Grad.Rate, probs = seq(0.0, 1.0, by = 0.1)))
     
seq <- seq(0.0, 1.0, by = 0.1)
     
hist(college$Accept.Rate, col = "beige", breaks = quantile(college$Accept.Rate, probs = seq))
hist(college$Grad.Rate, col = "orange", breaks = quantile(college$Grad.Rate, probs = seq))
hist(college$Enroll.Rate, col = "red", breaks = quantile(college$Enroll.Rate, probs = seq))
hist(college$Top10perc, col = "black",breaks = quantile(college$Top10perc, probs = seq))
     
par(mfrow = c(2,2))
hist(college$Books, col = 2, xlab = "Books", ylab = "Count")
hist(college$PhD, col = 3, xlab = "PhD", ylab = "Count")
hist(college$Grad.Rate, col = 4, xlab = "Grad Rate", ylab ="Count")
     

########start of question 3########
library(ISLR)
data("Carseats")
lm.fit.a <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.fit.a)
View(Carseats)
attach(Carseats)

str(data.frame(Price, Urban, US))
fit4 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit4)

confint(fit4)

par(mfrow = c(2, 2))
plot(fit4)
########end of question 10#######

########start of question 14######
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

cor(x1, x2)

plot(x1, x2)

fit13 <- lm(y ~ x1 + x2)
summary(fit13)

fit14 <- lm(y ~ x1)
summary(fit14)

fit15 <- lm(y ~ x2)
summary(fit15)

fit16 <- lm(y ~ x1 + x2)
fit17 <- lm(y ~ x1)
fit18 <- lm(y ~ x2)
summary(fit16)

summary(fit17)

summary(fit18)

plot(fit16)

plot(fit17)

plot(fit18)
######end of question 14########
