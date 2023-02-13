##############################################
# TP01 - Chapter 7, Moving Beyond Linearity 
# February 6, 2023
# Team 2 | Kylah | Rupam | Ned | Will 

# First Applied Problem (7.9.6)

library(ISLR2)
data(Wage)

library(boot)
set.seed(1)
cv.error <- rep(0, 10)
for (i in 1:10) {
     glm.fit <- glm(wage ~ poly(age, i), data = Wage)
     cv.error[i] <- cv.glm(Wage, glm.fit)$delta[1]
}

cv.error
Optimal.degree <- which.min(cv.error)
print(Optimal.degree)
# Cross Validation selected an optimal 9 (degree) Polynomial 

# Compare the results of hypothesis testing using ANOVA
fit.1 <- lm(wage ~ poly(age,1), data=Wage)
fit.2 <- lm(wage ~ poly(age,2), data=Wage)
fit.3 <- lm(wage ~ poly(age,3), data=Wage)
fit.4 <- lm(wage ~ poly(age,4), data=Wage)
fit.5 <- lm(wage ~ poly(age,5), data=Wage)
fit.6 <- lm(wage ~ poly(age,6), data=Wage)
fit.7 <- lm(wage ~ poly(age,7), data=Wage)
fit.8 <- lm(wage ~ poly(age,8), data=Wage)
fit.9 <- lm(wage ~ poly(age,9), data=Wage)
fit.10 <- lm(wage ~ poly(age,10), data=Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

plot(wage ~ age, data = Wage, col = "darkgrey")
age.range <- range(Wage$age)
age.grid <- seq(from = age.range[1], to = age.range[2])
fit.poly <- lm(wage ~ poly(age, 9), data = Wage)
pred.poly <- predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

# 2 Step Function 
cv.error1 <- NA
for (i in 2:15) {
     Wage$age.cut <- cut(Wage$age, i)
     lm.fit <- glm(wage ~ age.cut, data = Wage)
     cv.error1[i] <- cv.glm(Wage, lm.fit, K = 10)$delta[1]
}

Optimaldegree <- which.min(cv.error1)
print(Optimaldegree)

plot(2:15, cv.error1[-1], xlab = 'Cuts', ylab = 'Test MSE', type = 'l')
points(Optimaldegree, cv.error1[Optimaldegree], col = 'red', cex = 2, pch = 19)

plot(wage ~ age, data = Wage, col = "darkgrey")
fit.step <- glm(wage ~ cut(age, 8), data = Wage)
pred.step <- predict(fit, list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

