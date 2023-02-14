##############################################
# TP01 - Chapter 7, Moving Beyond Linearity 
# February 6, 2023
# Team 2 | Kylah | Rupam | Ned | Will 

# First Applied Problem (7.9.6)

library(ISLR2)
data(Wage)

fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

attach(Wage)
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 *preds$se.fit)

library(boot)
set.seed(1)
cv.error <- rep(0, 5)
for (i in 1:5) {
     glm.fit <- glm(wage ~ poly(age, i), data = Wage)
     cv.error[i] <- cv.glm(Wage, glm.fit)$delta[1]
}

cv.error
Optimal.degree <- which.min(cv.error)
print(Optimal.degree)
# Cross Validation selected an optimal 4(degree) Polynomial 

# Compare the results of hypothesis testing using ANOVA
fit.1 <- lm(wage ~ poly(age,1), data=Wage)
fit.2 <- lm(wage ~ poly(age,2), data=Wage)
fit.3 <- lm(wage ~ poly(age,3), data=Wage)
fit.4 <- lm(wage ~ poly(age,4), data=Wage)
fit.5 <- lm(wage ~ poly(age,5), data=Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary(fit.5))

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), 
    oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree 4 Polynomial", outer = T)
lines(age.grid, preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# 2 Step Function 
# Cut function converts numbers to factor 
table(cut(age, 4))
fit.cut <- lm(wage ~ cut(age, 4))
coef(summary(fit.cut))

cv.error1 <- rep(NA, 10)
for (i in 2:10) {
     Wage$age.cut <- cut(Wage$age, i)
     lm.fit <- glm(wage ~ age.cut, data = Wage)
     cv.error1[i] <- cv.glm(Wage, lm.fit, K = 10)$delta[1]
}

Optimaldegree <- which.min(cv.error1)
print(Optimaldegree)

plot(2:10, cv.error1[-1], xlab = 'Cuts', ylab = 'Test MSE', type = 'l')
points(Optimaldegree, cv.error1[Optimaldegree], col = 'red', cex = 2, pch = 19)

plot(wage ~ age, data = Wage, col = "darkgrey")
fit.step <- glm(wage ~ cut(age, 8), data = Wage)
pred.step <- predict(fit.step, list(age = age.grid))
lines(age.grid, pred.step, col = "red", lwd = 2)

