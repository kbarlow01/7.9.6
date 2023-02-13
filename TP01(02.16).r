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

plot(age, wage, xlim = Age)

write.csv(Wage,'C:/Users/kylah/Machine Learning II/wage.csv')
