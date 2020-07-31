#Problem 1

prob22 <- data.frame(Prob22)
t.test(prob22, mu=225, alternative = "greater") #right sided t test
t.test(prob22, conf.level = 0.95)
qt(.025, df=15)

#Problem 2

prob26 <- data.frame(prob26)
attach(prob26)
#F test for two sided for population variances
var.test(Type.1, Type.2, data = prob26, alternative = "two.sided")
qf(.975, 9, 9)
#unpaired t test
#t test for unequal variances for means part B
t.test(Type.1, Type.2, data = prob26, var.equal = FALSE)
qt(.95, 9)



#Problem 3

prob33 <- data.frame(prob33)
attach(prob33)
shapiro.test(prob33)
diff <- Birth.Order..1 - Birth.Order..2
shapiro.test(diff)
qqPlot(diff)
#paired t test
t.test(Birth.Order..1, Birth.Order..2, data = prob33, paired = TRUE)
qt(.975, 9)

#Problem 4

prob4 <- data.frame(prob4)
prob4.stacked <- stack(prob4)
attach(prob4.stacked)
prob4.aov <- aov(values ~ ind, data = prob4.stacked)
summary(prob4.aov)
prob4.res <- resid(prob4.aov)
plot(values, prob4.res)
plot(prob4.aov, which = 2)
#Problem 5 

attach(prob5)
plot(Density ~ Temp, data = prob5)
prob5$Temp <- as.factor(Temp)
prob5.aov <- aov(prob5$Density ~ prob5$Temp)
summary(prob5.aov)
plot(prob5.aov, which = 2)
analy <- lm(Density ~ as.factor(Temp), data = prob5)
anova(analy)
plot(analy, which = 2)
sred <- rstandard(analy)
hist(sred)
TukeyHSD(aov(analy))

prob5.st <- studres(prob5.aov)
hist(prob5.st, freq = FALSE, main = "studentized residuals")
# adding a bell curve
x_n <- seq(min(prob5.st), max(prob5.st),length=50)
y_n <- dnorm(x_n)
lines(x_n, y_n)
#should pay attention to studentized residuals that exceed +2/-2,
# and be more concerned about those exceed +2.5/-2.5
what1 <- t.test(prob5$Density, mu=21.7, alternative = "two.sided", paired = FALSE, var.equal = FALSE) #two sided t test
curve(dt(prob5*0.0717, df=4)*0.0717, 21.2, 21.8)
dt.prob5 <- dt(prob5$Density, df =4)
scaled.prob5 <- dt.prob5*0.0717
plot(scaled.prob5)
curve(scaled.prob5, 21.2, 21.8)
plot(x=prob5$Density, y= scaled.prob5, type = "l", xlim = c(21.2,21.8))
plot(what1)
prob5.aov
summary(prob5.aov)
plot(prob5$Density, 0.2394)
#Problem 6

prob6 <- data.frame(prob6)
prob6.stacked <- stack(prob6)
attach(prob6.stacked)
prob6.aov <- aov(values ~ ind, data = prob6.stacked)
summary(prob6.aov)
prob6.res <- resid(prob6.aov)
qqPlot(prob6.aov)
plot(values, prob6.res)
plot(prob6.aov, which = 1)
hist(prob6.res)
TukeyHSD(prob6.aov)
prob6.aov$assign
prob6.aov$call
prob6.aov$df.residual
mean(prob6$Brand.2)
t.test(prob6$Brand.2, prob6$Brand.3, data = prob6, mu=0, alternative = "two.sided", paired = TRUE, conf.level = 0.99)
t.test(prob6, mu=85, alternative = "two.sided", paired = FALSE, var.equal = FALSE) #two sided t test

#Problem 7

prob7.stacked <- stack(prob7)
attach(prob7.stacked)
prob7.aov <- aov(values ~ ind, data = prob7.stacked)
summary(prob7.aov)
plot(prob7.aov, which = 2)
prob7.res <- resid(prob7.aov)
hist(prob7.res)
qqPlot(prob7.aov)
