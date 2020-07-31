grade.lm <- lm(GPA~ACT)
confint(grade.lm,level = 0.99) ##0.99 confidence interval
summary(grade.lm)
anova(grade.lm)
MSE.grade <- (summary(grade.lm)$sigma)^2 ##MSE
qt(0.995,118) #calculate t value 

#Problem 3
point.est.act <- grade.lm$coefficients[[1]] + grade.lm$coefficients[[2]]*28 #find point estimate
attach(grade.avg)
ACT.mean <- mean(grade.avg$ACT) #find the mean
sum.dev <- sum((grade.avg$ACT-ACT.mean)^2) #sum of squared deviations
var.grade <- MSE.grade*((1/120)+((28 - ACT.mean)^2)/sum.dev) 
rm(sd.grade)
sd.grade <- sqrt(var.grade) #new sd for new value
qt(0.975,118) #t value at 95%
point.est.act - 1.980272*sd.grade
point.est.act + 1.980272*sd.grade

#Solving it through functions
act.new <- data.frame(ACT=28) 
act.new.conf <- predict(grade.lm, act.new, interval = "confidence", level = 0.95, se.firmt = T)
act.new.conf

#prediction interval
var.pred <- MSE.grade + var.grade #MSE + new variance of new value
sd.pred <- sqrt(var.pred)
#prediction interval at 95%:
point.est.act - 1.980272*sd.pred #lower bound
point.est.act + 1.980272*sd.pred #upper bound
#verify with function
act.new.pred <- predict(grade.lm, act.new, interval = "prediction", level = 0.95, se.firmt = T)
act.new.pred

#confidence band at 95% for part d
W <- sqrt(2*qf(0.95,2,120-2)) 
act.new.conf$fit[,1]+W*act.new.conf$se.fit
act.new.conf$fit[,1]-W*act.new.conf$se.fit


#multiple linear regression
property <- read.table("~/Downloads/CH06PR18.txt", quote="\"", comment.char="")
names(property) <- c("Y","X1","X2","X3","X4")
par(mfrow=c(3,2))
pairs(property)
attach(property)
prop.lm <- lm(Y ~ X1 + X2 + X3 + X4)
summary(prop.lm)
prop.lm1 <- lm(Y ~ X1 + X2 + X4) #drop parameter X3
summary(prop.lm1)
prop.lm2 <- lm(Y ~ X1 + X2) #drop parameter X4
summary(prop.lm2)
prop.lm3 <- lm(Y ~ X1 + X4) #drop parameter X2
summary(prop.lm2)
prop.lm4 <- lm(Y ~ X2 + X4) #drop parameter X1
summary(prop.lm2)
#Will leave parameters X1, X2, and X3 since it has the largest adjusted R^2 
#and largest F statistic
model_aic_back <- step(prop.lm5, direction = "backward") #verify regression is correct model chosen
coef(model_aic_back)

summary(prop.lm1)
confint(prop.lm1, parm = c(2:4), level = 0.95)
#conduct model diagnostic
plot(prop.lm1,which=1) # residuals vs fitted, to check linearity and homoscedasticity(euqal variance)
# check two things: mean of the residuals = 0?/ the spread of the residuals are roughly the same?
# the redline through the scatter plot should be straight and horizontal, not curved
# R automatically flagged 3 data points with largest residuals
plot(prop.lm1,which=2) # QQ plot for normality check
# compare the residules to "ideal" normal observation along the 45-degree line.

#qqnorm(resid(prop.lm1))
#qqline(resid(prop.lm1), col="red")

qqPlot(prop.lm1)


plot(prop.lm1,which=3) # scale-location plot (sqrt standardized residual vs predicted value)
# to check the assumption of homoscedasticity

plot(prop.lm1,which=5) # to detect influential cases
# outliers may/may not be influential points
# ideally, there is no red dash curve line ( cook's distance line)
par(mfrow=c(2,2))
plot(prop.lm1) # put above four plots into one

### detect outliers #####
# Bonferonni adjusted outlier test
# H0: the observaion is not an outlier
outlierTest(prop.lm1) 


### detect Influential observation(s) ####
# Cook's Distance: heuristic, not an exact rule
# cutoff =4/(n-k-1), if C > cutoff, then needs further investigation
cutoff <- 4/(nrow(property)-length(prop.lm1$coefficients)-1)
par(mfrow=c(1,1))
plot(prop.lm1, which=4,cook.levels=cutoff)

influencePlot(prop.lm1)# circle size is proportional to Cook's distance
###  testing the normality assumption ###
#Shapiro-wilk normality test
#H0: the data were sampled from a normal distribution
shapiro.test(resid(prop.lm1))

res.st <- studres(prop.lm1)
hist(res.st, freq = FALSE, main = "studentized residuals")
# adding a bell curve
x_n <- seq(min(res.st), max(res.st),length=50)
y_n <- dnorm(x_n)
lines(x_n, y_n)
#should pay attention to studentized residuals that exceed +2/-2,
# and be more concerned about those exceed +2.5/-2.5


### testing the homoscedasticity assumption: equal variance or not
# H0: variance is constant
ncvTest(prop.lm1)

### testing the linearity #####

crPlots(prop.lm1) # blue line represents the line of best fit
# pink line modelsthe residules
# if pink line curves, then we likely have a linearity problem.


### testing the independence #####
# H0: error terms are not autocorrelated
durbinWatsonTest(prop.lm1)

### a global test for all assumptions ####
globaltest <- gvlma(prop.lm1)
summary(globaltest)


#last problem
#confidence intervals
property.spec <- data.frame(X1 = c(4,6,12), X2 = c(10,11.5,12.5), X3= c(0.10,0,0.32), 
                            X4 = c(80000,120000,340000))

property.conf <- predict(prop.lm, property.spec, interval = "confidence", level = 0.95, se.firmt = T)
property.conf

#prediction interval
property.pred <- predict(prop.lm, property.spec, interval = "prediction", level = 0.95, se.firmt = T)
property.pred
