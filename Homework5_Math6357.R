#Problem 1 Ch4 4.21

attach(hw4_1)
hw4_1 <- data.frame(hw4_1)
attach(hw4_1)
Distance <- as.factor(Distance)
Subject <- as.factor(Subject)
fit.fix <- aov(Focus.Time~Distance+Error(Subject))
summary(fit.fix)
(9.075-1.275)/4

library(lme4)
fit.reml = lmer(Focus.Time~Distance+(1|Subject))
summary(fit.reml)
confint(fit.reml)
#Problem 2 Ch4 4.22
hw5_2 <- data.frame(Data_Sets)
rm(Data_Sets)
attach(hw5_2)
Batch <- as.factor(Batch) ; Day <- as.factor(Day) ; Catalyst <- as.factor(Catalyst)
fit.pb2 <- aov(Time~Batch+Day+Catalyst)
summary(fit.pb2)
#check model adequacy
plot(fit.pb2, which=2)
plot(fit.pb2, which=1)
#Problem 3 CH5 5.1
1-pf(.0367, 1, 12)
1-pf(4.59, 2, 12)
1-pf(2.5833, 2, 12)
#Problem 4 Ch5 5.3
hw5_4 <- data.frame(hw5_4)
attach(hw5_4)
Pressure <- as.factor(Pressure)
Temperature <- as.factor(Temperature)
fit.fact <- aov(Yield~ Pressure*Temperature)
summary(fit.fact)
par(mfrow=c(2,2))
plot(fit.fact)

par(mfrow=c(1,1))
interaction.plot(x.factor     = Pressure,
                 trace.factor = Temperature, 
                 response     = Yield, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 21, 30, 5),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#Problem 5 Ch2 2.42
colnames(CH02PR42) <- c("Y1", "Y2")
CH02PR42
attach(CH02PR42)
plot(Y1, Y2)
cor(Y1, Y2)
mean(Y1)
mean(Y2)
Y.1 = Y1 - mean(Y1)
Y.2 = Y2 - mean(Y2)
Y.1_and_Y.2 = Y.1*Y.2
Y.1_squared = Y.1^2
Y.2_squared = Y.2^2
sum(Y.1_and_Y.2)/ sqrt(sum(Y.1_squared)*sum(Y.2_squared)) #correlation coeff. 
#test stat
(0.9528469*sqrt(13))/sqrt((1-(0.9528469)^2))
qt(0.995, 13)

#Problem 6 Ch2 2.43
#test stat for part a 
(0.61*sqrt(82))/sqrt((1-(0.61)^2))
qt(0.975, 82)
#part b
1/sqrt(84-3) #std
qnorm(0.975) #z value
#get lower and upper bound of CI
0.70892 - 1.959964*0.1111111
0.70892 + 1.959964*0.1111111
FisherZ(0.61)
fisherz(0.61)
fisherz2r(0.4911)
fisherz2r(0.9266)
#square of correlation of coeff CI
0.455^2
0.729^2

#Problem 7 ch2 2.49
plot(CH01PR28)
colnames(CH01PR28) <- c("Y", "X")
CH01PR28
attach(CH01PR28)
Ri1 = rank(CH01PR28$X)
Ri2 = rank(CH01PR28$Y)
Ri1. = Ri1 - mean(Ri1)
Ri2. = Ri2 - mean(Ri2)
Ri1_and_Ri2 = Ri1.*Ri2.
Ri1.squared = Ri1.^2
Ri2.squared = Ri2.^2
#spearman coeff
sum(Ri1_and_Ri2)/(sum(Ri1.squared)*sum(Ri2.squared))^(1/2)
corr <- cor.test(x=CH01PR28$X, y=CH01PR28$Y, method = 'spearman')
corr
#test statistic
(-0.4259324*sqrt(82))/sqrt(1-(-0.4259324^2))
qt(.995, 82)
