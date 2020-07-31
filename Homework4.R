#problem 4.3

View(ch4_1)
ch4_1 <- ch4_1[,2:4]
ch4_1 <- data.table(ch4_1)
ch4_1$Chemist <- as.factor(ch4_1$Chemist) #convert chemist and bolt into factors
ch4_1$Bolt <- as.factor(ch4_1$Bolt)
View(ch4_1)
fit <- aov(ch4_1$Strength ~ ch4_1$Chemist + ch4_1$Bolt)
summary(fit)
fit$residuals
qf(1-0.05, 3, 12) #get F critical at alpha=0.05

##try linear regression form
lm.fit <- lm(ch4_1$Strength ~ ch4_1$Chemist + ch4_1$Bolt)
lm.aov <- aov(lm.fit)
summary(lm.aov)
summary(lm.fit)

#Problem 4.10
#part a
ch4_10 <- data.frame(ch4_10)
plot(ch4_10$Nozzle.Design, ch4_10$Shape)
ch4_10$Nozzle.Design <- as.factor(ch4_10$Nozzle.Design)
ch4_10$Jet.Velocity <- as.factor(ch4_10$Jet.Velocity)
fit_p10 <- aov(ch4_10$Shape ~ ch4_10$Nozzle.Design + ch4_10$Jet.Velocity)
summary(fit_p10)
#part b
qqPlot(fit_p10)
plot(fit_p10)
qf(1-0.05, 4, 20) #get F critical at alpha=0.05

interaction.plot(x.factor     = ch4_10$Nozzle.Design,
                 trace.factor = ch4_10$Jet.Velocity, 
                 response     = ch4_10$Shape, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green", "blue", "orange", "yellow"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 21, 30, 5),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
#part c
model.tables(fit_p10, type="means", se = TRUE)

plot(TukeyHSD(fit_p10, which = "ch4_10$Nozzle.Design"), las = 1)
DMCT <- duncan.test(ch4_10$Shape, ch4_10$Nozzle.Design, 20, 0.002865)
duncan.test(fit_p10,"ch4_10$Nozzle.Design",alpha=0.05,console=TRUE)

#without the velocity
fit_p102 <- aov(ch4_10$Shape ~ ch4_10$Nozzle.Design)
summary(fit_p102)
qqPlot(fit_p102)
plot(fit_p102)

#Problem 4.11
ch4_11 <- data.frame(ch4_11)
ch4_11$Project <- as.factor(ch4_11$Project)
ch4_11$Algorith <- as.factor(ch4_11$Algorith)
str(ch4_11)
attach(ch4_11)
fit11 <- aov(Cost.Error ~ Algorith + Project, data = ch4_11)
summary(fit11)
qf(1-0.05, 5, 25) #get F critical at alpha=0.05
#part b
plot(fit11)
qqPlot(fit11)
#part c
model.tables(fit11, type="means", se = TRUE)

try <- ch4_11[-c(27,31,33),]

ch4_11_rem <- aov(Cost.Error ~ Algorith+Project, data = ch4_11[-c(27,31,33),])
summary(ch4_11_rem)
model.tables(ch4_11_rem, type="means", se = TRUE)


#Problem last

road = as.factor(c(rep("Asphalt", 12), rep("Concrete", 12), rep("Gravel", 12)))

brand = as.factor(rep(c(rep("X",4), rep("Y",4), rep("Z",4)),3))


tread = c(36, 39, 39, 38, 42, 40, 39, 42, 32, 36, 35, 34,
          38, 40, 41, 40, 42, 45, 48, 47, 37, 33, 33, 34, 
          34, 32, 34, 35, 34, 34, 30, 31, 36, 35, 35, 33)

tire = data.frame(road, brand, tread)
View(tire)
## Brand X and Asphalt as reference level
fit.tire <- lm(tread ~ brand*road, data = tire)
summary(fit.tire)
model.matrix(fit.tire)

anova.tire <- aov(fit.tire)
summary(anova.tire)

## Brand Y and Concrete as reference level
tire$brand <- relevel(tire$brand, ref = "Y")
tire$road <- relevel(tire$road, ref = "Concrete")
fit.tire1 <- lm(tread ~ brand*road, data = tire)
summary(fit.tire1)
model.matrix(fit.tire1)

anova.tire1 <- aov(fit.tire1)
summary(anova.tire1)

## Brand Z and Gravel as reference level
tire$brand <- relevel(tire$brand, ref = "Z")
tire$road <- relevel(tire$road, ref = "Gravel")
fit.tire2 <- lm(tread ~ brand*road, data = tire)
summary(fit.tire2)
model.matrix(fit.tire2)

anova.tire2 <- aov(fit.tire2)
summary(anova.tire2)

plot_model(fit.tire)
attach(tire)
interaction.plot(x.factor     = brand,
                 trace.factor = road, 
                 response     = tread, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 21, 30, 5),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
plot(tire)
plot(fit.tire)


attempt <- model.matrix(~brand+road, data = tire)
aov(attempt)
dummy.coef(fit.tire)
confint.lm()


tire$brand <- relevel(tire$brand, ref = "X")
tire$road <- relevel(tire$road, ref = "Asphalt")
plot(tire)
attach(tire)
tire$
plot(road, brand)
plot(tire)

install.packages(caret)
library(caret)
dmy <- dummyVars(~ brand + road, data=tire, fullRank=T)
View(dmy)
dummy <- data.frame(predict(dmy, tire), tread)
View(dummy)
View(when)
dummy.lm <- lm(tread ~ brand + road, data = dummy)
summary(dummy.lm)
dummy.aov <- aov(dummy.lm)
summary(dummy.aov)

library(ggplot2)
#Scatterplot of all factors X, Y, and Z, and Asphalt, Concrete, and Gravel against tread
tire$brand <- relevel(tire$brand, ref = "X")
tire$road <- relevel(tire$road, ref = "Asphalt")
ggplot(tire, aes(x = road, y = tread)) +
  geom_point(aes(color = brand))

## Brand Y and Concrete as reference level
tire$brand <- relevel(tire$brand, ref = "Y")
tire$road <- relevel(tire$road, ref = "Concrete")
ggplot(tire, aes(x = road, y = tread)) +
  geom_point(aes(color = brand))

## Brand Z and Gravel as reference level
tire$brand <- relevel(tire$brand, ref = "Z")
tire$road <- relevel(tire$road, ref = "Gravel")
ggplot(tire, aes(x = road, y = tread)) +
  geom_point(aes(color = brand))

#Scatterplot of factors X, Y, and Z, and tread
ggplot(tire, aes(x = brand, y = tread)) +
  geom_point(aes(color=brand))

#Scatterplot of road factor
ggplot(tire, aes(x = road, y = tread)) +
  geom_point(aes(color=road))


##ignore from here below
brand.c <- contr.ltfr(brand, contrasts = T, sparse = F)
View(brand.c)     

addit <- data.frame(brand.c, road.c)
View(addit)
model.matrix(~brand+road, data = tire)
model.frame(~brand+road, data = tire)


library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(dummy.lm)
interplot(m=fit.tire, var1 = "brand", var2 = "road")
eff.dum <- effects(dummy.lm)
plot(eff.dum)
hello <- allEffects(dummy.lm)
plot(hello)
gotit <- confint.lm(dummy.lm)
plot(gotit)
ggplot(tire, aes(x=road, y=brand))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)

##ignore from here below
plot(road ~ brand, tire)
r.lm <- lm(hp ~ wt, mtcars)
lines(dummy.lm, col="red", conf.level=0.95, args.pband=list(col=SetAlpha("grey",0.3)) )


ggplot(tire, aes(x=road, y=brand)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')


plotCI(tread,road, ui=-5, li = -5, err = "y")
plot(road, brand)
plot(tire)
View(tire)

model.matrix(tire.lm)
plot(tire[1:10,])
plot(road~brand, data = tire, )


library(ggplot2)
#Scatterplot of all factors X, Y, and Z, and Asphalt, Concrete, and Gravel against tread
tire$brand <- relevel(tire$brand, ref = "X")
tire$road <- relevel(tire$road, ref = "Asphalt")
ggplot(tire, aes(x = road, y = tread)) +
  geom_point(aes(color = brand))

## Brand Y and Concrete as reference level
tire$brand <- relevel(tire$brand, ref = "Y")
tire$road <- relevel(tire$road, ref = "Concrete")
ggplot(tire, aes(x = road, y = tread)) +
  geom_point(aes(color = brand))

## Brand Z and Gravel as reference level
tire$brand <- relevel(tire$brand, ref = "Z")
tire$road <- relevel(tire$road, ref = "Gravel")
ggplot(tire, aes(x = road, y = tread)) +
  geom_point(aes(color = brand))

#Scatterplot of factors X, Y, and Z, and tread
ggplot(tire, aes(x = brand, y = tread)) +
  geom_point(aes(color=brand))

#Scatterplot of road factor
ggplot(tire, aes(x = road, y = tread)) +
  geom_point(aes(color=road))



plot_model(dummy.lm)

