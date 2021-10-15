# import data
data <- read.csv(file="BME 501 Midterm 2021 Dataset P3.csv", header=T)
colnames(data)

# 1
fit <- lm(SBP~age+factor(raceth)+factor(smoking)+factor(drinkany)
                    +BMI+LDL+factor(statins), data = data )
summary(fit)

step(fit)

fullfit <- lm(SBP~age+factor(raceth)+factor(smoking)+factor(drinkany)
                    +BMI+LDL, data = data)
summary(fullfit)
strfit <- lm(SBP~LDL,data = data)
summary(strfit)

# 2

# 3
fit2 <- lm(SBP~age+factor(raceth)+factor(smoking)+factor(drinkany)
                    +BMI+LDL+factor(raceth):LDL, data = data )
summary(fit2)

anova(fullfit,fit2)
