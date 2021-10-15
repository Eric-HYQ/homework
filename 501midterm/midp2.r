# import data
data <- read.csv(file="BME 501 Midterm 2021 Dataset P2.csv", header=T)
colnames(data)

# 1
fit <- lm(MPG~Acceleration+Cylinders+Displacement+Horsepower
                    +Manufacturer+Origin+factor(Year)+Weight, data = data )
summary(fit)

step(fit)

fitbest <- lm(MPG~Displacement+Horsepower
                +Manufacturer+factor(Year)+Weight, data = data )
summary(fitbest)

# 2
fit2 <- lm(MPG~Displacement+Horsepower+Manufacturer+factor(Year)
             +Weight+Displacement:Weight, data = data )
summary(fit2)

anova(fitbest,fit2)

# 3
library(MASS)
library(ggplot2)
plot(fit2)
## remove nan rows
stufit2 <- studres(fit2)
prefit2 <- predict(fit2)
rena <- data.frame(stufit2 = stufit2, prefit2 = prefit2)
rena <- na.omit(rena) 

rstu <- ggplot(data = rena, aes(x = rena[,2],y = rena[,1]))+
  geom_smooth(method = 'lm', color="black") +
  geom_point()+
  labs(title="Studentized residuala plot",x="Predict value",
                y="Rstudent")
rstu # studentized residual & prediction domain

# 4
mean(data[,4])
mean(na.omit(data[,5]))
mean(na.omit(data[,9]))
new.wgt <- data.frame(Displacement = c(195,195,195,195),
                      Horsepower = c(105,105,105,105),
                      Manufacturer = factor('ford','ford','ford','ford'),
                      Year = factor('76','76','76','76'),
                      Weight = c(1, 200,2000, 5000))
diff <- predict(fit2,newdata=new.wgt,, interval = "confidence")
diff # prediction