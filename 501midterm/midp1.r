# import data
data <- read.csv(file="BME 501 Midterm 2021 Dataset P1.csv", header=T)

tri <- data[,1] # rename colums
cho <- data[,2]

# 1
plot(tri,cho)

cor.test(tri,cho) # correlation

# 2
library(ggplot2)
library(car)
dietfit <- lm(cho~tri, data = data) # linear model

diet <- ggplot(data = data, aes(x = predict(dietfit), y = rstudent(dietfit))) +
  geom_smooth(method = "lm", se=TRUE, 
              color="black", formula = y ~ x) +
  geom_point()+
  labs(title="Studentized residuala plot",x="Triglyceride level(mmol/l)",
                y="Cholesterol level(mmol/l)")
diet # studentized residual & prediction domain

ncvTest(dietfit) # homoscedasticity

# 3
cholog <- log10(cho)
data2 <- data.frame(tri = tri, cholog = cholog)
dietfit2 <- lm(cholog~tri, data = data2)

diet2 <- ggplot(data = data2, aes(x = predict(dietfit2), y = rstudent(dietfit2))) +
  geom_smooth(method = "lm", se=TRUE, 
              color="black", formula = y ~ x) +
  geom_point()+
  labs(title="Studentized residuala plot",x="Triglyceride level(mmol/l)",
                y="Log cholesterol level(mmol/l)")
diet2

ncvTest(dietfit2)

# 4
new.tri <- data.frame(tri=c(5))
precho <- predict(dietfit,newdata=new.tri,, interval = "confidence")
precho # prediction 