data <- read.csv(file="BME 501 Homework 6-3 dataset.csv", header=T)

modelman1 <- lm(sbp~season+age+dbp+pr+bf+bmi, data = data[which(data$sex == 1),])
summary(modelman1)
step(modelman1)
modelman2 <- lm(sbp~season+age, data = data[which(data$sex == 1),])
summary(modelman2)

modelwo1 <- lm(sbp~season+age+dbp+pr+bf+bmi, data = data[which(data$sex == 2),])
summary(modelwo1)
step(modelman1)
modelwo2 <- lm(sbp~season+age, data = data[which(data$sex == 2),])
summary(modelwo2)

plot(modelman2)
plot(modelwo2)