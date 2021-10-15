# import data
data <- read.csv(file="BME 501 Midterm 2021 Dataset P4.csv", header=T)
colnames(data)
phy <- data[,1]
glu <- data$glucose

# 1
boxplot(glu ~ phy,data = data,
         main = "Glucose physical activities category",
         xlab = "Physical activities",
         ylab = "Glucose(mg/dl)")

fit <- lm(glu ~ phy, data = data)
anova(fit)

# 2
glu1 <- data[which(data[,1] == "much less active"),]
glu2 <- data[which(data[,1] == "somewhat less active"),]
glu3 <- data[which(data[,1] == "about as active"),]
glu4 <- data[which(data[,1] == "somewhat more active"),]
glu5 <- data[which(data[,1] == "much more active"),]
shapiro.test(glu1[,2])
shapiro.test(glu2[,2])
shapiro.test(glu3[,2])
shapiro.test(glu4[,2])
shapiro.test(glu5[,2])

# 3
plot(fit)
num <- c(dim(glu1)[1],dim(glu2)[1],dim(glu3)[1],dim(glu4)[1],dim(glu5)[1])
vari <- c(var(glu1[,2]),var(glu2[,2]),var(glu3[,2]),var(glu4[,2]),var(glu5[,2]))
num
vari
t.test(num, vari)
cor.test(num,vari)

# 4
bartlett.test(glu~phy)

# 5
library(onewaytests)
bf.test(glu~phy,data = data)