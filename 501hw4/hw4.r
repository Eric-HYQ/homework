# import data
data <- read.csv(file="2.ex.vonHippelLindau.csv", header=T)

# 1
colnames(data)
library(ggplot2)
pne <- data$p_ne
tumorvol <- data$tumorvol

ptum <- ggplot(data = data, aes(x = pne, y = tumorvol)) +
  geom_smooth(method = "lm", se=TRUE, 
              color="black", formula = y ~ x) +
  geom_point()+
  labs(title="Scatter plot",x="plasma norepinephrine(pg/ml)",
                y="tumor volume(ml)")
ptum

r <- cor(pne,tumorvol)
sprintf("r = %f",r)
b <- r*sd(tumorvol)/sd(pne)
sprintf("b = %f",b)

# 2
library(MASS)
model <- lm(tumorvol ~ pne, data = data)
stures <- studres(model)

names <- c(1:37)
stuframe <- data.frame(stures = stures, names = names)
stu <- ggplot(data = stuframe, aes(x = names, y = stures)) +
  geom_smooth(method = "lm", se=TRUE, 
              color="black", formula = y ~ x) +
  geom_point()+
  labs(title="Studentized residuals",x="names",
                y="Residual")
stu

# 5
pnelog <- log(pne)
tvlog <- log(tumorvol)
datalog <- data.frame(pnelog = pnelog, tvlog = tvlog)

ptum <- ggplot(data = datalog, aes(x = pnelog, y = tvlog)) +
  geom_smooth(method = "lm", se=TRUE, 
              color="black", formula = y ~ x) +
  geom_point()+
  labs(title="Log scatter plot",x="plasma norepinephrine(pg/ml)",
                y="tumor volume(ml)")
ptum

rlog <- cor(pnelog,tvlog)
sprintf("rlog = %f",rlog)
blog <- rlog*sd(tvlog)/sd(pnelog)
sprintf("blog = %f",blog)

# 6
modellog <- lm(pnelog ~ tvlog, data = datalog)
new.tvlog <- data.frame(tvlog=c(100))
predcon <- predict(modellog,newdata=new.tvlog,, interval = "confidence")
predcon
predpre <- predict(modellog,newdata=new.tvlog,, interval = "prediction")
predpre

# 7
stureslog <- studres(modellog)

stulogframe <- data.frame(stures = stureslog, names = names)
stulog <- ggplot(data = stulogframe, aes(x = names, y = stureslog)) +
  geom_smooth(method = "lm", se=TRUE, 
              color="black", formula = y ~ x) +
  geom_point()+
  labs(title="Log studentized residuals",x="names",
                y="Residual")
stulog

# 8
pnelog0 <- data$p_ne[which(data$disease == 0)]
tvlog0 <- data$tumorvol[which(data$disease == 0)]
pnelog1 <- data$p_ne[which(data$disease == 1)]
tvlog1 <- data$tumorvol[which(data$disease == 1)]
data0 <- subset(data,disease = 0)
data1 <- subset(data,disease = 1)
fit0 <- lm(tvlog0~pnelog0,data = data0)
fit1 <- lm(tvlog1~pnelog1,data = data1)
summary(fit0)
summary(fit1)

t <- abs((0.03434-0.19924)/sqrt(-0.01911^2+0.09581^2))
t
df <- 26 + 7 - 4
df
