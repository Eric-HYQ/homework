# 1.a
n1 <- 48+60+129+40
n2 <- 16+120+200+800
or <- (48*40/n1+16*800/n2)/(60*129/n1+200*120/n2)
paste('common OR for air EMS and death after adjusting is', or)

# 1.b
library(vcd)

ems <- c(rep("road",40),rep("heli",177),rep("road",260),rep("heli",136),rep("road",800))
live <- c(rep("surv",169),rep("dead",324),rep("surv",920))
ser <- c(rep("serious",277), rep("less",1136))
mytable <- table(ems,live,ser)
mytable
mantelhaen.test(mytable)

# 1.c
woolf_test(mytable)

# 2.a
livenum <- c(rep(c(1),169),rep(c(0),324),rep(c(1),920)) # 1 is surv. 0 is dead
data <- data.frame(ems,livenum,ser) 
model <- glm(livenum~ems*ser,family='binomial',data = data)
summary(model)

# 2.b
library(pROC)

pred <- predict(model,data = data)
p <- exp(pred)/(1+exp(pred))

pRoc <- roc(livenum, p)
plot(pRoc, print.auc=TRUE, legacy.axes=TRUE)
