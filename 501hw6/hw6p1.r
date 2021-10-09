dis1 <- c(2,4,5,3,7,1,5,6,3,4)
dis2 <- c(5,9,9,7,7,7,8,8,8,10)
dis3 <- c(6,9,6,5,7,5,6,6,7,9)
dis4 <- c(3,4,3,6,8,5,5,6,6,4)
exp <- data.frame(irri = c(dis1,dis2,dis3,dis4), 
        dis = factor(c(rep(c("dis1","dis2","dis3","dis4"), each = 10))))
# exp2 <- data.frame(dis1 = dis1, dis2 = dis2, dis3 = dis3, dis4 = dis4)

# 1
x1 = 
model <- lm(irri ~ dis, data = exp)
model
summary(model)
contrasts(exp$dis)
# 2
modelan <- anova(model)
modelan
# summary(modelan)

# part 2
rat <- factor(c(rep(c('one','two','three','four','five','six','seven','eight','nine','ten'),time = 4)))
exp2 <- data.frame(irri = c(dis1,dis2,dis3,dis4), 
        dis = factor(c(rep(c("dis1","dis2","dis3","dis4"), each = 10))),
        rat = rat)


model2 <- lm(irri ~ dis+rat, data = exp)
model2
contrasts(exp$rat)
summary(model2)

modelan2 <- anova(model2)
modelan2