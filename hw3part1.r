back <- c(20, 24, 26, 21, 19)
side <- c(21, 23, 25, 24, 16)
sit <- c(24, 25, 27, 28, 24)
stand <- c(26, 25, 28, 29, 25)

heartbeat <- data.frame(length = c(back,side,sit,stand),site = factor(c(rep("1",5),rep("2",5),rep("3",5),rep("4",5))))

# check the data
options(digits = 4)
tapply(heartbeat$length, heartbeat$site, mean)

tapply(heartbeat$length, heartbeat$site, var)

# variance test
bartlett.test(length~site, data=heartbeat)

# ANOVA
fit <- aov(length~site, data=heartbeat)
summary(fit)
plot(fit)