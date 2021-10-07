# import data
data <- read.csv(file="3.ex.Funding.csv", header=T)
id <- data$id
dollar <- data$dollars
incid <- data$incid
preval <- data$preval
hospdays <- data$hospdays
mort <- data$mort
yll <- data$yrslost
daly <- data$disabil

logdol <- log(dollar)
loghd <- log(hospdays)
logmo <- log(mort)
logyll <- log(yll)
logdy <- log(daly)

library(MASS)
library(ggplot2)

logfund <- data.frame(logdol=logdol, loghd=loghd, logmo = logmo, logyll = logyll, logdy = logdy)

ptum <- ggplot(data = logfund, aes(x = loghd+logmo+logyll+logdy, y = logdol)) +
  geom_smooth(method = "lm", se=TRUE, 
              color="black", formula = y ~ x) +
  geom_point()+
  labs(title="Log scatter plot",x="plasma norepinephrine(pg/ml)",
                y="tumor volume(ml)")
ptum
