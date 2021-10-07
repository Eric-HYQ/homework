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
data
# 2
covia <- aov(dollar~id+incid+preval+hospdays+mort+yll+daly)
summary(covia)
funding <- data.frame(id,dollar,incid,preval,hospdays,mort,yll,daly)
library(HH)
ancova(dollar~id+incid+preval+hospdays+mort+yll+daly, data = funding)