# import data
spesisdata <- read.csv(file="1.4.11.Sepsis.csv", header=T)

# exercise 1
colnames(spesisdata)

# exercise 2
racecol <- spesisdata["race"]
table(racecol)

# exercise 3
apaposi <- order(spesisdata$apache);
lowsix <- data.frame(apachelow = spesisdata$apache[apaposi[1:6]],
                       basetemp = spesisdata$temp0[apaposi[1:6]])
lowsix

line35 <- c("id","fate","apache")
spesisdata[which(spesisdata$apache > 34 & spesisdata$race == 1),line35]

# exercise 6
otherrace <- spesisdata[which(spesisdata$race == 2),"id"]
tempother <- data.frame(basetemp = spesisdata$temp0[otherrace],
                        twohtemp = spesisdata$temp1[otherrace])
t.test(tempother$basetemp, tempother$twohtemp)

# exercise 7
t.test(spesisdata$apache~spesisdata$treat)