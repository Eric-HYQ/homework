# 1
# cinstruct data frame
id <- c(1,2,3,4,5,6,7)
mid <- c(13,16,20,20,26,23,28)
fin <- c(59,82,130,NA,171,100,160)

# clear and reshape
score <- data.frame(id = id, mid = mid, fin = fin)
scorevalid <- na.omit(score)
line <- c(scorevalid$mid,scorevalid$fin)
site <- c(rep("mid",6),rep("final",6))

# anova
slope <- aov(line~site, data=score)
summary(slope)

# 2
final <- scorevalid$fin
midtern <- scorevalid$mid
es <- lm(final~midtern)
summary(es)

# 3
full.mid <- data.frame(midtern=c(13,16,20,20,26,23,28))
predict(es,newdata = full.mid, interval = "confidence")
