# import data
data <- read.csv(file="BME 501 Final 2021 Dataset P1.csv", header=T)

# 1
str(data)
data$deg_malig <- as.factor(data$deg_malig)
data$menopause <- as.factor(data$menopause)
data$node_caps <- as.factor(data$node_caps)
data$breast <- as.factor(data$breast)
data$breast_quad <- as.factor(data$breast_quad)
data$irradiat <- as.factor(data$irradiat)
data$class_num <- as.factor(data$class_num)

primaryModel <- glm(class_num~age+tumor_size+inv_nodes+deg_malig+menopause
                                    +node_caps+breast+breast_quad+irradiat,
                                    data = data, family = binomial())
summary(primaryModel)

# 2
library(pROC)

priPredict <- predict(primaryModel,data = data)
prip <- exp(priPredict)/(1+exp(priPredict))

priRoc <- roc(data$class_num, prip)
plot(priRoc, print.auc=TRUE, legacy.axes=TRUE)

# 3
library(rms)

step(primaryModel)
optModel <- glm(class_num~tumor_size+inv_nodes+deg_malig+irradiat,
                                    data = data, family = binomial())
summary(optModel)
lrtest(primaryModel, optModel)

# 4
optPredict <- predict(optModel,data = data)
optp <- exp(optPredict)/(1+exp(optPredict))

optRoc <- roc(data$class_num, optp)
plot(optRoc, print.auc=TRUE, legacy.axes=TRUE)

# 5
data[which(data$id == 100),]
nd <- data.frame(tumor_size = c(32), inv_nodes = c(4), 
                    deg_malig = factor(c(2)), irradiat = factor(c(1)),
                    class_num = factor(c(1)))
predict(optModel,nd,'response', se.fit = TRUE)

low <- 0.365568-1.96*0.07289716
low
high <- 0.365568+1.96*0.07289716
high

write(optp, file='adjustedPredictData.csv', ncolumns=1)