# import data
data <- read.csv(file="BME 501 Final 2021 Dataset P1.csv", header=T)
data <- na.omit(data)
data$deg_malig <- as.factor(data$deg_malig)
data$menopause <- as.factor(data$menopause)
data$node_caps <- as.factor(data$node_caps)
data$breast <- as.factor(data$breast)
data$breast_quad <- as.factor(data$breast_quad)
data$irradiat <- as.factor(data$irradiat)
data$class_num <- as.factor(data$class_num)

# 1
oh_deg_malig <- model.matrix(~deg_malig-1,data)
oh_irradiat <- model.matrix(~irradiat-1,data)

library(dplyr)
data <- cbind(data$tumor_size,data$inv_nodes,as.data.frame(oh_deg_malig),as.data.frame(oh_irradiat))
str(data)

traindata <- data[1:222,]
testdata <- data[223:277,]

# 2
library(keras)

model <- keras_model_sequential()
model %>%
    layer_dense(units=4, activation='relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units=1, activation='sogmoid')
summary(model)