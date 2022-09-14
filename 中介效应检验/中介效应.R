#导入数据#
data = read.csv("dataset_modmed.csv")
#中心化数据#
data$cen.x <- scale(data$x, center = TRUE, scale = FALSE)[,]
data$cen.m <- scale(data$m, center = TRUE, scale = FALSE)[,]
data$cen.y <- scale(data$y, center = TRUE, scale = FALSE)[,]
自变量 = data$cen.x
中介变量 = data$cen.m
因变量 = data$cen.y
#检验主效应和中介效应#
model1 = lm(因变量~自变量)
summary(model1)
model2 = lm(中介变量~自变量)
summary(model2)
model3 = lm(因变量 ~ 自变量+中介变量)
summary(model3)
#拟合系数估计和效应对比(boostrup)#
library(mediation)
results = mediate(model2, model3, treat='自变量', mediator="中介变量", boot=T)
summary(results)
