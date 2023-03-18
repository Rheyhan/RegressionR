library(readxl)
library(ggplot2)

data.rls <- read_excel("D:/Kuliah/!yes/R/Analisa Regresi/DataFrame/Anstat_pert4.xlsx", sheet = 1)
View(data.rls)
#Membuat plot dengan sumbu x adalah age (usia) dan sumbu y adalah distance (jarak)
ggplot(data.rls, aes(Age,Distance)) + geom_point() + ggtitle("Hubungan Antara Usia dan Jarak") +
  ylab("Distance")+
  xlab("Age") + theme_classic()

#Membuat scatter plot hubungan antara Age dan Distance

#membuat model regresi
model.reg <- lm(Distance ~ Age, data=data.rls)
model.reg
#Kenaikan usia (age) sebesar 1 satuan akan menyebabkan penurunan jarak yang ditempuh sebesar 3.0008 satuan jarak
summary(model.reg)

#melihat anova
anova(model.reg)

#SK pendugaan parameter regresi
confint(model.reg, level = 0.95) #SK pada taraf 5%

newdata <- data.frame(Age=50)

#SK bagi individu
predict(model.reg, newdata, interval = "prediction")

#SK bagi rataan y
predict(model.reg, newdata, interval = "confidence")

#1. Ukuran Kebaikan Model (R-squared)
#melihat dari summary model
summary(model.reg)
summary(model.reg)[[8]] #langsung

anova(model.reg)
#manual
jkg <- 71096
jkr <- 123871

r_squared <- 1 - (jkg/(jkg+jkr))
r_squared

r_squared.2 <- jkr/(jkg+jkr)
r_squared.2

round(r_squared,3) == round(r_squared.2,3)

#Khusus RLS
korelasi <- cor(data.rls$Age, data.rls$Distance)

r_squared.3 <- korelasi^2
r_squared.3