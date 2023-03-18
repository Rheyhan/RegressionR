###----Impor Data----####
wood <- read.table(header = T, text = "
dbh vol
10.20 25.93
13.72 45.87
15.43 56.20
14.37 58.60
15.00 63.36
15.02 46.35
15.12 68.99
15.24 62.91
15.24 58.13
15.28 59.79
13.78 56.20
15.67 66.16
15.67 62.18
15.98 57.01
16.50 65.62
16.87 65.03
17.26 66.74
17.28 73.38
17.87 82.87
19.13 95.71")

#mengganti nama kolom
colnames(wood) <- c("x","y")

model <- lm(y~x, data=wood)
summary(model)

#1. Ukuran Kebaikan Model (R-squared)
#melihat dari summary model
summary(model)
summary(model)[[8]] #langsung

anova(model)
#manual
jkg <- 658.57
jkr <- 3085.79

r_squared <- 1 - (jkg/(jkg+jkr))
r_squared

r_squared.2 <- jkr/(jkg+jkr)
r_squared.2

round(r_squared,3) == round(r_squared.2,3)

##====KHUSUS RLS======
korelasi <- cor(wood$x, wood$y)

r_squared.3 <- korelasi^2
r_squared.3

#2. Prediksi Nilai Y dan SK
#memasukkan x tertentu
min(wood$x)
max(wood$x)

newdata <- data.frame(x=12)


#SK bagi individu
predict(model, newdata, interval = "prediction")


#SK bagi rataan y
predict(model, newdata, interval = "confidence")




#######################
x <- rnorm(150); y <- rnorm(length(x)); 
n=length(x)
# for simple linear regression confidence intervals are calculated as follows.
fit<-lm(y~x)
summary(fit)
# Automatic calculation 
AutoConfidence <- data.frame(predict(fit, interval="confidence"))
# Manual calculation of confidence interval 
Estimate <- predict(fit)


ManualUpperLimit <- Estimate + qt(.975, df = n-2)* summary(fit)$sigma * sqrt(1/length(x) + (x-mean(x))^2 / sum((x-mean(x))^2))
ManualLowerLimit <- Estimate - qt(.975, df = n-2)* summary(fit)$sigma * sqrt(1/length(x) + (x-mean(x))^2 / sum((x-mean(x))^2))

selang_kepercayaan=cbind(ManualLowerLimit,Estimate,ManualUpperLimit); head(selang_kepercayaan)

#Latihan: Ganti y <- rnorm(length(x)) dengan y=2+0.7*x+ rnorm(length(x)) 

set.seed(001)
