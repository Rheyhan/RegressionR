library(datarium)
View(marketing)
x<-marketing[,c("youtube","sales")]
View(x)

#---------------------OLS------------------------
model<-lm(sales~youtube, x)
library(ggplot2)
ggplot(x) + 
  geom_point(mapping = aes(youtube, sales)) +
  geom_smooth(mapping = aes(youtube,sales), 
              method=lm, se=F)+
  labs(title = "Scatterplot of Sales ~ Youtube")


#Cek homoscedasticity
library(dplyr)
data.frame(y = rstandard(model),
           x = model$fitted.values) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")


#---------------------WLS------------------------
#perhitungan weights untuk sisaan berbentuk megafon
resid_abs <- abs(model$residuals)
fitted_val <- model$fitted.values
fit <- lm(resid_abs ~ fitted_val, x)
data.weights <- 1 / fit$fitted.values^2
#model
model.lmw <- lm(sales~ youtube, 
                data = x, 
                weights = data.weights)


#---Perbandingan Sisaan pad OLS dan WLS------------
par(mfrow=c(1,2))
plot(y = rstandard(model),
     x = model$fitted.values,
     main="OLS")
abline(h=0, col="red")
plot(y = rstandard(model.lmw),
     x = model.lmw$fitted.values,
     main="WLS")
abline(h=0, col="red")
par(mfrow=c(1,1))

#atau
res1<-data.frame(yduga = model$fitted.values,
                 sisaan=rstandard(model),
                 regresi=rep("OLS",dim(x)[1]))
res2<-data.frame(yduga = model.lmw$fitted.values,
                 sisaan=rstandard(model.lmw),
                 regresi=rep("WLS",dim(x)[1]))
dres<-rbind(res1,res2)

ggplot(data = dres) + 
  geom_point(mapping = aes(x = yduga, y = sisaan, color = regresi))+
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")



ggplot(data = x, aes(youtube, sales)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE,
              color = "blue", 
              size = 0.9) +
  geom_smooth(method = lm, se = FALSE, 
              aes(weight = data.weights),
              color = "red", 
              size = 0.9)+
  labs(title = "Scatterplot of Sales ~ Youtube")

#======pola parabola======
#Pola parabola dengan B2 >0
x<-seq(1:50)
set.seed(1)
e<-runif(n=50,min=-500,max=2000)
y<- 1 + (2.3*x) + (5*x^2)+e
ggplot()+
  geom_point(aes(x=x, y=y),col="blue")+
  labs(title = "Scatterplot peubah X dan Y")

#transformasi y diperkecil
akar_y<-sqrt(y)
dtrans1<-data.frame(x,akar_y)
ggplot(data = dtrans1)+
  geom_point(aes(x=x, y=akar_y),col="blue")+
  labs(title = "Transformasi memperkecil Y")

#transformasi x diperbesar
x_kuadrat<-x^2
dtrans2<-data.frame(x_kuadrat,y)
ggplot(data = dtrans2)+
  geom_point(aes(x=x_kuadrat, y=y),col="blue")+
  labs(title = "Transformasi memperbesar X")



#=======pola hiperbola======
x1<-x
set.seed(1)
e1<-runif(50, min =-0.01, max=0.01)
y1<-(x1/(1.2+2*x1))+e1

ggplot()+
  geom_point(aes(x=x1, y=y1),col="blue")+
  labs(title = "Scatterplot y1 terhadap x1")

#transformasi y=1/y1 dan x=1/x1
ggplot()+
  geom_point(aes(x=1/x1, y=1/y1),col="blue")+
  labs(title = "Scatterplot 1/y1 terhadap 1/x1")


#======pola eksplonensial=====
x2<-x
set.seed(1)
e2<-runif(50, min = 0, max = 15)
y2<-(1.2*exp(0.1*x))+e2
ggplot()+
  geom_point(aes(x=x2, y=y2),col="red")+
  labs(title = "Scatterplot y2 terhadap x2")
#transformasi y=ln(y2)
ggplot()+
  geom_point(aes(x=x2, y=log(y2)),col="red")+
  labs(title = "Scatterplot ln(y2) terhadap x2")


#=======pola pangkat========
#pembangkitan data
x3<-x
set.seed(1)
e3<-runif(50, min =0, max=10000)
y3<-1.3*(x^3.5)
ggplot()+
  geom_point(aes(x=x3, y=y3),col="coral")+
  labs(title = "Scatterplot y3 terhadap x3")

#transformasi y=ln(y3), x=ln(x3)
ggplot()+
  geom_point(aes(x=log(x3), y=log(y3)),col="coral")+
  labs(title = "Scatterplot ln(y3) terhadap ln(x3")

#======pola kebalikan eksplonensial=====
x4<-x
set.seed(1)
e4<-runif(50, min = 0, max = 0.02)
y4<-(1.2*exp(0.1/x))+e4
ggplot()+
  geom_point(aes(x=x4, y=y4),col="red")+
  labs(title = "Scatterplot y4 terhadap x4")
#transformasi y=ln(y4) terhadap 1/x4
ggplot()+
  geom_point(aes(x=1/x4, y=log(y4)),col="red")+
  labs(title = "Scatterplot ln(y4) terhadap 1/x4")



#PENDUGAAN PARAMETER
#contoh
library(readxl)

data<-read_excel("D:/Kuliah/!yes/R/Analisa Regresi/DataFrame/Data Prak 7.xlsx")
ggplot(data=data)+
  geom_point(aes(x=x, y=y),col="dark blue")+
  labs(title = "Scatterplot DC output terhadap Wind Velocity")

#transformasi x=1/x
ggplot(data=data)+
  geom_point(aes(x=1/x, y=y),col="dark blue")+
  labs(title = "Scatterplot DC output terhadap 1/Wind Velocity")



#model linier sederhana data asli
model_dataasli<-lm(data$y~data$x)
plot(model_dataasli)
resummary(model_dataasli)
#model linier sederhada data transformasi
xbaru<-1/data$x
databaru<-data.frame(xbaru,data$y)
model<-lm(data$y~xbaru)

#uji kenormalan
shapiro.test(residuals(model)) #ho: sisaan menyebar normal
tseries::jarque.bera.test(resid(model))#ho: sisaan menyebar normal
car::qqPlot(resid(model),distribution="norm",main="Normal QQ Plot") 

#uji kehomogenan ragam
library(dplyr)
data.frame(rstandard(model),
           model$fitted.values) %>%
  ggplot(aes(x = model$fitted.values, y = rstandard(model))) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")

#nilai harapan = nol
t.test(resid(model), mu = 0,) #h0: nilai harapan sisaan=0


#cek autokorelasi
lmtest::dwtest(databaru$data.y~databaru$xbaru) #h0:tidak ada auto korelasi



