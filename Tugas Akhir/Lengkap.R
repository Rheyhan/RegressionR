#eksplorasi

df=read.csv("https://raw.githubusercontent.com/Rheyhan/RegressionR/main/Tugas%20Akhir/Data%20Explor.csv")

View(df)
#automatis
model.rlb <- lm(Y ~ X1+X2+X3+X4, data=df) #Assign intercept=R, peubah ctrp dan p 
model.rlb                      #Koefisiennya
summary(model.rlb)           #Uji T, uji f, R, R^2
anova(model.rlb)             #Uji F masing-masing peubah
qf(0.95,2,35)  #Ftab f(a;k;n-k-1)
  

library(olsrr)


#titik pencilan dan leverage
ols_plot_resid_lev(model.rlb)

#amatan berpengaruh
ols_plot_dffits(model.rlb)
ols_plot_dfbetas(model.rlb)

#matrix corr
library(metan)
plot(corr_coef(df))

#boxplot
boxplot(df$Y)

#=====================================Asumsi====================================
# Asumsi GAUSS MARKOV 
# 1 Nilai harapan sisaan sama dengan nol
t.test(model.rlb$residuals,
       mu = 0,
       conf.level = 0.95)

# 2 Ragam sisaan homogen
library(lmtest)
bptest(model.rlb)

library("ggplot2")
data.frame(rstandard(model.rlb),
           model.rlb$fitted.values) %>%
  ggplot(aes(x = model.rlb$fitted.values, y = rstandard(model.rlb))) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")

# 3 Sisaan saling bebas 
# Bisa pake Run Test atau Durbin Watson
library(randtests)
runs.test(model.rlb$residuals) #runtest


# ASUMSI NORMALITAS SISAAN
library(car)
shapiro.test(model.rlb$residuals)

car::qqPlot(resid(model.rlb),distribution="norm",main="Normal QQ Plot") 

#Tidak ada MULTIKOLINEARITAS
library(car)
vif(model.rlb) #Jika VIF>10 ada multikolinearitas
library(olsrr)

#penanganan-----------------------------------------------------------------------------------------------
dftrans<-read.csv("https://raw.githubusercontent.com/Rheyhan/RegressionR/main/Tugas%20Akhir/Datatrans.csv")
modeltrans<-lm(Y~X1+X2+X3+X4, dftrans)

#=====================================Asumsi====================================
# Asumsi GAUSS MARKOV 
# 1 Nilai harapan sisaan sama dengan nol
t.test(modeltrans$residuals,
       mu = 0,
       conf.level = 0.95)

# 2 Ragam sisaan homogen
library(lmtest)
bptest(modeltrans)

library(ggplot2)
data.frame(rstandard(modeltrans),
           modeltrans$fitted.values) %>%
  ggplot(aes(x = modeltrans$fitted.values, y = rstandard(modeltrans))) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")

# 3 Sisaan saling bebas 
# Bisa pake Run Test atau Durbin Watson
library(randtests)
runs.test(modeltrans$residuals) #runtest

# ASUMSI NORMALITAS SISAAN
library(car)
shapiro.test(modeltrans$residuals)
car::qqPlot(resid(modeltrans),distribution="norm",main="Normal QQ Plot") 

#Tidak ada MULTIKOLINEARITAS
vif(modeltrans) #Jika VIF>10 ada multikolinearitas


  #---Stepwise Regression Model----
library(MASS)
step.model <- stepAIC(modeltrans, direction = "both", trace = F)
summary(step.model) #model terbaik 