library(randtests)
#1Normal tidak pakai standarisasi
df<-read.csv("https://raw.githubusercontent.com/Rheyhan/RegressionR/main/Tugas%20Akhir/Data%20Explor.csv")
df

model.rlb<- lm(Y ~ X1+X2+X3+X4, df)
summary(model.rlb)
AIC(model.rlb)

runs.test(model.rlb$residuals) #runtest

  #transform 1/x 1/y
df<-read.csv("https://raw.githubusercontent.com/Rheyhan/RegressionR/main/Tugas%20Akhir/Datatrans.csv")
model.rlb<- lm(Y ~ X1+X2+X3+X4, df)
summary(model.rlb)
AIC(model.rlb)

runs.test(model.rlb$residuals) #runtest

#2 scaled (X distandarisasi)---------------------------------------------------------
df1<-read.csv("https://raw.githubusercontent.com/Rheyhan/RegressionR/main/Tugas%20Akhir/Data%20Explor.csv")
df2<- scale(df1[3:6])
df<- data.frame(df1[1:2],
                df2)

model.rlb<- lm(Y ~ X1+X2+X3+X4, df)
summary(model.rlb)
AIC(model.rlb)

runs.test(model.rlb$residuals) #runtest

  #transform 1/x 1/y
df<-read.csv("https://raw.githubusercontent.com/Rheyhan/RegressionR/main/Tugas%20Akhir/Datatrans1.csv")
model.rlb<- lm(Y ~ X1+X2+X3+X4, df)
summary(model.rlb)
AIC(model.rlb)

runs.test(model.rlb$residuals) #runtest


#3 scaled (Y dan X distandarisasi)--------------------------------------------------------------
df1<-read.csv("https://raw.githubusercontent.com/Rheyhan/RegressionR/main/Tugas%20Akhir/Data%20Explor.csv")
df2<- scale(df1[2:6])
df<- data.frame(df1[1],
                df2)

model.rlb<- lm(Y ~ X1+X2+X3+X4, df)
summary(model.rlb)
AIC(model.rlb)

runs.test(model.rlb$residuals) #runtest

  #transform 1/x 1/y
df<-read.csv("https://raw.githubusercontent.com/Rheyhan/RegressionR/main/Tugas%20Akhir/Datatrans2.csv")
model.rlb<- lm(Y ~ X1+X2+X3+X4, df)
summary(model.rlb)
AIC(model.rlb)

runs.test(model.rlb$residuals) #runtest
