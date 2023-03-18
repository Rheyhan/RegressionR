#Singkat Punya Rhey
library(readxl)

data <- read.table(header=T,text ="
Serial	CTRP	P	R
1	133	111600	1197576
2	111	104400	1053648
3	129	97200	1124172
4	117	79200	987144
5	130	126000	1283616
6	154	108000	1295100
7	149	147600	1407444
8	90	104400	922416
9	118	169200	1272012
10	131	75600	1064856
11	141	133200	1269960
12	119	133200	1064760
13	115	176400	1207488
14	102	180000	1186284
15	129	133200	1231464
16	144	147600	1296708
17	153	122400	1320648
18	96	158400	1102704
19	104	165600	1184316
20	156	104400	1326360
21	119	136800	1162596
22	125	115200	1195116
23	130	115200	1134768
24	123	151200	1269024
25	128	97200	1118688
26	97	122400	904776
27	124	208800	1357644
28	138	93600	1027308
29	137	115200	1181976
30	129	118800	1221636
31	97	129600	1060452
32	133	100800	1229028
33	145	147600	1406169
34	149	126000	1293936
35	122	108000	1056384
36	120	194400	1415316
37	128	176400	1338060
38	117	172800	1457400
")

#Cari Koef Manual
y <- as.matrix(data[,4])
y
x <- as.matrix(cbind(rep(1,nrow(data)), data[,2], data[,3]))
x

xtx <- t(x)%*%x
xtx

xtx.inv <- solve(xtx)
xtx.inv

xty <- t(x)%*%y
xty

b <- xtx.inv%*%xty
b

#Otomatis
model.rlb <- lm(R ~ CTRP+P, data=data) #Assign intercept=R, peubah ctrp dan p 
model.rlb                      #Koefisiennya
summary(model.rlb)           #Uji T, uji f, R, R^2
Anova(model.rlb)             #Uji F masing-masing peubah
qf(0.95,2,35)  #Ftab f(a;k;n-k-1)

# pemdugaan selang kepercayaan bagi parameter
confint(model.rlb, level = 0.95)

# pendugaan selang kepercayaan bagi rataan y saat x diketahu
newdata <- data.frame(CTRP = 120, P = 111000)
predict(model.rlb, newdata, interval="confidence")

# pendugaan selang kepercayaan bagi y  saat x diketahui
predict(model.rlb, newdata, interval="prediction")

#============Deteksi pencilan dan leverage=============
#titik pencilan (xnya sama dengan variabel terdekat tapi Ynya jauh)
sisaan<-residuals(model.rlb)
stdr.sisaan<-rstandard(model.rlb)
mut.stdr.sisaan<-abs(stdr.sisaan)
data.frame(sort(mut.stdr.sisaan)) #nunjukin yang normal

#titik leverage (xnya kejauhan, tapi ynya sesuai)
hi<-hatvalues(model.rlb, infl = influence(model.rlb))
ambang_batas<-2*2/38 #2p/n, p=3, n=25
data.frame(hi, hi>ambang_batas) #amatan 9 dan 22 = titik leverage


#titik pencilan dan leverage (Ini plot nunjukkin normal, leverage, ama outlier)
library(olsrr)
ols_plot_resid_lev(model.rlb)

#=====================================Asumsi====================================
# Asumsi GAUSS MARKOV 
  # 1 Nilai harapan sisaan sama dengan nol
t.test(model.rlb$residuals,
       mu = 0,
       conf.level = 0.95)

  # 2 Ragam sisaan homogen
cek.homogen = lm(formula = abs(model.rlb$residuals) ~ R, # y: abs residual, x: youtube
                 data = data)
summary(cek.homogen)
library(lmtest)
bptest(model.rlb)
library(car)
ncvTest(model.rlb)

# 3 Sisaan saling bebas 
# Bisa pake Run Test atau Durbin Watson
library(randtests)
runs.test(model.rlb$residuals) #runtest
library(lmtest)
dwtest(model.rlb) #Durbin Watson
 


# ASUMSI NORMALITAS SISAAN
library(dgof)
ks.test(model.reg$residuals, "pnorm", mean=mean(model.reg$residuals), sd=sd(model.reg$residuals))
library(car)
shapiro.test(model.rlb$residuals)

#Tidak ada MULTIKOLINEARITAS
library(car)
vif(model.rlb) #Jika VIF>10 ada multikolinearitas
library(olsrr)

#===============Matriks================================= (Manual) masbro

jkreg <- t(b)%*%xty-((sum(y)^2)/nrow(y))
jkreg
jktot <- t(y)%*%y-((sum(y)^2)/nrow(y))
jktot
jksis <- jktot-jkreg
jksis

#KT
ktreg <- jkreg/2
ktreg
ktsis <-jksis/35
ktsis

#Fhitung
fhit <- ktreg/ktsis
fhit
qf(0.95,2,35)
anova(model.rlb)
summary(model.rlb)

qt(0.975,35)

thit_b0 <- b[1]/sqrt(ktsis*xtx.inv[1,1]) 
thit_b0

thit_b1 <- b[2]/sqrt(ktsis*xtx.inv[2,2]) 
thit_b1

thit_b2 <- b[3]/sqrt(ktsis*xtx.inv[3,3]) 
thit_b2