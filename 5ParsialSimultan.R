data<-read.table(header=T,text ="
y	x1	x2
16.68	7	560
11.5	3	220
12.03	3	340
14.88	4	80
13.75	6	150
18.11	7	330
8	2	110
17.83	7	210
79.24	30	1460
21.5	5	605
40.33	16	688
21	10	215
13.5	4	255
19.75	6	462
24	9	448
29	10	776
15.35	6	200
19	7	132
9.5	3	36
35.1	17	770
17.9	10	140
52.32	26	810
18.75	9	450
19.83	8	635
10.75	4	150
")
n<-nrow(data)
k<-2 #banyak peubah bebas
model<-lm(y~x1+x2,data)

X<-cbind(rep(1,n), data$x1,data$x2)
y<-as.matrix(data$y)

# uji hipotesis simultan
reg.lengkap <- lm(y ~ ., data = data)
anova(reg.lengkap)
summary(reg.lengkap)

# uji hipotesis parsial
reg.x1 <- lm(y ~ x2, data = data)
anova(reg.x1, reg.lengkap)
summary(reg.x1)

reg.x2 <- lm(y ~ x1, data = data)
anova(reg.x2, reg.lengkap)
summary(reg.x2)

#Uji hipotesis manual
jkreg <- t(betaduga)%*%(t(X)%*%y)-((sum(y)^2)/nrow(y))
jkreg
jktot <- t(y)%*%y-((sum(y)^2)/nrow(y))
jktot
jksis <- jktot-jkreg
jksis

#KT
ktreg <- jkreg/2
ktreg
ktsis <-jksis/22
ktsis

#Fhitung
fhit <- ktreg/ktsis
fhit
qf(0.95,2,22)
anova(model)
summary(model)

qt(0.975,22)

thit_b0 <- betaduga[1]/sqrt(ktsis*solve(t(X)%*%X)[1,1]) 
thit_b0

thit_b1 <- betaduga[2]/sqrt(ktsis*solve(t(X)%*%X)[2,2]) 
thit_b1

thit_b2 <- betaduga[3]/sqrt(ktsis*solve(t(X)%*%X)[3,3]) 
thit_b2

# pendugaan selang kepercayaan bagi rataan y saat x diketahui
min(data$x1);max(data$x1)
min(data$x2);max(data$x2)
newdata <- data.frame(x1 = 20, x2=1200)
predict(model, newdata, interval="confidence")

# pendugaan selang kepercayaan bagi y  saat x diketahui
newdata <- data.frame(x1 = 20, x2=1200)
predict(model, newdata, interval="prediction")

#=====================part 3================================
##DATA CONTOH 2
#Uji F Parsial
data2<-read.delim2("clipboard")
data2<-read.table(header=T,text ="
Y	X1	X2	X3	X4
78.5	7	26	6	6
74.3	1	29	15	52
104.3	11	56	8	20
87.6	11	31	8	47
95.9	7	52	6	33
109.2	11	55	9	22
102.7	3	71	17	6
72.5	1	31	22	44
93.1	2	54	18	22
115.9	21	47	4	26
83.8	1	40	23	34
113.1	11	66	9	12
109.4	10	68	8	12
")
#menguji peubah X3
#H0: b3=0
#H1: b3!=0
model_penuh<-lm(Y~., data2)
model_penuh
model_tdk_penuh<-lm(Y~X1+X2+X4, data2) #model tanpa X3
model_tdk_penuh
a<- anova(model_penuh)
b<- anova(model_tdk_penuh)
a
b
fhit<-((sum(sum(a$`Sum Sq`[1:4]),sum(b$`Sum Sq`[1:3])))/1)/a$`Mean Sq`[5]
ftabel<-qf(0.05, 1, 8, lower.tail = F)
kesimpulan<-function(fhit,ftabel){
  kes<-NULL
  if(fhit>=ftabel){
    kes<-"Tolak H0, pada taraf nyata 5%"
  }
  else{
    kes<-"Tak tolak H0, pada taraf nyata 5%"
  }
  return(kes)
}
kesimpulan(fhit, ftabel)

#menguji peubah x3 dan x4
#H0: b3=b4=0
#H1: minimal ada satu di antara b3 atau b4 !=0
model_penuh<-lm(Y~., data2)
model_tdk_penuh<-lm(Y~X1+X2, data2) # model tanpa x3 dan x4
a<-anova(model_penuh)
b<-anova(model_tdk_penuh)

fhit<-((sum(sum(a$`Sum Sq`[1:4]),sum(b$`Sum Sq`[1:2])/2)))/a$`Mean Sq`[5]
ftabel<-qf(0.05, 2,8, lower.tail = F)
fhit
kesimpulan<-function(fhit,ftabel){
  kes<-NULL
  if(fhit>=ftabel){
    kes<-"Tolak H0, pada taraf nyata 5%"
  }
  else{
    kes<-"Tak tolak H0, pada taraf nyata 5%"
  }
  return(kes)
}
kesimpulan(fhit, ftabel)

#menguji peubah x2 dan x4
#H0: b2=b4=0
#H1: minimal ada satu di antara b2 atau b4 !=0
model_penuh<-lm(Y~., data2)
model_tdk_penuh<-lm(Y~X1+X3, data2) # model tanpa x2 dan x4
a<-anova(model_penuh)
b<-anova(model_tdk_penuh)

fhit<-((sum(sum(a$`Sum Sq`[1:4]),sum(b$`Sum Sq`[1:2])))/2)/a$`Mean Sq`[5]
ftabel<-qf(0.05, 2,8, lower.tail = F)
kesimpulan<-function(fhit,ftabel){
  kes<-NULL
  if(fhit>=ftabel){
    kes<-"Tolak H0, pada taraf nyata 5%"
  }
  else{
    kes<-"Tak tolak H0, pada taraf nyata 5%"
  }
  return(kes)
}
kesimpulan(fhit, ftabel)

#MULTIKOLINEARITAS
library(car)
vif(model_penuh)
#tidak ada yang >10, tidak terindikasi adanya multokolinearitas

#DATA CONTOH 3
#DUMMY VARIABLE
data3<-read.delim2("clipboard")
data3<-read.table(header=T,text ="
bidang	usia	gaji
0	23	115
0	21	245
0	21	315
0	22	365
0	24	575
0	25	385
0	25	425
0	26	350.5
0	29	587
0	30	985
0	30	635
0	30	695
0	30	785
0	30	402.5
0	31	612.5
1	20	425
1	24	680
1	27	623
1	29	651.5
1	31	620
1	30	606.5
1	33	1055
1	33	565
")
model<-lm(gaji~., data3)
summary(model)
