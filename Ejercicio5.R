
#install.packages("lawstat")

rm(list=ls())
c_n_1 <- read.delim("C:/OneDrive/OneDrive - UNED/4INF/UNED/MICD/MODSTAT/2019_2020_med_pte_datos_plataforma/c_n_1.txt")
View(c_n_1)
datos = c_n_1

attach(datos)

ind1=which(exp==1);
ind2=which(exp==2);
ind3=which(exp==3);
n1=length(rta[ind1]); n1
n2=length(rta[ind2]); n2
n3=length(rta[ind3]); n3
tapply(rta,exp,mean); tapply(rta,exp,sd)

shapiro.test(rta[ind1]); shapiro.test(rta[ind2]); shapiro.test(rta[ind3])
library(lawstat)
levene.test(rta,exp,location="mean")

summary(aov(rta~factor(exp)))

pvalue = 1-pf(0.2763, 2,19)

pvalue

exp2=1*(exp==2)
exp3=1*(exp==3)
summary(lm(data=datos, formula=rta ~ exp2+exp3))


b2 =  22.60000  - 25.85714

b2

eei = sqrt( 84.50827/7)
eei

dem = (1/7)+(1/10)
ee2 = sqrt( 84.50827*dem)
ee2

dem = (1/7)+(1/5)
ee3 = sqrt( 84.50827*dem)
ee3

dem = (1/7)
tint = 25.85714/sqrt( 84.50827*dem)
tint  

dem = (1/7)+(1/10)
texp2 = 0.34286/sqrt( 84.50827*dem)
texp2  

dem = (1/7)+(1/5)
texp3 =-3.25714/sqrt( 84.50827*dem)
texp3  

rse = sqrt (84.50827)
rse

rcuad =  (46.7/(46.7+1605.7))
rcuad


rcuadnum = (1-rcuad)*(21)
rcuadnum
dem = (22-3)
rcuadadj = 1-(rcuadnum/dem)
rcuadadj 



