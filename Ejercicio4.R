

#install.packages("lawstat")

rm(list=ls())
c_d_1 <- read.delim("C:/OneDrive/OneDrive - UNED/4INF/UNED/MICD/MODSTAT/2019_2020_med_pte_datos_plataforma/c_d_1.txt")
datos=c_d_1
attach(datos)
ind1=which(exp==1)
ind2=which(exp==2)
n1=length(rta[ind1]); n1
n2=length(rta[ind2]); n2
tapply(rta,exp,mean)
tapply(rta,exp,sd)

shapiro.test(rta[ind1])
shapiro.test(rta[ind2])
library(lawstat)
levene.test(rta,exp,location="mean")


t.test(rta[ind1],rta[ind2],var.equal=TRUE)

exp2=1*(exp==2)
summary(lm(data = datos,formula = rta ~ exp2))


tsq = (-0.075009)^2

Rsq = tsq/(tsq+15)
Rsqadjf = ((1-Rsq)*(17-1))
Rsqadjf/15

Rsqadj = 1-(((1-Rsq)*(17-1))/15)
tsq
Rsq
Rsqadj



