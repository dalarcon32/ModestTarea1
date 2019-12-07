install.packages("QuantPsyc")
library(QuantPsyc)
rm(list=ls()) 
detach(datos)

datos=read.table('c_ccd.txt',header=T)
attach(datos)

exp3Fitness = 1 * (exp3==2)

########## modelo 1
model1 <- lm(rta ~ exp1 + exp2 + exp3Fitness, data=datos)
summary(model1)
lm.beta(model1)

########## modelo 2
model2 <- lm(rta ~ exp1 + exp3Fitness, data=datos)
summary(model2)
lm.beta(model2)

########## modelo 3
exp1x3 = exp1 * exp3Fitness

model3 <- lm(rta ~ exp1 + exp3Fitness + exp1x3, data=datos)
summary(model3)
lm.beta(model3)

########## Condiciones RL: Independencia
install.packages("lmtest")
library(lmtest) 

dwtest(model2)# esta es la función de la prueba para autocorrelación



########## Condiciones RL: homocedasticidad
#install.packages("MASS")
ajustados_2 <- fitted(model2)
library(MASS)
residuos_2 <- stdres(model2)

plot(ajustados_2, abs(residuos_2), pch = 16, col = 'darkcyan',
     xlab = 'Peso en Kg', ylab = 'abs(Residuos)', main = 'homocedasticidad')
lines(loess.smooth(ajustados_2, abs(residuos_2)), col = 'red3', lwd = 3, lty = 3)


library(lmtest)
bptest(model2)

########## Condiciones RL: Normalidad
qqnorm(residuos_2)
qqline(residuos_2)

shapiro.test(residuos_2)



