
rm(list=ls()) 
detach(datos)

datos=read.table('c_ccd.txt',header=T)
attach(datos)

modelo1 <- lm(rta ~ ., data=datos)
summary(modelo1)


modelo2 <- lm(rta ~ exp1 + exp3, data=datos)
summary(modelo2)

exp1x3 = exp1 * exp3

modelo3 <- lm(rta ~ exp1 + exp3 + exp1x3, data=datos)
summary(modelo3)


exp3Fitness = 1 * (exp3==2)

modelo4 <- lm(rta ~ exp1 + exp2 + exp3Fitness, data=datos)
summary(modelo4)

modelo5 <- lm(rta ~ exp1 + exp3Fitness, data=datos)
summary(modelo5)

exp1x3Fit = exp1 * exp3Fitness

modelo6 <- lm(rta ~ exp1 + exp3 + exp1x3Fit, data=datos)
summary(modelo6)

