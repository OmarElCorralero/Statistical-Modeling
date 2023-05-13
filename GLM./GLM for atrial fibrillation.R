datoh<- read.csv("/Users/omarguerra/Documents/2022/cursos semestre 9/modelación 2/2da parte prof martiza/informe 1/heart.csv",header=TRUE)

attach(datoh)
head(datoh)
X1<-heart.rate
Y1<-atrialfibrillation

# Establecemos los modelos sumando en cada vez una nueva covariable

modelo1h <- glm(Y1~X1,data=datoh,family=binomial(link=logit))
modelo2h <- glm(Y1~X1+depression,family=binomial(link=logit))
modelo3h <- glm(Y1~X1+depression+Hyperlipemia,family=binomial(link=logit))
modelo4h <- glm(Y1~X1+depression+Hyperlipemia+age,family=binomial(link=logit))
modelo5h <- glm(Y1~X1+depression+Hyperlipemia+age+COPD,family=binomial
(link=logit))

# Mostramos las relaciones entre variables mediante nube puntos

plot(X1,Y1)
abline(modelo1h,col='red')
plot(depression,Y1)
abline(modelo2h,col='red')
plot(Hyperlipemia,Y1)
abline(modelo3h,col='red')
plot(age,Y1)
abline(modelo4h,col='red')

# Hacemos un resumen por cada modelo para estudiar

summary(modelo1h)
summary(modelo2h)
summary(modelo3h)
summary(modelo4h)
summary(modelo5h)

# Con anova vemos nuestro mejor modelo

anova(modelo1h,modelo2h,modelo3h,modelo4h,modelo5h,test="Chi") 

# generamos un intervalo de confianza

confint(modelo4h)

# Relacion entre Edad y fibrilación
plot(density(datoh$age[datoh$atrialfibrillation ==0],adjust=3),
     main="Relacion entre Edad y fibrilación")
lines(density(datoh$age[datoh$atrialfibrillation== 1],adjust=3),
      main="Relacion entre Edad y fibrilación",lty=4,col="red")
hist(datoh$age[datoh$atrialfibrillation ==0],main="Relacion entre Edad y 
fibrilación")

# Relacion entre Edad y Falla Renal
plot(density(datoh$age[datoh$Renal.failure ==0],adjust=3),
     main="Edad y Falla Renal")
lines(density(datoh$age[datoh$Renal.failure == 1],adjust=3),
      main="Edad y Falla Renal",lty=4,col="red")
hist(datoh$age[datoh$Renal.failure ==0],main="Edad y Falla Renal")

# Relación entre Edad y Depresión

plot(density(datoh$age[datoh$depression ==0],adjust=3),
     main="Edad y Depresión")
lines(density(datoh$age[datoh$depression == 1],adjust=3),
      main="Edad y Depresión",lty=4,col="red")
hist(datoh$age[datoh$depression ==0],main="Edad y Depresión")

# Relación entre Edad y Hyperlipemia

plot(density(datoh$age[datoh$Hyperlipemia ==0],adjust=3),
     main="Edad y Hyperlipemia")
lines(density(datoh$age[datoh$Hyperlipemia == 1],adjust=3),
      main="Edad y Hyperlipemia",lty=4,col="red")
hist(datoh$age[datoh$Hyperlipemia ==0],main="Edad y Falla Renal")


# Tabla de frecuencia por depresion:
table(atrialfibrillation,depression)
colSums(table(atrialfibrillation,depression))
# Proporcion firbi por depre:
Odds.psd <- 52/88 # con depre
Odds.pnd <- 479/558 # sin depre

psd
pnd

# generamos un ultimo modelo que relacione la variable respuesta con una de 
# interés propia (depresion)


modelo6h<- glm(atrialfibrillation~depression,data=datoh, family="binomial")
summary(modelo6h)
