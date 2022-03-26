#Autores:
        # Daniel López Montero
        # Rodrigo de la Nuez Moraleda
        # José García Rebollo
        # David Parro Plaza
install.packages("faraway")
install.packages("leaps")
install.packages("MASS")
install.packages("PASWR")
install.packages("car")
install.packages("ggplot2")
install.packages("GGally")
install.packages("corrplot")
install.packages("scatterplot3d")
install.packages("lmtest")
install.packages("plotly")
install.packages("mixlm")


library(faraway)
library(leaps)
library(MASS)
library(PASWR)
library(car)
library(ggplot2)
library(GGally)
library(corrplot)
library(plotly)
library(scatterplot3d)
library(lmtest)
library(mixlm)





# 1) Introduccion

data(cheddar)
attach(cheddar)

# Variable Respuesta: taste
# Variables Predictoras: Acetic, H2S, Lactic


# Estudiamos el tipo de las variables que van a formar parte de los posibles modelos
sapply(cheddar, class) # todas las variables son numericas
head(cheddar)

# Comprobamos que no hay entradas vacias
any(is.na(cheddar))

# De haberlas habido, podriamos haber tomado las siguientes decisiones:
#  - eliminar las observaciones con valores NA
#  - eliminar variables si muchas de las entradas vacias aparecen en ella   
#  - emplear métodos para predecir que valores deberian apercer en dichas entradas



# Histogramas de todas las variables
ids <- names(cheddar)
layout(matrix(1:4, nrow = 1))
y_lab_string <- "Cantidad"
for (id in ids) {
  hist(cheddar[, id], xlab = id, ylab = y_lab_string, main = paste("Histogram of ", id))
  y_lab_string <- ""
}


# Graficas de las relaciones entre variables.
plot(cheddar)

# Graficas de dispersion entre la variable respuesta "taste" y las variables predictoras.
layout(matrix(1:3, nrow = 1))

plot(Acetic, taste,
     main = "Relación entre Taste y Acetic",
     xlab = "Acetic", ylab = "Taste",
     pch = 19, frame = FALSE)


plot(H2S, taste,
     main = "Relación entre Taste y H2S",
     xlab = "H2S", ylab = "Taste",
     pch = 19, frame = FALSE)


plot(Lactic, taste,
     main = "Relación entre Taste y Lactic",
     xlab = "Lactic", ylab = "Taste",
     pch = 19, frame = FALSE)


layout(matrix(1:1, nrow = 1))
summary(cheddar)





# 2) Estudio y evaluacion del modelo completo

x <- model.matrix( ~ Acetic + H2S + Lactic, data = cheddar)
betahat <- solve(crossprod(x, x), crossprod(x, taste))
betahat <- c(betahat)
betahat

# Comprobamos el resultado con funciones ya implementadas
model.all <- lm(taste ~ ., data = cheddar)
summary(model.all)
model.all$coefficients

anova(model.all)



# Intervalos de confianza de las betas del modelo completo
confint(model.all)

# Observamos que 0 esta en el intervalo de confianza de beta_0 y beta_Acetic, planteamos dos tests
#    con hipótesis nula beta_i = 0 y hipotesis alternativa beta_i != 0.

# Comenzamos con beta_Acetic pues el valor estimado es más cercano a 0 que el de beta_0
modnoAcetic <- lm(taste ~ H2S + Lactic, data=cheddar)
anova(modnoAcetic,model.all) # no solo el p-valor > 0.05 sino que de hecho p-valor ~ 1, aceptamos la hipotesis nula.

modnoInterceptor <- lm(taste ~ H2S + Lactic + 0, data=cheddar) # modelo reducido sin interceptor
anova(modnoInterceptor,model.all) # el p-valor es menor que 0.05, luego no es suficiente para rechazar la hipotesis nula


# Veamos los p-valores de las variables predictoras
summary(model.all)$coeff[,4] 
# Observamso que el p-valor de Acetic es el mas elevado, este resultado junto con el contrate de hipotesis que hemos hecho
#  podemos deducir que la variable Acetic no sera muy relevante para el estudio de los distintos modelos que estudiaremos.


# Correlaciones de las variables que intervienen en el modelo completo
cor(cheddar)
ggpairs(cheddar)

mat_cor <- cor(cheddar, method = "pearson")
corrplot(mat_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


# Por ultimo veamos si hay outliers en el modelo completo
Model.all<-fortify(model.all)
outlierTest(model.all) # no los hay

# Por el metodo de Bonferroni
alpha <- 0.05
BCV <- qt(1-alpha/(2*30),26) # el valor critico de Bonferroni t_{1-alpha/2n;n-p-1}, n=30,p=3
BCV
sum(abs(rstudent(model.all))>BCV) # el metodo no nos da ninguna observacion

sort(abs(rstandard(model.all)),decreasing = TRUE)[1:3] # estos valores estan relativamente lejanos de BCV


# Veamos si por influenceIndexPlot() hay alguna observacion que pueda ser relevante
influenceIndexPlot(model.all) # en la tercera grafica se ve que la observacion 15 tiene un comportamiento anormal

# Comprobemos si los supuestos de los modelos de regresion lineal mejoran al retirar esta observacion
#  de nuestro dataset. De ser asi, aceptamos que pueda ser una observacion que pejudique a nuestro modelo.
model.all_15<- lm(taste ~ ., data = cheddar[-15,])

# Homocedasticidad
ncvTest(model.all)
ncvTest(model.all_15) # empeora 

# Normalidad
shapiro.test(resid(model.all))
shapiro.test(resid(model.all_15))# mejora

# Autocorrelacion
durbinWatsonTest(model.all)
durbinWatsonTest(model.all_15)# mejora

# Linealidad
resettest(model.all, power=2:3, type="regressor", data=cheddar) 
resettest(model.all_15, power=2:3, type="regressor", data=cheddar)# empeora

# En el computo global no parece que los p-valores de estos tests den a entender una mejoria general del modelo.
#  Si bien es cierto, tampoco los empeora y dependiendo del criterio a seguir podriamos decidir si es relevante o no.
#  Nosotros hemos decidido no considerar la observacion 15 como tal.





# 3) Seleccion del mejor modelo. Metodos por pasos y por criterios

# Separacion del dataset en conjuntos de entrenamiento y test (70-30%)

# Hemos considerado varias semillas para abarcar más modelos. Además, hemos intentado evitar 
#  en la medida de lo poible que se repitan muchos elementos en los distintos conjuntos de test seleccionados.

set.seed(1) 
train.1 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.1 <- (!train.1)
sum(test.1)
model.all1 <- lm(taste ~ ., data = cheddar[train.1,])

set.seed(1100) 
train.2 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.2 <- (!train.2)
sum(test.2)
sum(test.1==TRUE & test.2==TRUE)#solo 1 preseguimos
model.all2 <- lm(taste ~ ., data = cheddar[train.2,])

set.seed(5) 
train.3 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.3 <- (!train.3)
sum(test.3)
sum(test.1==TRUE & test.3==TRUE)
sum(test.2==TRUE & test.3==TRUE)
model.all3 <- lm(taste ~ ., data = cheddar[train.3,])



# i) BACKWARD (alpha=0.05)

drop1(model.all1, test = "F")
# quitamos Acetic del modelo por ser la de mayor p-valor

model.updateB1 <- update(model.all1, . ~ . - Acetic)
drop1(model.updateB1, test = "F")
# quitamos H2S del modelo por ser la de mayor p-valor

model.updateB2 <- update(model.updateB1, . ~ . - H2S)
drop1(model.updateB2, test = "F")
# termina el proceso pues el p-valor de Lactic no llega a alpha

model.1 <- lm(taste ~ Lactic, data = cheddar[train.1,])
summary(model.1)

# Para evitar repetir este proceso usamos el package mixlm con el que obtenemos los mismos resultados.
model.1 <- mixlm::backward(model.all1, alpha=0.05)
summary(model.1) # LACTIC

model.2 <- mixlm::backward(model.all2, alpha=0.05)
summary(model.2) # H2S + LACTIC

model.3 <- mixlm::backward(model.all3, alpha=0.05)
summary(model.3) # H2S + LACTIC



# ii) FORWARD (alpha=0.05)
SCOPE <- (~ . + Acetic + H2S + Lactic)
model.inicial <- lm(taste ~ 1, data = cheddar[train.1,]) # solo con termino independiente

add1(model.inicial, scope = SCOPE, test = "F")
# Añadimos Lactic por ser la variable predictora con menor p-valor
model.updateF1 <- update(model.inicial, . ~ . + Lactic)

add1(model.updateF1, scope = SCOPE, test = "F")
# No añadimos ninguna variable pues todos los p-valores superan la barrera de alpha

model.1a <- lm(taste ~ Lactic, data = cheddar[train.1,])
summary(model.1a)


# Al igual que en BACKWARD usamos el paquete mixlm para automatizar el proceso de FORWARD.
model.1a <- mixlm::forward(model.all1, alpha=0.05)
summary(model.1a) # LACTIC

model.2a <- mixlm::forward(model.all2, alpha=0.05)
summary(model.2a) # H2S + LACTIC

model.3a <- mixlm::forward(model.all3, alpha=0.05)
summary(model.3a) # H2S + LACTIC

# Nótese que los modelos obtenidos por BACKWARD y FORWARD coinciden para cada conjunto train.



# iii) CRITERIOS

# R2 ajustado
models1 <- regsubsets(taste ~ ., data = cheddar[train.1,])
models2 <- regsubsets(taste ~ ., data = cheddar[train.2,])
models3 <- regsubsets(taste ~ ., data = cheddar[train.3,])

summary(models1)
summary(models2)
summary(models3)

MR2adj1 <- summary(models1)$adjr2
MR2adj2 <- summary(models2)$adjr2
MR2adj3 <- summary(models3)$adjr2

summary(models1)$which[which.max(MR2adj1), ]
summary(models2)$which[which.max(MR2adj2), ]
summary(models3)$which[which.max(MR2adj3), ]



# Cp de Mallows
MCp1 <- summary(models1)$cp
MCp2 <- summary(models2)$cp
MCp3 <- summary(models3)$cp


summary(models1)$which[which.min(MCp1), ]
summary(models2)$which[which.min(MCp2), ]
summary(models3)$which[which.min(MCp3), ]


# Criterio de Informacion de Bayes (BIC)
MBIC1 <- summary(models1)$bic
MBIC2 <- summary(models2)$bic
MBIC3 <- summary(models3)$bic

summary(models1)$which[which.min(MBIC1), ]
summary(models2)$which[which.min(MBIC2), ]
summary(models3)$which[which.min(MBIC3), ]

# Criterio de Informacion de Akaike (AIC)
stepAIC(model.all1, scope = SCOPE, k = 2)
stepAIC(model.all2, scope = SCOPE, k = 2)
stepAIC(model.all3, scope = SCOPE, k = 2)


# Para el conjunto train.1 aparecen modelos con Lactic y H2S + Lactic como variables predictoras, 
#  mientras que para los conjuntos train.2 y train.3 unicamente aparece el modelo taste ~ H2S + Lactic.
model.1crit <- lm(taste ~ H2S + Lactic, data=cheddar[train.1,])

# Esto nos puede llevar a pensar que el modelo final vaya a ser el que utiliza H2S y Lactic.

model.1
model.1crit
model.2
model.3

anova(model.1,model.all1)
anova(model.1crit,model.all1)
anova(model.2,model.all2)
anova(model.3,model.all3)
# De esta forma vemos que los modelos creados son mejores que los completos.





# 4) Diagnostico. Comprobaciones de hipotesis, outliers y observaciones influyentes

# Comprobacion de hipotesis de los  modelos 

# Linealidad

resettest(model.1, power=2:3, type="regressor", data=cheddar[train.1,]) 
resettest(model.1crit, power=2:3, type="regressor", data=cheddar[train.1,]) 
resettest(model.2, power=2:3, type="regressor", data=cheddar[train.2,]) 
resettest(model.3, power=2:3, type="regressor", data=cheddar[train.3,]) 
# p-valores > 0.05 luego aceptamos hipótesis de linealidad en todos los casos

plot(cheddar$H2S, cheddar$taste,
     main = "Relacion entre Taste y H2S",
     xlab = "H2S", ylab = "Taste",
     pch = 19, frame = FALSE) 
plot(cheddar$Lactic, cheddar$taste,
     main = "Relacion entre Taste y Lactic",
     xlab = "Lactic", ylab = "Taste",
     pch = 19, frame = FALSE) 



# Normalidad y Autocorrelacion

shapiro.test(resid(model.1)) # p-valor > 0.05 no se rechaza la hipótesis nula, i.e. la normalidad
qqnorm(resid(model.1))
qqline(resid(model.1))

shapiro.test(resid(model.1crit))#bien
qqnorm(resid(model.1crit))
qqline(resid(model.1crit))

shapiro.test(resid(model.2))#bien
qqnorm(resid(model.2))
qqline(resid(model.2))

shapiro.test(resid(model.3))#bien
qqnorm(resid(model.3))
qqline(resid(model.3))

# Observamos que las colas no siguen el mismo patrón que el resto de datos, lo que podría indicar que 
#  el modelo no sigue una distribución normal. Sin embargo, al no afectar a un gran porcentaje de los datos y
#  teniendo en cuenta el resultado previo obtenido por el Test de Shapiro-Wilk no rechazamos la hipótesis de normalidad.


# Media de errores nula

t.test(resid(model.1), mu = 0, alternative = "two.sided")
t.test(resid(model.1crit), mu = 0, alternative = "two.sided")
t.test(resid(model.2), mu = 0, alternative = "two.sided")
t.test(resid(model.3), mu = 0, alternative = "two.sided")
# p-valores ~ 1 > 0.05 luego aceptamos la hipotesis nula en todos los casos



# Autocorrelacion

durbinWatsonTest(model.1) 
durbinWatsonTest(model.1crit) 
durbinWatsonTest(model.2) 
durbinWatsonTest(model.3) 
# p-valores > 0.05 luego rechazamos que haya autocorrelacion



# Homocedasticidad

fmodel1 <- fortify(model.1)
fmodel1crit <- fortify(model.1crit)
fmodel2 <- fortify(model.2)
fmodel3 <- fortify(model.3)

X1 <- fmodel1$.fitted
Y1 <- fmodel1$.stdresid

X1crit <- fmodel1crit$.fitted
Y1crit <- fmodel1crit$.stdresid

X2 <- fmodel2$.fitted
Y2 <- fmodel2$.stdresid

X3 <- fmodel3$.fitted
Y3 <- fmodel3$.stdresid

plot(X1, Y1, ylab = "Residuos estandarizados", xlab = "valores ajustados")
plot(X1crit, Y1crit, ylab = "Residuos estandarizados", xlab = "valores ajustados")
plot(X2, Y2, ylab = "Residuos estandarizados", xlab = "valores ajustados")#casi casi hay patron
plot(X3, Y3, ylab = "Residuos estandarizados", xlab = "valores ajustados")
# Los residuos se distribuyen de forma homogénea a lo largo de una banda horizontal, luego se verifica la hipótesis nula


ncvTest(model.1) 
ncvTest(model.1crit)
ncvTest(model.2)
ncvTest(model.3)
# p-valores > 0.05, luego no hay evidencia para rechazar que la varianza sea constante



# Outliers

alpha <- 0.05
n1 <- nrow(cheddar[train.1,])
p1 <- nrow(summary(model.1)$coef)

n1crit <- nrow(cheddar[train.1,])
p1crit <- nrow(summary(model.1crit)$coef)

n2 <- nrow(cheddar[train.2,])
p2 <- nrow(summary(model.2)$coef)

n3 <- nrow(cheddar[train.3,])
p3 <- nrow(summary(model.3)$coef)


# El valor critico de Bonferroni t_{1-alpha/2n;n-p-1}
BCV1 <- qt(1 - alpha / (2 * n1), n1-p1-1) 
BCV1crit <- qt(1 - alpha / (2 * n1crit), n1crit-p1crit-1) 
BCV2 <- qt(1 - alpha / (2 * n2), n2-p2-1) 
BCV3 <- qt(1 - alpha / (2 * n3), n3-p3-1) 

sum(abs(rstudent(model.1)) > BCV1)
sum(abs(rstudent(model.1crit)) > BCV1crit)
sum(abs(rstudent(model.2)) > BCV2)
sum(abs(rstudent(model.3)) > BCV3)

outlierTest(model.1)#NA?
outlierTest(model.1crit) #NA?
outlierTest(model.2) 
outlierTest(model.3)
# No hay outliers en ninguno de los dos modelos planteados



# Observaciones Influyentes

# Criterio 1: valores leverage (hii) mayores que 2p/n

X1 <- model.matrix(~ H2S+Lactic, data = cheddar[train.1,])
X1crit <- model.matrix(~ H2S+Lactic, data = cheddar[train.1,])
X2 <- model.matrix(~ H2S+Lactic, data = cheddar[train.2,])
X3 <- model.matrix(~ Lactic, data = cheddar[train.3,])

H1 <- X1 %*% solve(t(X1) %*% X1) %*% t(X1)
H1crit <- X1crit %*% solve(t(X1crit) %*% X1crit) %*% t(X1crit)
H2 <- X2 %*% solve(t(X2) %*% X2) %*% t(X2)
H3 <- X3 %*% solve(t(X3) %*% X3) %*% t(X3)

hii1 <- diag(H1)
hii1crit <- diag(H1crit)
hii2 <- diag(H2)
hii3 <- diag(H3)

hCV1 <- 2 * p1 / n1
hCV1crit <- 2 * p1crit / n1crit
hCV2 <- 2 * p2 / n2
hCV3 <- 2 * p3 / n3

sum(hii1 > hCV1)
which(hii1>hCV1)#1  5 16 23 24 26 

sum(hii1crit > hCV1crit)
which(hii1crit>hCV1crit)

sum(hii2 > hCV2)
which(hii2>hCV2)

sum(hii3 > hCV3)


# Criterio 2: valores |DFFITS| son mayores que 2*sqrt(p/n)

dffitsCV1 <- 2 * sqrt(p1 / n1)
dffitsCV1crit <- 2 * sqrt(p1crit / n1crit)
dffitsCV2 <- 2 * sqrt(p2 / n2)
dffitsCV3 <- 2 * sqrt(p3 / n3)

dffitsmodel1 <- dffits(model.1)
dffitsmodel1crit <- dffits(model.1crit)
dffitsmodel2 <- dffits(model.2)
dffitsmodel3 <- dffits(model.3)

sum(dffitsmodel1 > dffitsCV1)
which(dffitsmodel1 > dffitsCV1)#1 12 24 

sum(dffitsmodel1crit > dffitsCV1crit)
which(dffitsmodel1crit > dffitsCV1crit)#1, 12

sum(dffitsmodel2 > dffitsCV2)
sum(dffitsmodel3 > dffitsCV3)



# Criterio 3: valores |DFBETAS| mayores que 2/sqrt(n)

dfbetaCV1 <- 2 / sqrt(n1)
dfbetaCV1crit <- 2 / sqrt(n1crit)
dfbetaCV2 <- 2 / sqrt(n2)
dfbetaCV3 <- 2 / sqrt(n3)

dfbetamodel1 <- dfbeta(model.1)
dfbetamodel1crit <- dfbeta(model.1crit)
dfbetamodel2 <- dfbeta(model.2)
dfbetamodel3 <- dfbeta(model.3)


sum(dfbetamodel1[, 1] > dfbetaCV1)
sum(dfbetamodel1[, 2] > dfbetaCV1)
which(dfbetamodel1[, 1] > dfbetaCV1)
which(dfbetamodel1[, 2] > dfbetaCV1)

sum(dfbetamodel1crit[, 1] > dfbetaCV1crit)
sum(dfbetamodel1crit[, 2] > dfbetaCV1crit)
sum(dfbetamodel1crit[, 3] > dfbetaCV1crit)
which(dfbetamodel1crit[, 1] > dfbetaCV1crit)
which(dfbetamodel1crit[, 3] > dfbetaCV1crit)

sum(dfbetamodel2[, 1] > dfbetaCV2)
sum(dfbetamodel2[, 2] > dfbetaCV2)
sum(dfbetamodel2[, 3] > dfbetaCV2)
which(dfbetamodel2[, 1] > dfbetaCV2)
which(dfbetamodel2[, 3] > dfbetaCV2)

sum(dfbetamodel3[, 1] > dfbetaCV3)
sum(dfbetamodel3[, 2] > dfbetaCV3)
sum(dfbetamodel3[, 3] > dfbetaCV3)
which(dfbetamodel3[, 1] > dfbetaCV3)
which(dfbetamodel3[, 3] > dfbetaCV3)

# Dado que nuestro datset es pequeño, no tendremso en cuenta este metodo por dar demasiadas observaciones.


# Grafica con las distancias de Cook
influencePlot(model.1)
pos_influyentes_1 <- c(1,12,24)

influencePlot(model.1crit)
pos_influyentes_1crit <- c(1,8,12,23)

influencePlot(model.2)
pos_influyentes_2 <- c(1,6,7,8,15)

influencePlot(model.3)
pos_influyentes_3 <- c(1,7,15,19,24)



# Colinealidad
vif(model.1) # tiene sentido pues en model.1 solo hay una variable predictora
vif(model.1crit)
vif(model.2)
vif(model.3) # como los valores de VIF no son grando, no indican colinealidad grave


# Vamos a tomar como posibles influyentes las que aparecen por varios metodos en cada modelo

# En el 1: 1 12 
pos_1 <- c(TRUE,rep(FALSE,10),TRUE,rep(FALSE,18))
train.1inf <- train.1&!pos_1
model.1inf <- lm(taste ~ Lactic, data = cheddar[train.1inf,])

ncvTest(model.1)
ncvTest(model.1inf) # empeora pero sigue cumpliendo la hipotesis

shapiro.test(resid(model.1))
shapiro.test(resid(model.1inf)) # mejora poco

durbinWatsonTest(model.1)
durbinWatsonTest(model.1inf) # mejora

resettest(model.1, power=2:3, type="regressor", data=cheddar[train.1inf,]) 
resettest(model.1inf, power=2:3, type="regressor", data=cheddar[train.1inf,]) # mejora bastante

# Redefinimos el modelo eliminando las observaciones influyentes de pos_1.
train.1aux<-train.1
train.1 <- train.1inf
model.1 <- model.1inf


# En el 1crit: 1 12  23
pos_1crit <- c(TRUE,rep(FALSE,10),TRUE,rep(FALSE,10),TRUE,rep(FALSE,7))
train.1critinf <- train.1&!pos_1crit
model.1critinf <- lm(taste ~H2S+Lactic , data = cheddar[train.1critinf,])

ncvTest(model.1crit)
ncvTest(model.1critinf) # empeora

shapiro.test(resid(model.1crit))
shapiro.test(resid(model.1critinf)) # mejora

durbinWatsonTest(model.1crit)
durbinWatsonTest(model.1critinf) # mejora bastante

resettest(model.1crit, power=2:3, type="regressor", data=cheddar[train.1,]) 
resettest(model.1critinf, power=2:3, type="regressor", data=cheddar[train.1critinf,]) # empeora dentro de aceptable

# Redefinimos el modelo eliminando las observaciones influyentes de pos_1crit.
train.1crit <- train.1critinf
model.1crit <- model.1critinf


# En el 2: 6 
pos_2 <- c(rep(FALSE,5),TRUE,rep(FALSE,24))
train.2inf <- train.2&!pos_2
model.2inf <- lm(taste ~ H2S+Lactic, data = cheddar[train.2inf,])

ncvTest(model.2)
ncvTest(model.2inf) # mejora poco

shapiro.test(resid(model.2))
shapiro.test(resid(model.2inf)) # aproximadamente lo mismo

durbinWatsonTest(model.2)
durbinWatsonTest(model.2inf) # mejora

resettest(model.2, power=2:3, type="regressor", data=cheddar[train.2inf,]) 
resettest(model.2inf, power=2:3, type="regressor", data=cheddar[train.2inf,]) # aproximadamente lo mismo

# Redefinimos el modelo eliminando las observaciones influyentes de pos_2.
train.2 <- train.2inf
model.2 <- model.2inf


# En el modelo 3 no hacemos comprobaciones nada ya que no se repiten observaciones por distintos metodos.





# 5) Errores de Test. Comparacion de Modelos

# Vamos a considerar otras dos semillas para evaluar los modelos y elegir el mejor.
# Las tomamos de forma que no coincidan en la medida de lo posible los tests de las distintas particiones.
# Dividimos el conjunto total para las dos semillas nuevas.

set.seed((2234))
train.4 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.4 <- (!train.4)
sum(test.4)
sum(test.1==TRUE & test.4==TRUE)
sum(test.2==TRUE & test.4==TRUE)
sum(test.3==TRUE & test.4==TRUE)


set.seed((131))
train.5 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.5 <- (!train.5)
sum(test.5)
sum(test.1==TRUE & test.5==TRUE)
sum(test.2==TRUE & test.5==TRUE)
sum(test.3==TRUE & test.5==TRUE)
sum(test.4==TRUE & test.5==TRUE)

train.1h <- train.1crit
train.2h <- train.2
train.3h <- train.3
train.4h <- train.4
train.5h <- train.5

# Recalculamos train.2 porque fue modificado
set.seed(1100)
train.2 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))




# Veamso si podemos eliminar posibles influyentes en los modelos restantes

mh1 <- lm(taste ~ H2S + Lactic, data = cheddar[train.1h,]) # comprobados supuestos
mh2 <- lm(taste ~ H2S + Lactic, data = cheddar[train.2h,]) # comprobados supuestos
mh3 <- lm(taste ~ H2S + Lactic, data = cheddar[train.3h,]) # comprobados supuestos
mh4 <- lm(taste ~ H2S + Lactic, data = cheddar[train.4h,])
mh5 <- lm(taste ~ H2S + Lactic, data = cheddar[train.5h,])

m1 <- lm(taste ~ Lactic, data = cheddar[train.1,]) # comprobados supuestos
m2 <- lm(taste ~ Lactic, data = cheddar[train.2,])
m3 <- lm(taste ~ Lactic, data = cheddar[train.3,])
m4 <- lm(taste ~ Lactic, data = cheddar[train.4,])
m5 <- lm(taste ~ Lactic, data = cheddar[train.5,])


# Al igual que hicimos en 4 comprobamos si retirando outliers y observaciones influyentes mejoran las hipotesis.
#  Unicamente consideraremos observaciones obtenidas tanto por outlierTest() como por influencePlot().

# mh4
outlierTest(mh4)
influencePlot(mh4) # 6,12,15,24
pos4h <- c(rep(FALSE,5),TRUE,rep(FALSE,5),TRUE,rep(FALSE,2),TRUE,rep(FALSE,8),TRUE,rep(FALSE,6))

train.4inf <- train.4h&!pos4h
mh4inf <- lm(taste ~ H2S+Lactic, data = cheddar[train.4inf,])

ncvTest(mh4)
ncvTest(mh4inf) # mejora MUCHO

shapiro.test(resid(mh4))
shapiro.test(resid(mh4inf)) # mejora

durbinWatsonTest(mh4)
durbinWatsonTest(mh4inf) # mejora

resettest(mh4, power=2:3, type="regressor", data=cheddar[train.4inf,]) 
resettest(mh4inf, power=2:3, type="regressor", data=cheddar[train.4inf,]) # empeora pero es aceptable

# Redefinimos eliminando pos4h por ser tanta la mejora en homocedasticidad que compensa la linealidad
train.4h <- train.4inf
mh4 <- mh4inf


# mh5
outlierTest(mh5)
influencePlot(mh5) # 6,7,8,12,15
pos5h <- c(rep(FALSE,5),TRUE,TRUE,TRUE,rep(FALSE,3),TRUE,rep(FALSE,2),TRUE,rep(FALSE,15))

train.5inf <- train.5h&!pos5h
mh5inf <- lm(taste ~ H2S+Lactic, data = cheddar[train.5inf,])

ncvTest(mh5)
ncvTest(mh5inf) # mejora 

shapiro.test(resid(mh5))
shapiro.test(resid(mh5inf)) # empeora

durbinWatsonTest(mh5)
durbinWatsonTest(mh5inf) # a veces no se cumple la hipotesis nula de no autocorrelacion

resettest(mh5, power=2:3, type="regressor", data=cheddar[train.5inf,]) 
resettest(mh5inf, power=2:3, type="regressor", data=cheddar[train.5inf,]) # empeora

# Decidimos no actualizar el modelo pues no siempre se rechaza la correlacion y empeoran otros p-valores.


# m2
outlierTest(m2)
influencePlot(m2) # 1,12,15,18,24
pos2 <- c(TRUE,rep(FALSE,10),TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,5),TRUE,rep(FALSE,6))

train.2inf <- train.2&!pos2
m2inf <- lm(taste ~ Lactic, data = cheddar[train.2inf,])

ncvTest(m2)
ncvTest(m2inf) # empeora ligeramente

shapiro.test(resid(m2))
shapiro.test(resid(m2inf)) # empeora ligeramente

durbinWatsonTest(m2)
durbinWatsonTest(m2inf) # empeora bastante

resettest(m2, power=2:3, type="regressor", data=cheddar[train.2inf,]) 
resettest(m2inf, power=2:3, type="regressor", data=cheddar[train.2inf,]) # mejora

# Decicimos no redefinirlo, ya que empeora en muchas y mejora bastante en una


# m3
outlierTest(m3)
influencePlot(m3) # 1,15,19,24
pos3 <- c(TRUE,rep(FALSE,13),TRUE,rep(FALSE,3),TRUE,rep(FALSE,4),TRUE,rep(FALSE,6))

train.3inf <- train.3&!pos3
m3inf <- lm(taste ~ Lactic, data = cheddar[train.3inf,])

ncvTest(m3)
ncvTest(m3inf) # mejora poco

shapiro.test(resid(m3))
shapiro.test(resid(m3inf)) # mejora

durbinWatsonTest(m3)
durbinWatsonTest(m3inf) # empeora

resettest(m3, power=2:3, type="regressor", data=cheddar[train.3inf,]) 
resettest(m3inf, power=2:3, type="regressor", data=cheddar[train.3inf,]) # empeora casi lo pierde

# Decidimos no redefinirlo por estar al borde en un criterio y no mejorar mucho en otros


# m4
outlierTest(m4)
influencePlot(m4) # 6,12,15,20,24
pos4 <- c(rep(FALSE,5),TRUE,rep(FALSE,5),TRUE,rep(FALSE,2),TRUE,rep(FALSE,4),TRUE,rep(FALSE,3),TRUE,rep(FALSE,6))

train.4inf <- train.4&!pos4
m4inf <- lm(taste ~ Lactic, data = cheddar[train.4inf,])

ncvTest(m4)
ncvTest(m4inf) # empeora bastante

shapiro.test(resid(m4))
shapiro.test(resid(m4inf)) # empeora

durbinWatsonTest(m4)
durbinWatsonTest(m4inf)# empeora bastante

resettest(m4, power=2:3, type="regressor", data=cheddar[train.4inf,]) 
resettest(m4inf, power=2:3, type="regressor", data=cheddar[train.4inf,]) # mejora algo

# Decidimos no cambiarlo porque empeora en casi todo, y de por si tenia buenos p-values


# m5
outlierTest(m5)
influencePlot(m5) # 1,8,12,15,18,24
pos5 <- c(TRUE,rep(FALSE,6),TRUE,rep(FALSE,3),TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,5),TRUE,rep(FALSE,6))

train.5inf <- train.5&!pos5
m5inf <- lm(taste ~ Lactic, data = cheddar[train.5inf,])

ncvTest(m5)
ncvTest(m5inf) # empeora bastante

shapiro.test(resid(m5))
shapiro.test(resid(m5inf)) # mejora 

durbinWatsonTest(m5)
durbinWatsonTest(m5inf) # no se verifica la hipotesis nula de no autocorrelacion

resettest(m5, power=2:3, type="regressor", data=cheddar[train.5inf,]) 
resettest(m5inf, power=2:3, type="regressor", data=cheddar[train.5inf,]) # empeora

# Decidimos no cambiarlo por perderse la correlación

# Notese que en los originales siempre se cumplian las hipotesis, aunque no era necesario para el proceso




lista_test <- list(test.1,test.2,test.3,test.4,test.5)
lista_trainh <- list(train.1h,train.2h,train.3h,train.4h,train.5h)
lista_train <- list(train.1,train.2,train.3,train.4,train.5)


modelos_hip <- c("S1 taste ~ H2s + Lactic",
                 "S1 taste ~ Lactic",
                 "S2 taste ~ H2s + Lactic",
                 "S2 taste ~ Lactic",
                 "S3 taste ~ H2s + Lactic",
                 "S3 taste ~ Lactic",
                 "S4 taste ~ H2s + Lactic",
                 "S4 taste ~ Lactic",
                 "S5 taste ~ H2s + Lactic",
                 "S5 taste ~ Lactic",
                 "Nivel de significacion")

hip_RL <- c("Distribución_normal",
            "Media_0",
            "Varianza_no_constante",
            "No_Autocorrelación")
placeholder <- vector(mode = "logical",length = 11)
df_hipRL <- data.frame("0" = placeholder,
                       "1" = placeholder,
                       "2" = placeholder,
                       "3" = placeholder,
                       row.names = modelos_hip)

colnames(df_hipRL) <- hip_RL

r <- 1
for (dtrain in lista_trainh){
  model.HL.lm <- lm(taste ~ H2S + Lactic,
                    data = cheddar[dtrain,])
  residuos <- resid(model.HL.lm)
  new_row <- c()
  
  shap <- round(shapiro.test(residuos)$p.value,4)
  t <- t.test(residuos, mu = 0, alternative = "two.sided")$p.value
  v <- round(ncvTest(model.HL.lm)$p,4)
  dw <- round(durbinWatsonTest(model.HL.lm)$p,4)
  
  new_row = c(shap,t,v,dw)
  df_hipRL[r,] <- new_row
  r = r + 2
}


r <- 2
for (dtrain in lista_train){
  model.L.lm <- lm(taste ~Lactic,
                   data = cheddar[dtrain,])
  residuos <- resid(model.L.lm)
  new_row <- c()
  
  shap <- round(shapiro.test(residuos)$p.value,4)
  t <- t.test(residuos, mu = 0, alternative = "two.sided")$p.value
  v <- round(ncvTest(model.L.lm)$p,4)
  dw <- round(durbinWatsonTest(model.L.lm)$p,4)
  
  new_row = c(shap,t,v,dw)
  df_hipRL[r,] <- new_row
  r = r + 2
}
df_hipRL[11,] <- c(rep(0.05,4))
df_hipRL # Todos los modelos cumplen las hipótesis



err1 <- 0
for (i in 5){
  dtrain <- lista_trainh[[i]]
  dtest <- lista_test[[i]]
  dmod <- lm(taste ~ H2S + Lactic, data = cheddar[dtrain,])
  Y <- cheddar[dtest,]$taste
  Yhat <- predict(obj = dmod, newdata = cheddar[dtest,])
  err1 <- err1 + mean((Y - Yhat)^2)
}
err1 <- err1/5
err1 # H2S + LACTIC tiene error medio 11.63


err2<-0
for (i in 5){
  dtrain <- lista_train[[i]]
  dtest <- lista_test[[i]]
  dmod <- lm(taste ~ Lactic, data = cheddar[dtrain,])
  Y <- cheddar[dtest,]$taste
  Yhat <- predict(obj = dmod, newdata = cheddar[dtest,])
  err2 <- err2 + mean((Y - Yhat)^2)
}
err2 <- err2/5
err2 # LACTIC tiene error medio 19.4

err1 < err2 # Concluimos que el modelo taste ~ H2S + Lactic es mejor que taste ~ Lactic.





# 6) Conclusion

model.y <- lm(taste ~ H2S + Lactic,data=cheddar) # tomamos el mejor modelo de los obtenidos en 4
summary(model.y)
plot(model.y)

# Veamos los posibles outliers
outlierTest(model.y)
influencePlot(model.y)


# Estudio de hipótesis supuestas

shapiro.test(resid(model.y)) # normalidad de residuos
t.test(resid(model.y), mu = 0, alternative = "two.sided") # media cero de los residuos
ncvTest(model.y) # varianza constante de los residuos
durbinWatsonTest(model.y) # no autocorrelacion
resettest(model.y ,power=2:3, type="regressor", data=cheddar) # linealidad

# Efectivamente, cumple todas las hipótesis


summary(model.y) # en el summary observamos los valores de betahat, sus p-valores y sigma

coeff <- summary(model.y)$coeff[,1]
coeff

# Calculo de intervalo de confianza de beta1(H2S) y beta2(Lactic)

# Metodo Bonferroni
alpha <- 0.10
summary(model.y)$coef
b <- summary(model.y)$coef[2:3, 1]
s.b <- summary(model.y)$coef[2:3, 2]
g <- 3
n <- nrow(cheddar)
p <- ncol(summary(model.y)$coef)
t_teo <- qt(1 - alpha / (2 * g), n - p)
BomSimCI <- matrix(c(b - t_teo * s.b, b + t_teo * s.b), ncol = 2)
conf <- c("5%", "95%")
bnam <- c("H2S", "Lactic")
dimnames(BomSimCI) <- list(bnam, conf)
BomSimCI

# Intervalo de confianza simultaneo por el metodo de Scheffe
Q <- p - 1
f_teo <- qf(0.9, Q, n - p)#0.9 no seria 0.95?
SchSimCI <- matrix(c(b - sqrt(Q * f_teo) * s.b, b + sqrt(Q * f_teo) * s.b), ncol = 2)
conf <- c("5%", "95%")
bnam <- c("H2S", "Lactic")
dimnames(SchSimCI) <- list(bnam, conf)
SchSimCI


# La ecuación de nuestro modelo final es y = (-27.6) + 3.95*x_H + 19.9*x_L,
#    donde x_H y x_L denotan los valores observados de H2S y Lactic.


rse <- sqrt(deviance(model.y)/df.residual(model.y))
rse  # varianza de los residuos

pval <- summary(model.y)$coeff[,4]
pval  # vector con los p-valores


# Calculo de su R^2 
anova(model.y)
SSE <- anova(model.y)[3,2]
SST <- anova(model.y)[1,2]+anova(model.y)[2,2]+anova(model.y)[3,2]
rsqr <- 1 - SSE/SST
rsqr
summary(model.y)$r.squared# es la misma 

# Calculo de su R^2 ajustada
MSE <- SSE/anova(model.y)[3,1]
MST <- SST/(anova(model.y)[1,1]+anova(model.y)[2,1]+anova(model.y)[3,1])
Radj <-1-MSE/MST
Radj  # se comprueba en la tabla summary que el valor es correcto


# Observamos como se distribuye la variable taste en función de H2S y Lactic
plot_ly(x=H2S, y=Lactic, z=taste, type="scatter3d", mode="marker", color=taste) %>% 
  layout(scene = list(xaxis = list(title = 'H2S (%)'),
                      yaxis = list(title = 'Lactic (%)'),
                      zaxis = list(title = 'Taste (0-100)')))


# Vemos el plano de regresion del modelo propuesto.
# En rojo se marcan las observaciones que peor se ajustan al modelo.
planereg <- scatterplot3d(x=H2S, y=Lactic, z=taste, pch=16, cex.lab=1,
                          highlight.3d=TRUE, type="h", xlab='H2S (%)',
                          ylab='Lactic (%)', zlab='Taste (0-100)')
planereg$plane3d(model.y, lty.box="solid", col='mediumblue')
