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
sapply(cheddar, class)
head(cheddar)

# Comprobamos que no hay entradas vacias
any(is.na(cheddar))



# Histogramas de todas las variables
ids <- names(cheddar)
layout(matrix(1:4, nrow = 1))
y_lab_string <- "Quantity"
for (id in ids) {
  hist(cheddar[, id], xlab = id, ylab = y_lab_string, main = paste("Histogram of ", id))
  y_lab_string <- ""
}


# Graficas de las relaciones entre variables.
plot(cheddar)

# Graficas de dispersion entre la variable respuesta "taste" y las variables predictoras.
layout(matrix(1:3, nrow = 1))

plot(Acetic, taste,
     main = "Relacion entre Taste y Acetic",
     xlab = "Acetic", ylab = "Taste",
     pch = 19, frame = FALSE)


plot(H2S, taste,
     main = "Relacion entre Taste y H2S",
     xlab = "H2S", ylab = "Taste",
     pch = 19, frame = FALSE)


plot(Lactic, taste,
     main = "Relacion entre Taste y Lactic",
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
# Correlaciones y tabla de resultados con el estudio de sus p-valores
cor(cheddar)
ggpairs(cheddar)

mat_cor <- cor(cheddar, method = "pearson")
corrplot(mat_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


#Por ultimo nos conviene eliminar los outliers que dependan de la variable respuesta del 
# modelo,
Model.all<-fortify(model.all)
outlierTest(model.all) # comprobamos si hay outliers (no)
#ninguno

#por bonferroni
alpha <- 0.05
BCV <- qt(1-alpha/(2*30),26) #el valor crítico de Bonferroni t_{1-alpha/2n;n-p-1}, n=30,p=3
BCV
sum(abs(rstudent(model.all))>BCV)
sort(abs(rstandard(model.all)),decreasing = TRUE)[1:3]
#ninguno
influenceIndexPlot(model.all)

model.all_15<- lm(taste ~ ., data = cheddar[-15,])

ncvTest(model.all)
ncvTest(model.all_15)#no tendri sentido, ya que solo lo empeora 

shapiro.test(resid(model.all))
shapiro.test(resid(model.all_15))#mejora

durbinWatsonTest(model.all)
durbinWatsonTest(model.all_15)#mejora

resettest(model.all, power=2:3, type="regressor", data=cheddar) 
resettest(model.all_15, power=2:3, type="regressor", data=cheddar)#empeora
# vamos a ver si lo eliminamos seed a seed

# #NO TOCOAR LOS $ !!!!!!
plot(cheddar$taste~cheddar$Acetic + cheddar$H2S+ cheddar$Lactic,col=ifelse(cheddar$taste %in% c(54.9),"red","black"))
# sacamos en conclusión que el 15 podría ser interpretado como outlier
# encuentran en "extremos" salvo en el Acetic, pero no sería demasiado problema
# ya que esta variable salio no significativa.




# 3) Seleccion del mejor modelo. Metodos por pasos y por criterios


# Separacion del dataset en conjuntos de entrenamiento y test (70-30%)

# Hemos considerado varias semillas para abarcar más modelos y  además hemos intentado
# evitar en la medida de lo poible que se repitan muchos elementos en los test


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

#para evitar repetir este proceso usamos el package mixlm para automatizarlo
model.1 <- mixlm::backward(model.all1, alpha=0.05)
summary(model.1)#LACTIC

model.2 <- mixlm::backward(model.all2, alpha=0.05)
summary(model.2)#H2S LACTIC

model.3 <- mixlm::backward(model.all3, alpha=0.05)
summary(model.3)#H2S LACTIC


# ii) FORWARD (alpha=0.05)

SCOPE <- (~ . + Acetic + H2S + Lactic)
model.inicial <- lm(taste ~ 1, data = cheddar[train.1,]) # solo el termino independiente

add1(model.inicial, scope = SCOPE, test = "F")
# Añadimos Lactic por ser la variable predictora con menor p-valor
model.updateF1 <- update(model.inicial, . ~ . + Lactic)
# no añadimos ninguna variable pues sus p-valores superan la barrera de alpha

model.1a <- lm(taste ~ Lactic, data = cheddar[train.1,])
summary(model.1a)

#Igual que en backward usamos el paquete mixlm para automatizar
model.1a <- mixlm::forward(model.all1, alpha=0.05)
summary(model.1a)#LACTIC

model.2a <- mixlm::forward(model.all2, alpha=0.05)
summary(model.2a)#H2S LACTIC

model.3a <- mixlm::forward(model.all3, alpha=0.05)
summary(model.3a)#H2S LACTIC

# Nótese que los modelos obtenidos por metodos de pasos coinciden


# iii) CRITERIOS

# R2 ajustado
models1 <- regsubsets(taste ~ ., data = cheddar[train.1,])
models2 <- regsubsets(taste ~ ., data = cheddar[train.2,])
models3 <- regsubsets(taste ~ ., data = cheddar[train.3,])

summary(models1)# este  tiene a Lactic como 1
summary(models2)
summary(models3)#2 y 3 son iguales
MR2adj1 <- summary(models1)$adjr2
MR2adj2 <- summary(models2)$adjr2
MR2adj3 <- summary(models3)$adjr2

summary(models1)$which[which.max(MR2adj1), ]#nuevo
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

# Notese que en los train 2 y 3 solo sale el modelo H2S+Lactic 
# Pero en train1 sale ese y tambien el Lactic 
model.1crit <- lm(taste ~ H2S + Lactic, data=cheddar[train.1,])
model.1
model.1crit
model.2
model.3
#Esto nos puede llevar a pensar que el modelo optimo vaya a ser H2S y Lactic


anova(model.1,model.all1)
anova(model.1crit,model.all1)
anova(model.2,model.all2)
anova(model.3,model.all3)

#esto nos indica que los modelos creados son mejores que los completos
#más adelante profundizaremos en las relaciones entre ellos



# 4) Diagnostico. Comprobaciones de hipotesis, outliers y observaciones influyentes
attach(cheddar)
#Vamos a realizar el diagnostico de la seed 1 en profundidad,
# y el del resto se realizará antes del calculo de errores
# Además de porque en la seed1 salen los dos modelos a comparar

# Comprobación de hipótesis de los  modelos 

# Linealidad
resettest(model.1, power=2:3, type="regressor", data=cheddar[train.1,]) # p-valor > 0.05 luego aceptamos hipótesis de linealidad
resettest(model.1crit, power=2:3, type="regressor", data=cheddar[train.1,]) # p-valor > 0.05 luego aceptamos hipótesis de linealidad
resettest(model.2, power=2:3, type="regressor", data=cheddar[train.2,]) # p-valor > 0.05 luego aceptamos hipótesis de linealidad
resettest(model.3, power=2:3, type="regressor", data=cheddar[train.3,]) # p-valor > 0.05 luego aceptamos hipótesis de linealidad

 
plot(cheddar$H2S, cheddar$taste,
     main = "Relacion entre Taste y H2S",
     xlab = "H2S", ylab = "Taste",
     pch = 19, frame = FALSE) 
plot(cheddar$Lactic, cheddar$taste,
     main = "Relacion entre Taste y Lactic",
     xlab = "Lactic", ylab = "Taste",
     pch = 19, frame = FALSE) 

#corroboramos que la linealidad esta presente


# Normalidad y Autocorrelacion

shapiro.test(resid(model.1)) # como el p-valor > 0.05 no se rechaza la hipótesis nula, i.e. la normalidad
qqnorm(resid(model.1)) # o la funciÃ³n 2 del plot anterior
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



# observamos que las colas no siguen el mismo patrón que el resto de datos, lo que podría indicar que 
#  el modelo no sigue una distribución normal. Sin embargo, al no afectar a un gran porcentaje de los datos y
#  teniendo en cuenta el resultado previo obtenido por el Test de Shapiro-Wilk no rechazamos la hipótesis de normalidad.


#media de errores nula

t.test(resid(model.1), mu = 0, alternative = "two.sided")
t.test(resid(model.2), mu = 0, alternative = "two.sided")
t.test(resid(model.3), mu = 0, alternative = "two.sided")
t.test(resid(model.3crit), mu = 0, alternative = "two.sided")
#lo es todo

#Autocorrelacion

durbinWatsonTest(model.1) #bien

durbinWatsonTest(model.1crit) #bien

durbinWatsonTest(model.2) #bien

durbinWatsonTest(model.3) #bien




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




ncvTest(model.1) # como el p-valor > 0.05 no hay evidencias para rechazar que la varianza sea constante
ncvTest(model.1crit)
ncvTest(model.2)
ncvTest(model.3)

# Los residuos se distribuyen de forma homogénea a lo largo de una banda horizontal, luego se verifica la hipótesis

# Outliers
alpha <- 0.05
n1<-nrow(cheddar[train.1,])
p1 <-nrow(summary(model.1)$coef)


n1crit<-nrow(cheddar[train.1,])
p1crit <-nrow(summary(model.1crit)$coef)

n2<-nrow(cheddar[train.2,])
p2 <-nrow(summary(model.2)$coef)

n3<-nrow(cheddar[train.3,])
p3 <-nrow(summary(model.3)$coef)


# el valor critico de Bonferroni t_{1-alpha/2n;n-p-1}
BCV1 <- qt(1 - alpha / (2 * n1), n1-p1-1) 
BCV1crit <- qt(1 - alpha / (2 * n1crit), n1crit-p1crit-1) 
BCV2 <- qt(1 - alpha / (2 * n2), n2-p2-1) 
BCV3 <- qt(1 - alpha / (2 * n3), n3-p3-1) 

sum(abs(rstudent(model.1)) > BCV1)
sum(abs(rstudent(model.1crit)) > BCV1crit)
sum(abs(rstudent(model.2)) > BCV2)
sum(abs(rstudent(model.3)) > BCV3)


outlierTest(model.1)
outlierTest(model.1crit)#NA?
outlierTest(model.2) # no hay en ninguno de los dos modelos planteados
outlierTest(model.3)




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
which(hii1>hCV1)#1 5 16 23 24 26

sum(hii1crit > hCV1crit)
which(hii1crit>hCV1crit)#23

sum(hii2 > hCV2)
which(hii2>hCV2)#6 

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
which(dfbetamodel1[, 2] > dfbetaCV1)#c(1,3,5,8,9,12,13,14,16,19,23,24,27,28,30)

sum(dfbetamodel1crit[, 1] > dfbetaCV1crit)
sum(dfbetamodel1crit[, 2] > dfbetaCV1crit)
sum(dfbetamodel1crit[, 3] > dfbetaCV1crit)
which(dfbetamodel1crit[, 1] > dfbetaCV1crit)
which(dfbetamodel1crit[, 3] > dfbetaCV1crit)#c(1,3,8,9,11,12,13,14,19,23,24,26,27,28,30)

sum(dfbetamodel2[, 1] > dfbetaCV2)
sum(dfbetamodel2[, 2] > dfbetaCV2)
sum(dfbetamodel2[, 3] > dfbetaCV2)
which(dfbetamodel2[, 1] > dfbetaCV2)
which(dfbetamodel2[, 3] > dfbetaCV2)#c(1,3,4,7,8,9,11,12,16,17,20,29,30)

sum(dfbetamodel3[, 1] > dfbetaCV3)
sum(dfbetamodel3[, 2] > dfbetaCV3)
sum(dfbetamodel3[, 3] > dfbetaCV3)
which(dfbetamodel3[, 1] > dfbetaCV3)
which(dfbetamodel3[, 3] > dfbetaCV3)#c(1,4,5,7,12,14,16,17,19,23,27,29)

#nuestro dataset es demasiado pequeño como para tener en cuenta todas esas



# Grafica con la distancia de Cook
influencePlot(model.1)
pos_influyentes_1 <- c(1,12,24)

influencePlot(model.1crit)
pos_influyentes_1crit <- c(1,8,12,23)

influencePlot(model.2)
pos_influyentes_2 <- c(1,6,7,8,15)


influencePlot(model.3)
pos_influyentes_3 <- c(1,7,15,19,24)




# Colinealidad. Unicamente la estudiamos en el modelo por CRITERIOS pues en STEP solo interviene Lactic
vif(model.1)#dijimos que el de lactic no porque sucede esto no hay colin por solo 1 var
vif(model.1crit)
vif(model.2)
vif(model.3) # los valores de VIF no indican colinealidad grave


#sobre as influyentes vamos a tomar las que se repitan en algun test
#en el 1: 1 12 
pos_1<-c(TRUE,rep(FALSE,10),TRUE,rep(FALSE,18))
# length(pos_1crit)
train.1inf<-train.1&!pos_1
model.1inf<- lm(taste ~ Lactic, data = cheddar[train.1inf,])

ncvTest(model.1)
ncvTest(model.1inf)#empeora sigue dentro de aceptable

shapiro.test(resid(model.1))
shapiro.test(resid(model.1inf))#mejora

durbinWatsonTest(model.1)
durbinWatsonTest(model.1inf)#mejora

resettest(model.1, power=2:3, type="regressor", data=cheddar[train.1inf,]) 
resettest(model.1inf, power=2:3, type="regressor", data=cheddar[train.1inf,]) #mejora

#redefino eliminando pos influy
train.1<-train.1inf
model.1<-model.1inf

# #NO TOCOAR LOS $ !!!!!! falla 
#plot(cheddar[train$taste~cheddar$Acetic + cheddar$H2S+ cheddar$Lactic,col=ifelse(cheddar$taste %in% c(1,12,24),"red","black"))

#en el 1crit: 1 12  23
pos_1crit<-c(TRUE,rep(FALSE,10),TRUE,rep(FALSE,10),TRUE,rep(FALSE,7))

train.1critinf<-train.1&!pos_1crit
model.1critinf<- lm(taste ~H2S+Lactic , data = cheddar[train.1critinf,])

ncvTest(model.1crit)
ncvTest(model.1critinf)#mejora

shapiro.test(resid(model.1crit))
shapiro.test(resid(model.1critinf))#empeora,sigue dentro de aceptable

durbinWatsonTest(model.1crit)
durbinWatsonTest(model.1critinf)#mejora

resettest(model.1crit, power=2:3, type="regressor", data=cheddar[train.1critinf,]) 
resettest(model.1critinf, power=2:3, type="regressor", data=cheddar[train.1icritnf,]) #empeora dentro de aceptable

#redefinimos 
train.1crit<-train.1critinf
model.1crit<- model.1critinf

#en el 2: 6 
pos_2<-c(rep(FALSE,5),TRUE,rep(FALSE,24))

train.2inf<-train.2&!pos_2
model.2inf<- lm(taste ~ H2S+Lactic, data = cheddar[train.2inf,])

ncvTest(model.2)
ncvTest(model.2inf)#mejora

shapiro.test(resid(model.2))
shapiro.test(resid(model.2inf))#mejora

durbinWatsonTest(model.2)
durbinWatsonTest(model.2inf)#mejora

resettest(model.2, power=2:3, type="regressor", data=cheddar[train.2inf,]) 
resettest(model.2inf, power=2:3, type="regressor", data=cheddar[train.2inf,]) #mejora

#redefino eliminando pos influy
train.2<-train.2inf
model.2<-model.2inf


# sobre el 3 no se realiza nada ya que no se repiten en ninguno


#el 15 del principio no salia repetido en ninguno, puede tener sentido continuar con él


# 5) Errores de Test. Comparacion de Modelos
# para comparar los modelos vamos a usar 5 seeds, 3 de llas las ya usadas, y 
# 


# Comenzamos eliminando las observaciones influyentes de ambos modelos ya que no hay outliers

# cheese1 <- cheddar[-pos_influyentes_1, ]
# cheese2 <- cheddar[-pos_influyentes_2, ]
# cheese3 <- cheddar[-pos_influyentes_3, ]
# cheese3crit <- cheddar[-pos_influyentes_3crit, ]
# train.1a<-train.1&!pos_1
# train.2a<-train.2&!pos_2
# train.3a<-train.3&!pos_3
# train.3ca<-train.3&!pos_3crit




model.exh1 <- regsubsets(taste ~ ., data = cheddar[train.1a, ], method = "exhaustive")
model.exh2 <- regsubsets(taste ~ ., data = cheddar[train.2a, ], method = "exhaustive")
model.exh3 <- regsubsets(taste ~ ., data = cheddar[train.3a, ], method = "exhaustive")
model.exh3crit <- regsubsets(taste ~ ., data = cheddar[train.3ca, ], method = "exhaustive")


predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvar <- names(coefi)
  mat[, xvar] %*% coefi
}


Y1 <- cheddar[test.1, ]$taste
Y2 <- cheddar[test.2, ]$taste
Y3 <- cheddar[test.3, ]$taste
Y3crit <- cheddar[test.3, ]$taste

model1h<- lm(taste~H2S + Lactic,data=cheddar[train.1a,])
model2h<- lm(taste~H2S + Lactic,data=cheddar[train.2a,])
model3<- lm(taste~Lactic,data=cheddar[train.3a,])
model3h<- lm(taste~H2S + Lactic,data=cheddar[train.3ca,])


Yhat1 <-predict(obj=model1e,newdata=cheddar[test.1,])
Yhat2 <-predict(obj=model2e,newdata=cheddar[test.2,])
Yhat3 <-predict(obj=model3e,newdata=cheddar[test.3,])
Yhat3crit <-predict(obj=model3ce,newdata=cheddar[test.3,])

val.errors1 <- mean((Y1 - Yhat1)^2)
val.errors2 <- mean((Y2 - Yhat2)^2)
val.errors3 <- mean((Y3 - Yhat3)^2)
val.errors3crit <- mean((Y3crit - Yhat3crit)^2)

val.errors1
val.errors2
val.errors3
val.errors3crit

val.errors_1 <-rep(NA,3)
for (i in 1:3){
  Yhat1 <-predict.regsubsets(model.exh1, newdata=cheddar[test.1,],id=i)
  val.errors_1[i]<- mean((Y1-Yhat1)^2)
}
val.errors_1
coef(model.exh1, which.min(val.errors_1))

val.errors_2 <-rep(NA,3)
for (i in 1:3){
  Yhat2 <-predict.regsubsets(model.exh2, newdata=cheddar[test.2,],id=i)
  val.errors_2[i]<- mean((Y2-Yhat2)^2)
}
val.errors_2#muy cerca el 2 del 3 y el completo no suele ser
coef(model.exh2, which.min(val.errors_2))

val.errors_3 <-rep(NA,3)
for (i in 1:3){
  Yhat3 <-predict.regsubsets(model.exh3, newdata=cheddar[test.3,],id=i)
  val.errors_[i]<- mean((Y3-Yhat3)^2)
}
val.errors_3
coef(model.exh3, which.min(val.errors_))

regfit.best3 <- regsubsets(taste ~ ., cheese3)
coef(regfit.best, which.min(val.errors_))
regfit.best3crit <- regsubsets(taste ~ ., cheese3crit)
coef(regfit.best3crit, which.min(val.errors_))

#AHORA CALCULO DE ERRORES

# Me centro en mis dos candidatos
m1<-lm(taste~H2S+Lactic,data=cheddar)
m2<-lm(taste~Lactic,data=cheddar)
# En realidad podria haber descartado el 2 usando que para el train de donde salio
# ya era peor que la otra opción, pero  continuaremos con el para tener con quien comparar
outlierTest(m1)
outlierTest(m2)
influencePlot(m1)
influencePlot(m2)
cheddar1<-cheddar#[-c(6,7,8,12,15),]
cheddar2<-cheddar#[-c(1,12,15,18,24),]

#elimino sus posiciones influyentes para poder sacar mas informacion de los train/test 


vector_semillas <- c(5,1100,1,999,77)#seeds aleatorias

lista_train1 <- list()
lista_train2 <- list()
lista_test1 <- list()
lista_test2 <- list()
#calculo los train seed
i <- 1
for (semilla in vector_semillas){
  
  set.seed((semilla))
  train1 <- sample(c(TRUE, FALSE),
                  size = nrow(cheddar1),
                  replace = TRUE,
                  prob = c(0.7, 0.3))
  test1 <- (!train1)
  train2 <- sample(c(TRUE, FALSE),
                   size = nrow(cheddar2),
                   replace = TRUE,
                   prob = c(0.7, 0.3))
  test2 <- (!train2)
  
  lista_train1[[i]] <- c(train1)
  lista_test1[[i]] <- c(test1)
  lista_train2[[i]] <- c(train2)
  lista_test2[[i]] <- c(test2)
  i <- i + 1
}


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
for (dtrain in lista_train1){
  model.HL.lm <- lm(taste ~ H2S + Lactic,
                    data = cheddar1[dtrain,])
  residuos <- resid(model.HL.lm)
  new_row <- c()
  
  shap <- round(shapiro.test(residuos)$p.value,3)
  t <- t.test(residuos, mu = 0, alternative = "two.sided")$p.value
  v<- round(ncvTest(model.HL.lm)$p,3)
  # aovL <- round(summary(res.aov)[[1]][["Pr(>F)"]][1],3)
  # aovH <-  round(summary(res.aov)[[1]][["Pr(>F)"]][2],3)
  dw <- round(durbinWatsonTest(model.HL.lm)$p,3)#CORREGIR PROBLEMILLA; NO SON ESTOS LOS P creo que ya
  
  new_row = c(shap,t,v,dw)
  df_hipRL[r,] <- new_row
  r = r + 2
}


r <- 2
for (dtrain in lista_train2){
  model.L.lm <- lm(taste ~Lactic,
                   data = cheddar2[dtrain,])
  residuos <- resid(model.L.lm)
  # res.aov <- aov(model.L.lm)
  new_row <- c()
  
  shap <- round(shapiro.test(residuos)$p.value,3)
  t <- t.test(residuos, mu = 0, alternative = "two.sided")$p.value
  v<- round(ncvTest(model.L.lm)$p,3)
  # aovL <- round(summary(res.aov)[[1]][["Pr(>F)"]][1],3)
  dw <- round(durbinWatsonTest(model.L.lm)$p,3)#CORREGIR PROBLEMILLA; NO SON ESTOS LOS P
  
  new_row = c(shap,t,v,dw)
  df_hipRL[r,] <- new_row
  r = r + 2
}
df_hipRL[11,] <- c(rep(0.05,4))
#Los datos del dataframe están redondeados al tercer decimal por claridad.
df_hipRL#falla una semilla s4en H2S Lactic
#Todas cumplen las hip


#ESCRIBIR COMENTARIOS SOBRE LOS DATOS DEL ANOVA H2S 
err1<-0

for (i in 5){
  dtest<-lista_test1[[i]]
  dtrain<-(!dtest)
  dmod<-lm(taste~H2S+Lactic,data=cheddar1[dtrain,])
  Y<- cheddar1[dtest,]$taste
  Yhat<-predict(obj=dmod,newdata=cheddar1[dtest,])
  err1<-err1 +mean((Y-Yhat)^2)
}
err1<-err1/5
err1# el de H2S LACTIC 27.86954

err2<-0
for (i in 5){
  dtest<-lista_test2[[i]]
  dtrain<-(!dtest)
  dmod<-lm(taste~Lactic,data=cheddar2[dtrain,])
  Y<- cheddar2[dtest,]$taste
  Yhat<-predict(obj=dmod,newdata=cheddar2[dtest,])
  err2<-err2 +mean((Y-Yhat)^2)
}
err2<-err2/5
err2#64.25982 el de LACTIC 17.09473
Ya <- cheddar2[test.a, ]$taste

Yb <- cheddar2[test.b, ]$taste
Yc <- cheddar2[test.c, ]$taste
Yd <- cheddar2[test.d, ]$taste
Ye <- cheddar2[test.e, ]$taste


Yhata1 <-predict(obj=model.candidato1,newdata=cheddar1[test.a,])
Yhata1<-Yhata1[is.na(Yhata1),]
Yhatb1 <-predict(obj=model.candidato1,newdata=cheddar1[test.b,])
Yhatc1 <-predict(obj=model.candidato1,newdata=cheddar1[test.c,])
Yhatd1 <-predict(obj=model.candidato1,newdata=cheddar1[test.d,])
Yhate1 <-predict(obj=model.candidato1,newdata=cheddar1[test.e,])


Yhata2 <-predict(obj=model.candidato2,newdata=cheddar[test.a,])
Yhatb2 <-predict(obj=model.candidato2,newdata=cheddar[test.b,])
Yhatc2 <-predict(obj=model.candidato2,newdata=cheddar[test.c,])
Yhatd2 <-predict(obj=model.candidato2,newdata=cheddar[test.d,])
Yhate2 <-predict(obj=model.candidato2,newdata=cheddar[test.e,])

err1<-(mean((Ya-Yhata1)^2)+mean((Yb-Yhatb1)^2)+mean((Yc-Yhatc1)^2)+
         mean((Yd-Yhatd1)^2)+mean((Ye-Yhate1)^2))/5
err2<-(mean((Ya-Yhata2)^2)+mean((Yb-Yhatb2)^2)+mean((Yc-Yhatc2)^2)+
         mean((Yd-Yhatd2)^2)+mean((Ye-Yhate1)^2))/5

err1#68.04766
err2#106.8122
err1<err2


# # Validacion cruzada de 1
# 
# n <- nrow(cheese)
# k <- n # numero de grupos, como es elemento a elemento hay n
# 
# folds <- sample(x = 1:k, size = nrow(cheese), replace = FALSE)
# cv.errors <- matrix(NA, k, 3, dimnames = list(NULL, paste(1:3)))
# for (j in 1:k) {
#   best.fit <- regsubsets(taste ~ ., data = cheese[folds != j, ]) # cogemos datos del train
#   for (i in 1:3) {
#     pred <- predict.regsubsets(best.fit, newdata = cheese[folds == j, ], id = i) # datos test
#     cv.errors[j, i] <- mean((cheese$taste[folds == j] - pred)^2)
#   }
# }
# 
# mean.cv.errors <- apply(cv.errors, 2, mean)
# mean.cv.errors
# coef(best.fit, which.min(mean.cv.errors))
# 
# 
# 
# # Validacion en 4 grupos, cambiar la linea de k para otro numero
# 
# n <- nrow(cheese)
# k <- 4 # numero de grupos
# 
# folds <- sample(x = 1:k, size = nrow(cheese), replace = TRUE)
# cv.errors <- matrix(NA, k, 3, dimnames = list(NULL, paste(1:3)))
# for (j in 1:k) {
#   best.fit <- regsubsets(taste ~ ., data = cheese[folds != j, ]) # cogemos datos del train
#   for (i in 1:3) {
#     pred <- predict.regsubsets(best.fit, newdata = cheese[folds == j, ], id = i) # datos test
#     cv.errors[j, i] <- mean((cheese$taste[folds == j] - pred)^2)
#   }
# }
# mean.cv.errors <- apply(cv.errors, 2, mean)
# mean.cv.errors
# coef(best.fit, which.min(mean.cv.errors))

# Comprobacion
model.cv <- lm(taste ~ H2S + Lactic, data = cheese)
summary(model.cv)
plot(lm(taste ~ H2S + Lactic, data = cheddar), which = 1)
plot(lm(taste ~ H2S + Lactic, data = cheddar), which = 2)
residualPlot(model.cv)
influenceIndexPlot(model.cv)


# Suponiendo que los errores se distribuyen con media 0 y varianza v^2

# Calculo de intervalo de confianza de beta1(H2S) y beta2(Lactic)

# Metodo Bonferroni
model.y <- lm(taste ~ H2S + Lactic, data = cheddar)
alpha <- 0.10
summary(model.y)$coef
b <- summary(model.y)$coef[2:3, 1]
s.b <- summary(model.y)$coef[2:3, 2]
g <- 2
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

# Ahora la cosa esa de la elipse  para beta1 y 2


confidenceEllipse(model.y,
                  level = 0.90, which.coef = c(2, 3),
                  Scheffe = FALSE, main = ""
)
title(main = "Elipsoide de confianza Bonferroni")
abline(v = BomSimCI[1, ])
abline(h = BomSimCI[2, ])

confidenceEllipse(model.y,
                  level = 0.90, which.coef = c(2, 3),
                  Scheffe = TRUE, main = ""
)
title(main = "Elipsoide de confianza ScheffÃ©")
abline(v = SchSimCI[1, ])
abline(h = SchSimCI[2, ])



# Estudio de hipótesis supuestas:

residuos <- resid(model.y)

# Los errores tienen distribución normal y media cero
shapiro.test(residuos)

t.test(residuos, mu = 0, alternative = "two.sided")
t.test(resid(modelf2.lm), mu = 0, alternative = "two.sided")
# ambos tienen p-valor 1

# Los errores tienen varianza constante
res.aov <- aov(model.y, data = cheddar)
summary(res.aov)

res.bcaov <- aov(modelf2.lm, data = cheddar)
summary(res.aov)
# con alpha = 0.05 se garantizan las dos.


# Los errores no estan correlacionados
acf(residuos)
# Tiene que quedar 0 en 1 y el resto por debajo de nivel de signficacion, ocurre
durbinWatsonTest(model.y)
durbinWatsonTest(model.final1)
# Comprobado


#### BoxCox: Estudio de hipótesis supuestas:

# Los errores tienen distribución normal

# Tiene media 0

# Varianza constante

# Los errores no está correlaciones

# 
# ######### cosas haciendo boxcox y despues de hacerlo ...######
# 
# # training con boxcox y despues lo comparo
# bc <- boxCox(model.final1, lambda = seq(-2, 2, 1 / 10), plotit = TRUE)
# lambda <- bc$x[which.max(bc$y)]
# Y_bc <- (cheddar$taste^lambda - 1) / lambda
# 
# modelf2.lm <- lm(Y_bc ~ H2S + Lactic, data = cheddar)
# 
# influencePlot(modelf2.lm)
# influencePlot(model.final1)
# 
# pos_influyentes <- c(1, 6, 7, 15, 28)
# 
# cheddar2 <- cheddar
# cheddar2$taste <- (cheddar$taste^lambda - 1) / lambda
# cheddar
# cheddar2
# 
# obs.out <- c(1, 6, 7, 15, 28)
# cheese2 <- cheddar2[-obs.out, ]
# 
# train <- sample(c(TRUE, FALSE), size = nrow(cheese2), replace = TRUE, prob = c(0.70, 0.30))
# # conjunto de entrenamiento
# test <- (!train)
# test
# model.exh2 <- regsubsets(taste ~ ., data = cheddar2[train, 1:4], method = "exhaustive")
# summary(model.exh)
# 
# # la funcion esa que ella siempre copia y pega
# predict.regsubsets <- function(object, newdata, id, ...) {
#   form <- as.formula(object$call[[2]])
#   mat <- model.matrix(form, newdata)
#   coefi <- coef(object, id = id)
#   xvar <- names(coefi)
#   mat[, xvar] %*% coefi
# }
# 
# val.errors2 <- rep(NA, 3)
# Y <- cheddar2[test, ]$taste
# for (i in 1:3) {
#   Yhat <- predict.regsubsets(model.exh2, newdata = cheddar2[test, ], id = i)
#   val.errors2[i] <- mean((Y - Yhat)^2)
# }
# 
# val.errors2
# coef(model.exh2, which.min(val.errors2))
# 
# regfit.best <- regsubsets(taste ~ ., cheddar2[-obs.out, 1:4])
# coef(regfit.best, which.min(val.errors2))
# 
# # esto era antes de boxcox
# val.errors
# coef(model.exh, which.min(val.errors))
# 
# regfit.best <- regsubsets(taste ~ ., cheddar[-obs.out, 1:4])
# coef(regfit.best, which.min(val.errors))
# 
# # para verlo mÃ¡s visual
# 
# val.errors
# val.errors2
# 
# coef(model.exh2, which.min(val.errors2))
# coef(model.exh, which.min(val.errors))
# 
# coef(regfit.best, which.min(val.errors2))
# coef(regfit.best, which.min(val.errors))


# 6) Conclusion

model.final <- model.1crit ############ tomar el mejor modelo de 4
summary(model.final)
plot(model.final)
# En el summary podemos observar tanto los valores de betahat, sus p-valores y sigma

coeff <- summary(model.final)$coeff[,1]
# La ecuación de nuestro modelo final es y = beta0 + beta1*x_H + beta2*x_L,
#    donde x_H y x_L denotan los valores observados de H2S y Lactic.

rse <- sqrt(deviance(model.final)/df.residual(model.final))
pval <- summary(model.final)$coeff[,4]


#calculamos su R^2 
anova(model.final)
SSE<-anova(model.final)[3,2]
SST<- anova(model.final)[1,2]+anova(model.final)[2,2]+anova(model.final)[3,2]
rsqr<-1-SSE/SST
rsqr
Rsqr<-summary(model.final)$r.squared
#calculamos su R^2 ajustada
MSE<- SSE/anova(model.final)[3,1]
MST<- SST/(anova(model.final)[1,1]+anova(model.final)[2,1]+anova(model.final)[3,1])
Radj<-1-MSE/MST
Radj#se comprueba en la tabla summary es cierta


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
planereg$plane3d(model.final, lty.box="solid", col='mediumblue')












