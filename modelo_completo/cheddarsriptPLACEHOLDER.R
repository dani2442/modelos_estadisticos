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
     main = "RelaciÃ³n entre Taste y Acetic",
     xlab = "Acetic", ylab = "Taste",
     pch = 19, frame = FALSE)


plot(H2S, taste,
     main = "RelaciÃ³n entre Taste y H2S",
     xlab = "H2S", ylab = "Taste",
     pch = 19, frame = FALSE)


plot(Lactic, taste,
     main = "RelaciÃ³n entre Taste y Lactic",
     xlab = "Lactic", ylab = "Taste",
     pch = 19, frame = FALSE)


layout(matrix(1:1, nrow = 1))



# 2) Estudio y evaluacion del modelo completo


x <- model.matrix( ~ Acetic + H2S + Lactic, data = cheddar)
betahat <- solve(crossprod(x, x), crossprod(x, taste))
betahat <- c(betahat)
betahat

# Comprobamos el resultado con funciones ya implementadas
model.all <- lm(taste ~ ., data = cheddar)
summary(model.all)
model.all$coefficients

# Correlaciones y tabla de resultados con el estudio de sus p-valores
cor(cheddar)
ggpairs(cheddar)

mat_cor <- cor(cheddar, method = "pearson")
corrplot(mat_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

anova(model.all)

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

# qqPlot(Model.all$.stdresid,dist="t", df=26)#n-p-1 30-3-1
# # el 15 y el 8, pero vemos que segun Bonferroni distan mucho de ser outliers
# #veamoslo
# layout(matrix(c(1, 2,3, 4), byrow=TRUE, nrow=2))
# plot(model.all)
# #observamos que el 15 (y el 8 en menor medida) es el que peor podria afectar a 
# #la homocedasticidad  al tener valores residuales grandes
# ncvTest(model.all)
# model.all_8<- lm(taste ~ ., data = cheddar[-8,])
# model.all_15<- lm(taste ~ ., data = cheddar[-15,])
# model.all_<- lm(taste ~ ., data = cheddar[-c(8,15),])
# 
# ncvTest(model.all)
# ncvTest(model.all_8)
# ncvTest(model.all_15)#no tendri sentido, ya que solo lo empeora
# ncvTest(model.all_)
# 
# shapiro.test(resid(model.all))
# shapiro.test(resid(model.all_8))#no tendri sentido, ya que solo lo empeora
# shapiro.test(resid(model.all_15))
# shapiro.test(resid(model.all_))
# # por lo que tendría snetido eliminar ambos
# layout(matrix(c(1), byrow=TRUE, nrow=1))
# cheddar$taste[8]
# cheddar$taste[15]
# sum(taste==taste[15])
# sum(taste==taste[8])
# 
# #NO TOCOAR LOS $ !!!!!!
# plot(cheddar$taste~cheddar$Acetic + cheddar$H2S+ cheddar$Lactic,col=ifelse(cheddar$taste %in% c(21.9,54.9),"red","black"))

# sacamos en conclusión que no es muy descabellado quitar el 15 y 8 ya que se 
# encuentran en "extremos" salvo en el Acetic, pero no sería demasiado problema
# ya que esta variable salio no significativa.




# 3) Seleccion del mejor modelo. Metodos por pasos y por criterios
# attach(cheddar[train,])


# Separacion del dataset en conjuntos de entrenamiento y test (70-30%)
set.seed(5) 
train.1 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.1 <- (!train.1)
sum(test.1)
model.all1 <- lm(taste ~ ., data = cheddar[train.1,])

set.seed(838) 
train.2 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.2 <- (!train.2)
sum(test.2)
sum(test.1==TRUE & test.2==TRUE)
model.all2 <- lm(taste ~ ., data = cheddar[train.2,])

set.seed(1100) 
train.3 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.3 <- (!train.3)
sum(test.3)
sum(test.1==TRUE & test.3==TRUE)
sum(test.2==TRUE & test.3==TRUE)
model.all3 <- lm(taste ~ ., data = cheddar[train.3,])

set.seed(1) 
train.4 <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
test.4 <- (!train.4)
sum(test.4)
sum(test.1==TRUE & test.4==TRUE)
sum(test.2==TRUE & test.4==TRUE)
sum(test.3==TRUE & test.4==TRUE)
model.all4 <- lm(taste ~ ., data = cheddar[train.4,])
test.4




# i) BACKWARD (alpha=0.05)

drop1(model.all1, test = "F")
# quitamos Acetic del modelo por ser la de mayor p-valor

model.updateB1 <- update(model.all1, . ~ . - Acetic)
drop1(model.updateB1, test = "F")
# # quitamos H2S del modelo por ser la unica variable con p-valor > 0.05
# 
# model.updateB2 <- update(model.updateB1, . ~ . - H2S)
# drop1(model.updateB2, test = "F")
# # termina el proceso pues el p-valor de Lactic supera a alpha

model.1 <- lm(taste ~ H2S+Lactic, data = cheddar[train,])
summary(model.1)
model.1 <- mixlm::backward(model.all1, alpha=0.05)
summary(model.1)#H2S LACTIC

model.2 <- mixlm::backward(model.all2, alpha=0.05)
summary(model.2)#H2S LACTIC

model.3 <- mixlm::backward(model.all3, alpha=0.05)
summary(model.3)#H2S LACTIC

model.4 <- mixlm::backward(model.all4, alpha=0.05)
summary(model.4)# LACTIC

# ii) FORWARD (alpha=0.05)

SCOPE <- (~ . + Acetic + H2S + Lactic)
model.inicial <- lm(taste ~ 1, data = cheddar[train,]) # solo el termino independiente

add1(model.inicial, scope = SCOPE, test = "F")
# Añadimos Lactic por ser la variable predictora con menor p-valor
model.updateF1 <- update(model.inicial, . ~ . + Lactic)

add1(model.updateF1, scope = SCOPE, test = "F")
# no añadimos ninguna variable pues sus p-valores superan la barrera de alpha

model.1a <- lm(taste ~ Lactic, data = cheddar[train,])
summary(model.1a)
# Nótese que los modelos obtenidos por metodos de pasos coinciden

model.1a <- mixlm::forward(model.all1, alpha=0.05)

model.2a <- mixlm::forward(model.all2, alpha=0.05)
summary(model.2a)#H2S LACTIC
model.3a <- mixlm::forward(model.all3, alpha=0.05)
summary(model.3a)#H2S LACTIC
model.4a <- mixlm::forward(model.all4, alpha=0.05)
summary(model.4a)# LACTIC

# iii) CRITERIOS

# R2 ajustado
models1 <- regsubsets(taste ~ ., data = cheddar[train.1,])
models2 <- regsubsets(taste ~ ., data = cheddar[train.2,])
models3 <- regsubsets(taste ~ ., data = cheddar[train.3,])
models4 <- regsubsets(taste ~ ., data = cheddar[train.4,])

summary(models1)#1 2 y 3 son asi
summary(models4)# este por el contrario tiene a Lactic como 1
MR2adj1 <- summary(models1)$adjr2
MR2adj2 <- summary(models2)$adjr2
MR2adj3 <- summary(models3)$adjr2
MR2adj4 <- summary(models4)$adjr2

summary(models1)$which[which.max(MR2adj1), ]
summary(models2)$which[which.max(MR2adj2), ]
summary(models3)$which[which.max(MR2adj3), ]
summary(models4)$which[which.max(MR2adj4), ]



# Cp de Mallows
MCp1 <- summary(models1)$cp
MCp2 <- summary(models2)$cp
MCp3 <- summary(models3)$cp
MCp4 <- summary(models4)$cp

which.min(MCp1)
summary(models1)$which[which.min(MCp1), ]
summary(models2)$which[which.min(MCp2), ]
summary(models3)$which[which.min(MCp3), ]
summary(models4)$which[which.min(MCp4), ]


# Criterio de Informacion de Bayes (BIC)
MBIC1 <- summary(models1)$bic
MBIC2 <- summary(models2)$bic
MBIC3 <- summary(models3)$bic
MBIC4 <- summary(models4)$bic

which.min(MBIC)
summary(models1)$which[which.min(MBIC1), ]
summary(models2)$which[which.min(MBIC2), ]
summary(models3)$which[which.min(MBIC3), ]
summary(models4)$which[which.min(MBIC4), ]

# Criterio de Informacion de Akaike (AIC)
stepAIC(model.all1, scope = SCOPE, k = 2)
stepAIC(model.all2, scope = SCOPE, k = 2)
stepAIC(model.all3, scope = SCOPE, k = 2)
stepAIC(model.all4, scope = SCOPE, k = 2)

# Notese que en los train 1 2 y 3 solo sale el modelo H2S+Lactic 
# Pero en train4 sale ese y tambien el Lactic 
model.4crit <- lm(taste ~ H2S + Lactic, data=cheddar[train,])
model.1
model.2
model.3
model.4crit
model.4


anova(model.1,model.all1)
anova(model.2,model.all2)
anova(model.3,model.all3)
anova(model.4,model.all4)
anova(model.4crit,model.all4)
# anova(model.4,model.4crit)




# 4) Diagnostico. Comprobaciones de hipotesis, outliers y observaciones influyentes
attach(cheddar)

# Comprobación de hipótesis de los  modelos 


# Linealidad

plot(Lactic, taste,
     main = "RelaciÃ³n entre Taste y Lactic",
     xlab = "Lactic", ylab = "Taste",
     pch = 19, frame = FALSE) 

resettest(model.step, power=2:3, type="regressor", data=cheddar) # p-valor > 0.05 luego aceptamos hipótesis de linealidad

# Normalidad y Autocorrelacion

shapiro.test(resid(model.1)) # como el p-valor > 0.05 no se rechaza la hipótesis nula, i.e. la normalidad
qqnorm(resid(model.1)) # o la funciÃ³n 2 del plot anterior
qqline(resid(model.1))

shapiro.test(resid(model.2))#bien
qqnorm(resid(model.2))
qqline(resid(model.2))

shapiro.test(resid(model.3))#bien
qqnorm(resid(model.3))
qqline(resid(model.3))

shapiro.test(resid(model.4))#bien
qqnorm(resid(model.4))
qqline(resid(model.4))

shapiro.test(resid(model.4crit))#bien
qqnorm(resid(model.4crit))
qqline(resid(model.4crit)

# qqnorm(resid(model.step)) 
# qqline(resid(model.step))
# observamos que las colas no siguen el mismo patrón que el resto de datos, lo que podría indicar que 
#  el modelo no sigue una distribución normal. Sin embargo, al no afectar a un gran porcentaje de los datos y
#  teniendo en cuenta el resultado previo obtenido por el Test de Shapiro-Wilk no rechazamos la hipótesis de normalidad.

 # como el p-valor es mayor que 0.05 se acepta que no hay autocorrelacion en los datos
durbinWatsonTest(model.1) #bien
plot(residuals(model.1), pch = 19)

durbinWatsonTest(model.2) #bien
plot(residuals(model.2), pch = 19)

durbinWatsonTest(model.3) #bien
plot(residuals(model.3), pch = 19)

durbinWatsonTest(model.4) #bien
plot(residuals(model.4), pch = 19)

durbinWatsonTest(model.4crit) #bien
plot(residuals(model.4crit), pch = 19)


# Homocedasticidad
fmodel1 <- fortify(model.1)
fmodel2 <- fortify(model.2)
fmodel3 <- fortify(model.3)
fmodel4 <- fortify(model.4)
fmodel4crit <- fortify(model.4crit)

X1 <- fmodel1$.fitted
Y1 <- fmodel1$.stdresid

X2 <- fmodel2$.fitted
Y2 <- fmodel2$.stdresid

X3 <- fmodel3$.fitted
Y3 <- fmodel3$.stdresid

X4 <- fmodel4$.fitted
Y4 <- fmodel4$.stdresid

X4crit <- fmodel4crit$.fitted
Y4crit <- fmodel4crit$.stdresid

plot(X1, Y1, ylab = "Residuos estandarizados", xlab = "valores ajustados")
plot(X2, Y2, ylab = "Residuos estandarizados", xlab = "valores ajustados")#casi casi hay patron
plot(X3, Y3, ylab = "Residuos estandarizados", xlab = "valores ajustados")
plot(X4, Y4, ylab = "Residuos estandarizados", xlab = "valores ajustados")
plot(X4crit, Y4crit, ylab = "Residuos estandarizados", xlab = "valores ajustados")

ncvTest(model.1) # como el p-valor > 0.05 no hay evidencias para rechazar que la varianza sea constante
ncvTest(model.2)#casi casi
ncvTest(model.3)
ncvTest(model.4)
ncvTest(model.4crit)

# Los residuos se distribuyen de forma homogénea a lo largo de una banda horizontal, luego se verifica la hipótesis



# 
# # Comprobación de hipótesis del modelo 2 (métodos por CRITERIOS)
# plot(model.crit)
# 
# # Linealidad
# plot(H2S, taste,
#      main = "RelaciÃ³n entre Taste y Lactic",
#      xlab = "H2S", ylab = "Taste",
#      pch = 19, frame = FALSE) 
# 
# plot(Lactic, taste,
#      main = "RelaciÃ³n entre Taste y Lactic",
#      xlab = "Lactic", ylab = "Taste",
#      pch = 19, frame = FALSE) 
# 
# 
# resettest(model.crit, power=2:3, type="regressor", data=cheddar) # p-valor > 0.05 luego aceptamos hipótesis de linealidad
# # Este test nos da a entender que este modelo verifica la hipótesis de linealidad con mucha más contundencia que el anterior.
# 
# # Normalidad y Autocorrelacion
# shapiro.test(resid(model.crit)) # como el p-valor > 0.05 no se rechaza la hipótesis nula, i.e. la normalidad
# 
# qqnorm(resid(model.crit)) 
# qqline(resid(model.crit))
# 
# durbinWatsonTest(model.crit) # como el p-valor es mayor que 0.05 se acepta que no hay autocorrelacion en los datos
# 
# # Homocedasticidad
# fmodel <- fortify(model.crit)
# 
# X <- fmodel$.fitted
# Y <- fmodel$.stdresid
# plot(X, Y, ylab = "Residuos estandarizados", xlab = "valores ajustados")
# # Los residuos se distribuyen de forma homogénea a lo largo de una banda horizontal, luego se verifica la hipótesis
# 
# ncvTest(model.crit) # como el p-valor > 0.05 no hay evidencias para rechazar que la varianza sea constante



# Outliers
alpha <- 0.05
n1<-nrow(cheddar[train.1,])
p1 <-ncol(summary(model.1)$coef)

n2<-nrow(cheddar[train.2,])
p2 <-ncol(summary(model.2)$coef)

n3<-nrow(cheddar[train.3,])
p3 <-ncol(summary(model.3)$coef)

n4<-nrow(cheddar[train.4,])
p4 <-ncol(summary(model.4)$coef)

n4crit<-nrow(cheddar[train.4,])
p4crit <-ncol(summary(model.4crit)$coef)
# el valor critico de Bonferroni t_{1-alpha/2n;n-p-1}, n=30,p=3
BCV1 <- qt(1 - alpha / (2 * n1), n1-p1-1) 
BCV2 <- qt(1 - alpha / (2 * n2), n2-p2-1) 
BCV3 <- qt(1 - alpha / (2 * n3), n3-p3-1) 
BCV4 <- qt(1 - alpha / (2 * n4), n4-p4-1) 
BCV4crit <- qt(1 - alpha / (2 * n4crit), n4crit-p4crit-1) 

sum(abs(rstudent(model.1)) > BCV1)
sum(abs(rstudent(model.2)) > BCV2)
sum(abs(rstudent(model.3)) > BCV3)
sum(abs(rstudent(model.4)) > BCV4)
sum(abs(rstudent(model.4crit)) > BCV4crit)

outlierTest(model.1)
outlierTest(model.2) # no hay en ninguno de los dos modelos planteados
outlierTest(model.3)
outlierTest(model.4)
outlierTest(model.4crit)#NA ?



# Observaciones Influyentes
# Criterio 1: valores leverage (hii) mayores que 2p/n

X1 <- model.matrix(~ H2S+Lactic, data = cheddar[train.1,])
X2 <- model.matrix(~ H2S+Lactic, data = cheddar[train.2,])
X3 <- model.matrix(~ H2S+Lactic, data = cheddar[train.3,])
X4 <- model.matrix(~ Lactic, data = cheddar[train.4,])
X4crit <- model.matrix(~ H2S+Lactic, data = cheddar[train.4,])

H1 <- X1 %*% solve(t(X1) %*% X1) %*% t(X1)
H2 <- X2 %*% solve(t(X2) %*% X2) %*% t(X2)
H3 <- X3 %*% solve(t(X3) %*% X3) %*% t(X3)
H4 <- X4 %*% solve(t(X4) %*% X4) %*% t(X4)
H4crit <- X4crit %*% solve(t(X4crit) %*% X4crit) %*% t(X4crit)

hii1 <- diag(H1)
hii2 <- diag(H2)
hii3 <- diag(H3)
hii4 <- diag(H4)
hii4crit <- diag(H4crit)

hCV1 <- 2 * p1 / n1
hCV2 <- 2 * p2 / n2
hCV3 <- 2 * p3 / n3
hCV4 <- 2 * p4 / n4
hCV4crit <- 2 * p4crit / n4crit

sum(hii1 > hCV1)
sum(hii2 > hCV2)
which(hii2>hCV2)#6
sum(hii3 > hCV3)
sum(hii4 > hCV4)
sum(hii4crit > hCV4crit)
which(hii4crit>hCV4crit)#23




# Criterio 2: valores |DFFITS| son mayores que 2*sqrt(p/n)

dffitsCV1 <- 2 * sqrt(p1 / n1)
dffitsCV2 <- 2 * sqrt(p2 / n2)
dffitsCV3 <- 2 * sqrt(p3 / n3)
dffitsCV4 <- 2 * sqrt(p4 / n4)
dffitsCV4crit <- 2 * sqrt(p4crit / n4crit)

dffitsmodel1 <- dffits(model.1)
dffitsmodel2 <- dffits(model.2)
dffitsmodel3 <- dffits(model.3)
dffitsmodel4 <- dffits(model.4)
dffitsmodel4crit <- dffits(model.4crit)

sum(dffitsmodel1 > dffitsCV1)
sum(dffitsmodel2 > dffitsCV2)
sum(dffitsmodel3 > dffitsCV3)
sum(dffitsmodel4 > dffitsCV4)
which(dffitsmodel4 > dffitsCV4)#1
sum(dffitsmodel4crit > dffitsCV4crit)
which(dffitsmodel4crit > dffitsCV4crit)#1, 12



# Criterio 3: valores |DFBETAS| mayores que 2/sqrt(n)
dfbetaCV1 <- 2 / sqrt(n1)
dfbetaCV2 <- 2 / sqrt(n2)
dfbetaCV3 <- 2 / sqrt(n3)
dfbetaCV4 <- 2 / sqrt(n4)
dfbetaCV4crit <- 2 / sqrt(n4crit)

dfbetamodel1 <- dfbeta(model.1)
dfbetamodel2 <- dfbeta(model.2)
dfbetamodel3 <- dfbeta(model.3)
dfbetamodel4 <- dfbeta(model.4)
dfbetamodel4crit <- dfbeta(model.4crit)



sum(dfbetamodel1[, 1] > dfbetaCV1)
sum(dfbetamodel1[, 2] > dfbetaCV1)
sum(dfbetamodel1[, 3] > dfbetaCV1)


sum(dfbetamodel2[, 1] > dfbetaCV2)
sum(dfbetamodel2[, 2] > dfbetaCV2)
sum(dfbetamodel2[, 3] > dfbetaCV2)

sum(dfbetamodel3[, 1] > dfbetaCV3)
sum(dfbetamodel3[, 2] > dfbetaCV3)
sum(dfbetamodel3[, 3] > dfbetaCV3)

sum(dfbetamodel4[, 1] > dfbetaCV4)
sum(dfbetamodel4[, 2] > dfbetaCV4)

sum(dfbetamodel4crit[, 1] > dfbetaCV4crit)
sum(dfbetamodel4crit[, 2] > dfbetaCV4crit)
sum(dfbetamodel4crit[, 3] > dfbetaCV4crit)

which(dfbetamodel1[, 1] > dfbetaCV1)
which(dfbetamodel1[, 3] > dfbetaCV1)
obs1<-c(1,4,5,7,12,14,16,17,19,23,27,29)

which(dfbetamodel2[, 1] > dfbetaCV2)
which(dfbetamodel2[, 3] > dfbetaCV2)
obs2<-c(3,4,6,8,9,12,13,14,16,17,20,23,25,27,28,30)

which(dfbetamodel3[, 1] > dfbetaCV3)
which(dfbetamodel3[, 3] > dfbetaCV3)
obs3<-c(1,3,4,7,8,9,11,12,16,17,20,29,30)

which(dfbetamodel4[, 1] > dfbetaCV4)
which(dfbetamodel4[, 2] > dfbetaCV4)
obs4<-c(1,3,5,8,9,12,13,14,16,19,23,24,27,28,30)

which(dfbetamodel4crit[, 1] > dfbetaCV4crit)
which(dfbetamodel4crit[, 3] > dfbetaCV4crit)
obs4crit<-c(1,3,8,9,11,12,13,14,19,23,24,26,27,28,30)
#no las usaremos pq es exagerado, se os reduce mucho 


# Grafica con la distancia de Cook
influencePlot(model.1)
cheddar[test.1,]
pos_influyentes_1 <- c(1,7,15,19,24)

influencePlot(model.2)
pos_influyentes_2 <- c(6,8,12,15,23)

influencePlot(model.3)
pos_influyentes_3 <- c(1,6,7,8,15)

influencePlot(model.4)
pos_influyentes_4 <- c(1,12,24)

influencePlot(model.4crit)
pos_influyentes_4crit <- c(1,8,12,23)

# Colinealidad. Unicamente la estudiamos en el modelo por CRITERIOS pues en STEP solo interviene Lactic
vif(model.1) # los valores de VIF no indican colinealidad grave
vif(model.2)
vif(model.3)
vif(model.4)#no hay colin por ser una unica variable
vif(model.4crit)




# 5) Errores de Test. Comparacion de Modelos

# Comenzamos eliminando las observaciones influyentes de ambos modelos ya que no hay outliers

cheese1 <- cheddar[-pos_influyentes_1, ]
cheese2 <- cheddar[-pos_influyentes_2, ]
cheese3 <- cheddar[-pos_influyentes_3, ]
cheese4 <- cheddar[-pos_influyentes_4, ]
cheese4crit <- cheddar[-pos_influyentes_4crit, ]


model.exh1 <- regsubsets(taste ~ ., data = cheese1[train.1, ], method = "exhaustive")
model.exh2 <- regsubsets(taste ~ ., data = cheese1[train.2, ], method = "exhaustive")

model.exh3 <- regsubsets(taste ~ ., data = cheese1[train.3, ], method = "exhaustive")

model.exh4 <- regsubsets(taste ~ ., data = cheese1[train.4, ], method = "exhaustive")
model.exh4crit <- regsubsets(taste ~ ., data = cheese1[train.4, ], method = "exhaustive")

summary(model.exh)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvar <- names(coefi)
  mat[, xvar] %*% coefi
}


pos<-which(test.1==TRUE)
for (i in pos){
  i
  
  
}

Y1 <- cheddar[test.1, ]$taste
Y2 <- cheddar[test.2, ]$taste
Y3 <- cheddar[test.3, ]$taste
Y4 <- cheddar[test.4, ]$taste
Y4crit <- cheese3c[test.4, ]$taste



Yhat1 <-predict(obj=model.1,newdata=cheddar[test.1,])
Yhat2 <-predict(obj=model.2,newdata=cheddar[test.2,])
Yhat3 <-predict(obj=model.3,newdata=cheddar[test.3,])
Yhat4 <-predict(obj=model.4,newdata=cheddar[test.4,])
Yhat4crit <-predict(obj=model.4crit,newdata=cheddar[test.4,])

val.errors1 <- mean((Y1 - Yhat1)^2)
val.errors2 <- mean((Y2 - Yhat2)^2)
val.errors3 <- mean((Y3 - Yhat3)^2)
val.errors4 <- mean((Y4 - Yhat4)^2)
val.errors4crit <- mean((Y4crit - Yhat4crit)^2)



for (i in 1:3) {
  Yhat1 <- predict.regsubsets(model.1, newdata = cheese1[test.1, ])
  
  
  
  Yhat1 <- predict.regsubsets(model.exh1, newdata = cheese1[test.1, ], id = i)
  val.errors1[i] <- mean((Y1 - Yhat1)^2)
  Yhat2 <- predict.regsubsets(model.exh2, newdata = cheese2[test.2, ], id = i)
  val.errors2[i] <- mean((Y2 - Yhat2)^2)
  if (i!=3){
    Yhat3b <- predict.regsubsets(model.exh3b, newdata = cheese3b[test.3, ], id = i)
    val.errors3b[i] <- mean((Y3b - Yhat3b)^2)
  }
  Yhat3c <- predict.regsubsets(model.exh3c, newdata = cheese3c[test.3, ], id = i)
  val.errors3c[i] <- mean((Y3c - Yhat3c)^2)
}


val.errors
coef(model.exh, which.min(val.errors))

regfit.best <- regsubsets(taste ~ ., cheddar[-obs.out, ])
coef(regfit.best, which.min(val.errors))




# Validacion cruzada de 1

n <- nrow(cheese)
k <- n # numero de grupos, como es elemento a elemento hay n

folds <- sample(x = 1:k, size = nrow(cheese), replace = FALSE)
cv.errors <- matrix(NA, k, 3, dimnames = list(NULL, paste(1:3)))
for (j in 1:k) {
  best.fit <- regsubsets(taste ~ ., data = cheese[folds != j, ]) # cogemos datos del train
  for (i in 1:3) {
    pred <- predict.regsubsets(best.fit, newdata = cheese[folds == j, ], id = i) # datos test
    cv.errors[j, i] <- mean((cheese$taste[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
coef(best.fit, which.min(mean.cv.errors))



# Validacion en 4 grupos, cambiar la linea de k para otro numero

n <- nrow(cheese)
k <- 4 # numero de grupos

folds <- sample(x = 1:k, size = nrow(cheese), replace = TRUE)
cv.errors <- matrix(NA, k, 3, dimnames = list(NULL, paste(1:3)))
for (j in 1:k) {
  best.fit <- regsubsets(taste ~ ., data = cheese[folds != j, ]) # cogemos datos del train
  for (i in 1:3) {
    pred <- predict.regsubsets(best.fit, newdata = cheese[folds == j, ], id = i) # datos test
    cv.errors[j, i] <- mean((cheese$taste[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
coef(best.fit, which.min(mean.cv.errors))

# Comprobacion
model.cv <- lm(taste ~ H2S + Lactic, data = cheese)
summary(model.cv)
plot(lm(taste ~ H2S + Lactic, data = cheese), which = 1)
plot(lm(taste ~ H2S + Lactic, data = cheese), which = 2)
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
f_teo <- qf(0.9, Q, n - p)
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


######### cosas haciendo boxcox y despues de hacerlo ...######

# training con boxcox y despues lo comparo
bc <- boxCox(model.final1, lambda = seq(-2, 2, 1 / 10), plotit = TRUE)
lambda <- bc$x[which.max(bc$y)]
Y_bc <- (cheddar$taste^lambda - 1) / lambda

modelf2.lm <- lm(Y_bc ~ H2S + Lactic, data = cheddar)

influencePlot(modelf2.lm)
influencePlot(model.final1)

pos_influyentes <- c(1, 6, 7, 15, 28)

cheddar2 <- cheddar
cheddar2$taste <- (cheddar$taste^lambda - 1) / lambda
cheddar
cheddar2

obs.out <- c(1, 6, 7, 15, 28)
cheese2 <- cheddar2[-obs.out, ]

train <- sample(c(TRUE, FALSE), size = nrow(cheese2), replace = TRUE, prob = c(0.70, 0.30))
# conjunto de entrenamiento
test <- (!train)
test
model.exh2 <- regsubsets(taste ~ ., data = cheddar2[train, 1:4], method = "exhaustive")
summary(model.exh)

# la funcion esa que ella siempre copia y pega
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvar <- names(coefi)
  mat[, xvar] %*% coefi
}

val.errors2 <- rep(NA, 3)
Y <- cheddar2[test, ]$taste
for (i in 1:3) {
  Yhat <- predict.regsubsets(model.exh2, newdata = cheddar2[test, ], id = i)
  val.errors2[i] <- mean((Y - Yhat)^2)
}

val.errors2
coef(model.exh2, which.min(val.errors2))

regfit.best <- regsubsets(taste ~ ., cheddar2[-obs.out, 1:4])
coef(regfit.best, which.min(val.errors2))

# esto era antes de boxcox
val.errors
coef(model.exh, which.min(val.errors))

regfit.best <- regsubsets(taste ~ ., cheddar[-obs.out, 1:4])
coef(regfit.best, which.min(val.errors))

# para verlo mÃ¡s visual

val.errors
val.errors2

coef(model.exh2, which.min(val.errors2))
coef(model.exh, which.min(val.errors))

coef(regfit.best, which.min(val.errors2))
coef(regfit.best, which.min(val.errors))


# 6) Conclusion

model.final <- model.final1 ############ tomar el mejor modelo de 4
summary(model.final)
# En el summary podemos observar tanto los valores de betahat, sus p-valores y sigma

coeff <- summary(model.final)$coeff[,1]
# La ecuación de nuestro modelo final es y = beta0 + beta1*x_H + beta2*x_L,
#    donde x_H y x_L denotan los valores observados de H2S y Lactic.

rse <- sqrt(deviance(model.final)/df.residual(model.final))
pval <- summary(model.final)$coeff[,4]

anova(model.final)


# Observamos como se distribuye la variable taste en función de H2S y Lactic
plot_ly(x=H2S, y=Lactic, z=taste, type="scatter3d", color=taste) %>% 
  layout(scene = list(xaxis = list(title = 'H2S (%)'),
                      yaxis = list(title = 'Lactic (%)'),
                      zaxis = list(title = 'Taste (0-100)')))


# Vemos el plano de regresion del modelo propuesto.
# En rojo se marcan las observaciones que peor se ajustan al modelo.
planereg <- scatterplot3d(x=H2S, y=Lactic, z=taste, pch=16, cex.lab=1,
                          highlight.3d=TRUE, type="h", xlab='H2S (%)',
                          ylab='Lactic (%)', zlab='Taste (0-100)')
planereg$plane3d(model.final, lty.box="solid", col='mediumblue')