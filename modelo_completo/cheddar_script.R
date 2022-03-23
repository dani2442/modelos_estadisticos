install.packages("faraway")
install.packages("leaps")
install.packages("MASS")
install.packages("PASWR")
install.packages("car")
install.packages("ggplot2")
install.packages("GGally")
install.packages("corrplot")
install.packages("scatterplot3d")


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




# 1) Introduccion

data(cheddar)

# Variable Respuesta: taste
# Variables Predictoras: Acetic, H2S, Lactic


# Estudiamos el tipo de las variables que van a formar parte de los posibles modelos
sapply(cheddar, class)
head(cheddar)

# Comprobamos que no hay entradas vacias
any(is.na(cheddar))

# Separacion del dataset en conjuntos de entrenamiento y test (70-30%)
set.seed(1) 
train <- sample(c(TRUE, FALSE), size = nrow(cheddar), replace = TRUE, prob = c(0.7, 0.3))
train
test <- (!train)


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
     pch = 20, frame = FALSE)


plot(H2S, taste,
     main = "RelaciÃ³n entre Taste y H2S",
     xlab = "H2S", ylab = "Taste",
     pch = 20, frame = FALSE)


plot(Lactic, taste,
     main = "RelaciÃ³n entre Taste y Lactic",
     xlab = "Lactic", ylab = "Taste",
     pch = 19, frame = FALSE)


layout(matrix(1:1, nrow = 1))



# 2) Estudio y evaluacion del modelo completo
attach(cheddar[train,])

x <- model.matrix( ~ Acetic + H2S + Lactic, data = cheddar[train,])
betahat <- solve(crossprod(x, x), crossprod(x, taste))
betahat <- c(betahat)
betahat

# Comprobamos el resultado con funciones ya implementadas
model.all <- lm(taste ~ ., data = cheddar[train,])
summary(model.all)
model.all$coefficients

# Correlaciones y tabla de resultados con el estudio de sus p-valores
cor(cheddar[train,])
ggpairs(cheddar[train,])

mat_cor <- cor(cheddar, method = "pearson")
corrplot(mat_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

anova(model.all)


# outlierTest(model.all) # comprobamos si hay outliers (no)

# influenceIndexPlot(model.all)



# 3) Seleccion del mejor modelo. Metodos por pasos y por criterios

# i) BACKWARD (alpha=0.05)

drop1(model.all, test = "F")
# quitamos Acetic del modelo por ser la de mayor p-valor

model.updateB1 <- update(model.all, . ~ . - Acetic)
drop1(model.updateB1, test = "F")
# quitamos H2S del modelo por ser la unica variable con p-valor > 0.05

model.updateB2 <- update(model.updateB1, . ~ . - H2S)
drop1(model.updateB2, test = "F")
# termina el proceso pues el p-valor de Lactic supera a alpha

model.final1 <- lm(taste ~ Lactic, data = cheddar[train,])
summary(model.final1)


# ii) FORWARD (alpha=0.05)

SCOPE <- (~ . + Acetic + H2S + Lactic)
model.inicial <- lm(taste ~ 1, data = cheddar[train,]) # solo el termino independiente

add1(model.inicial, scope = SCOPE, test = "F")
# Añadimos Lactic por ser la variable predictora con menor p-valor
model.updateF1 <- update(model.inicial, . ~ . + Lactic)

add1(model.updateF1, scope = SCOPE, test = "F")
# no añadimos ninguna variable pues sus p-valores superan la barrera de alpha

model.final2 <- lm(taste ~ Lactic, data = cheddar[train,])
summary(model.final2)
# Nótese que los modelos obtenidos por metodos de pasos coinciden

model.step <- lm(taste ~ Lactic, data = cheddar[train,])
anova(model.step, model.all)


# iii) CRITERIOS

# R2 ajustado
models <- regsubsets(taste ~ ., data = cheddar[train,])
summary(models)
MR2adj <- summary(models)$adjr2
MR2adj
which.max(MR2adj)
summary(models)$which[which.max(MR2adj), ]

# Cp de Mallows
MCp <- summary(models)$cp
MCp
which.min(MCp)
summary(models)$which[which.min(MCp), ]

# Criterio de Informacion de Bayes (BIC)
MBIC <- summary(models)$bic
MBIC
which.min(MBIC)
summary(models)$which[which.min(MBIC), ]

# Criterio de Informacion de Akaike (AIC)
stepAIC(model.all, scope = SCOPE, k = 2)

# Notese que los modelos obtenidos por metodos de criterios coinciden.
model.crit <- lm(taste ~ H2S + Lactic, data=cheddar[train,])
anova(model.crit, model.all)


# 4) Diagnostico
attach(cheddar)

plot(model.final1)
fmodel <- fortify(model.final1)
head(fmodel)

# Normalidad y Autocorrelacion
shapiro.test(resid(model.final1)) # normalidad de los residuos
qqnorm(Model$.stdresid) # o la funcion 2 del plot anterior

durbinWatsonTest(model.final1) # no correlacion de errores
# es en este en el que se suponen en un tiempo (INDEX)

plot(residuals(model.final1), pch = 19)
plot(fmodel$.resid, ylab = "Residuos")


# INTRODUCIR HIPOTESIS DE MEDIA ERRORES NULA
# ----------


# Bonferroni
alpha <- 0.05
BCV <- qt(1 - alpha / (2 * 30), 26) # el valor crÃ­tico de Bonferroni t_{1-alpha/2n;n-p-1}, n=30,p=3
BCV
sum(abs(rstudent(model.final1)) > BCV)
which.max(abs(rstudent(model.final1)))


# Residuos Estandarizados
X <- fmodel$.fitted
Y <- fmodel$.stdresid
plot(X, Y, ylab = "Residuos Estandarizados", xlab = "Valores Ajustados")
segments(5, 0, 40, 0)
# en esa grafico hablar de homocedasticidad (varianza constante)
sort(abs(rstandard(model.final1)), decreasing = TRUE)[1:3]

# mas  formas de verlo(formula del paquete car)(sino ver test White en otro package)
ncvTest(model.final1) # p valor "grande" no hay evidencias para rechazar que sea cte


# Outliers y High Leverage
outlierTest(model.final1) # no hay outliers

# Criterio 1: valores leverage (hii) mayores que 2p/n
X <- model.matrix(~ H2S + Lactic, data = cheddar)
H <- X %*% solve(t(X) %*% X) %*% t(X)
hii <- diag(H)

hCV <- 2 * 3 / 30
sum(hii > hCV)
which(hii > hCV) # 6


# Criterio 2: valores |DFFITS| son mayores que 2*sqrt(p/n)
dffitsCV <- 2 * sqrt(3 / 30)
dffitsmodel <- dffits(model.final1)

sum(dffitsmodel > dffitsCV)

# Criterio 3: valores |DFBETAS| mayores que 2/sqrt(n)
dfbetaCV <- 2 / sqrt(30)
dfbetamodel <- dfbeta(model.final1)
dfbetamodel

sum(dfbetamodel[, 1] > dfbetaCV)
sum(dfbetamodel[, 2] > dfbetaCV)
sum(dfbetamodel[, 3] > dfbetaCV)

which(dfbetamodel[, 1] > dfbetaCV)
which(dfbetamodel[, 3] > dfbetaCV)

# Grafica con la distancia de Cook
influencePlot(model.final1)
pos_influyentes <- c(6, 7, 8, 12, 15)


# Eliminamos estas observaciones y estudiamos el resultado
obs.out <- c(6, 7, 8, 12, 15)
cheese <- cheddar[-obs.out, ]

set.seed(1) # establecemos la semilla y distribuimos 70-30%
train <- sample(c(TRUE, FALSE), size = nrow(cheese), replace = TRUE, prob = c(0.7, 0.3))
test <- (!train)

model.exh <- regsubsets(taste ~ ., data = cheddar[train, ], method = "exhaustive")
summary(model.exh)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvar <- names(coefi)
  mat[, xvar] %*% coefi
}

val.errors <- rep(NA, 3)
Y <- cheddar[test, ]$taste
for (i in 1:3) {
  Yhat <- predict.regsubsets(model.exh, newdata = cheddar[test, ], id = i)
  val.errors[i] <- mean((Y - Yhat)^2)
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

