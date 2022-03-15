## * Cheddar ------------------------------
library(faraway)
data(cheddar)
head(cheddar)

# Tipos de datos

# Librerias
library(car)

typeof(cheddar$taste)
typeof(cheddar$Acetic)
typeof(cheddar$H2S)
typeof(cheddar$Lactic)

# Todos son dobles


# Comprobacion de NA
rango <- ncol(cheddar)
for (i in 1:rango) {
  if (any(is.na(cheddar[, i]))) {
    next
  }
}

# Concluimos que no hay NA, asi que no tenemos que tratarlos

# Definicion de variables

Y <- cheddar$taste
x1 <- cheddar$Acetic
x2 <- cheddar$H2S
x3 <- cheddar$Lactic

# ¿Es normal la variable respuesta?
shapiro.test((cheddar$taste))
# P-valor 0.0972 > 0.05 pero es relativamente mala (sqrt mejor)


# Estudio de outliers (seccion en cada modelo)


# Comprobaciones

plot(cheddar)
scatterplotMatrix(x = cheddar)

# ¿Atributos normales?

aux <- 1
for (variable in cheddar) {
  if (shapiro.test(variable)[2] < 0.1) {
    print(paste("La variable", colnames(cheddar[aux]), "no es normal alfa = 0.1", "p-valor =", toString(shapiro.test(variable)[2])))
  }
  aux <- aux + 1
}
aux <- 1


# Transformaciones adecuadas

# ¿Se distribuye el error de manera normal?

# shapiro.test(resid(model4.lm)) depende del modelo

# Modelos preliminares

# Uno backward
model.all <- lm(Y ~ x1 + x2 + x3, data = cheddar)
drop1(model.all, test = "F")

# Quitamos x1(acetic)

model.update1 <- update(model.all, . ~ . - x1)
drop1(model.update1, test = "F")

# Todos los p-valores < 0.05  ==> tenemos modelo

model.final <- lm(Y ~ x2 + x3, data = cheddar)
summary(model.final)

# ¿Se distribuye el error normal?

shapiro.test(resid(model.final))
# p-valor = 0.8107 ==> se distribuyen normal

# ¿Tenemos outliers?

outlierTest(model.final)
# No los tenemos Bonferroni p > 0.18

# influenceIndexPlot(modelo.final) esta queda bien para explicaciones

# Transformaciones adecuadas (mejoras)

bc <- boxCox(model.final, lambda = seq(-2, 2, 1 / 10), plotit = TRUE)
lambda <- bc$x[which.max(bc$y)] # no hay razonones naturales para tomar otro(2/3)

Y_bc <- (Y^lambda - 1) / lambda
# Busca como se hace el fit en la boxcox, ponlo a mano ahora

df <- data.frame(Y_bc, x1, x2, x3)
modelo2.lm <- lm(Y_bc ~ x2 + x3, data = df)

# ¿Se distribuye el error normal?

shapiro.test(resid(modelo2.lm))
# p-valor = 0.9893 ==> se distribuyen normal

# ¿Tenemos outliers?

outlierTest(modelo2.lm)

# No ,  Bonf p > 0.57

summary(modelo2.lm)
summary(model.final)