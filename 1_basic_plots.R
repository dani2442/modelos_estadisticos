install.packages('faraway')
library('faraway')

data(cheddar)
print(cheddar)
summary(cheddar)

# Histogramas de todas las variables

ids = names(cheddar)
layout(matrix(1:4, nrow = 1))
y_lab_string = "Quantity";
for (id in ids){
  hist(cheddar[,id], xlab=id, ylab=y_lab_string, main = paste("Histogram of ", id))
  y_lab_string="";
}
cheddar[c("taste")]


# Gráficos de las relaciones entre todas las variables.

plot(cheddar)

# Gráficos de dispersión entre la variable respuesta "Taste" y
#  las variables predictoras "Acetic", "H2S" y "Lactose".

T <- cheddar$taste
A <- cheddar$Acetic
H <- cheddar$H2S
L <- cheddar$Lactic

plot(A, T, main = "Relación entre Taste y Acetic",
     xlab = "Acetic", ylab = "Taste",
     pch = 20, frame = FALSE)

plot(H, T, main = "Relación entre Taste y H2S",
     xlab = "H2S", ylab = "Taste",
     pch = 20, frame = FALSE)

plot(L, T, main = "Relación entre Taste y Lactic",
     xlab = "Lactic", ylab = "Taste",
     pch = 19, frame = FALSE)


