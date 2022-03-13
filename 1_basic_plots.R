install.packages('faraway')
library('faraway')

data(cheddar)
print(cheddar)
summary(cheddar)

# Histogramas de todas las variables

ids = names(cheddar)
m = dim(cheddar)[2]


#######   HISTOGRAM 1   ############
layout(matrix(1:m, nrow = 1))
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


#######   BOX PLOT 1 ############
layout(matrix(1:m, nrow = 1))
y_lab_string = "Quantity";
for (id in ids){
  boxplot(cheddar[,id], ylab = y_lab_string, xlab=id, varwidth = TRUE)
  y_lab_string="";
}


###### LINEAR REGRESSION 1 ############
#par(mar = c(2,2,2,2))
layout(matrix(1:(m*m), nrow = m))
i=0;j=0;
for (id_x in ids){
  for (id_y in ids){
    if (i==0){
      y_lab_string = id_y
    }else{
      y_lab_string = "";
    }
    
    if (j==0){
      x_lab_string = id_x
    }else{
      x_lab_string = "";
    }
    X = cheddar[,id_x]
    Y = cheddar[,id_y]
    plot(X,Y, ylab=y_lab_string, xlab=x_lab_string)
    abline(lm(Y ~ X, data=cheddar), col = "blue")
    j=j+1;  
  }
  i=1+1;
}

