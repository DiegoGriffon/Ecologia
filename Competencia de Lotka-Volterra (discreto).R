
#	   Modelo de Competencia de Lotka-Volterra (discreto)

LotkaVolterraComp<- function(N, Inter, rd = c(1, 1)) { 
  N1prox <- N[1] + rd[1] * N[1] * (1 -Inter[1, 1] * N[1] - Inter[1, 2] * N[2])
  N2prox <- N[2] + rd[2] * N[2] * (1 - Inter[2, 1] * N[1] -Inter [2, 2] * N[2]) 
  c(N1prox, N2prox) } 
# Esta función implementa en modelo de competencia
# Lotka-Volterra en tiempo discreto. De esta forma,
# en función de las densidades poblacionales en un 
# tiempo t, se puede obtener los valores de las densidades 
# en t+1. El parámetro llamado Inter es igual a αij/K.

Interacciones <- matrix(c(0.01, 0.005, 0.008, 0.01), 
                        ncol = 2, 
                        byrow = TRUE) # Esta instrucción 
# crea una matriz que tiene como entradas los valores del 
# parámetro Inter. En este caso K=100. Tomar en cuenta 
# que el comando byrow asegura que la matriz se construya 
# fila a fila. 

Interacciones

t <- 20 # Número de generaciones a iterar el modelo.

N <- matrix(NA, nrow = t + 1, ncol = 2) # Esta instrucción 
# crea una matriz en la cual se van a guardar los resultados 
# de la iteración del modelo. 

N 

N[1, ] <- c(10, 10) # Se asignan las densidades iniciales.

N 

for (i in 1:t) N[i + 1, ] <- LotkaVolterraComp (N[i, ], 
                                                Interacciones) 
# Ciclo “For” que itera la función a partir de las densidades 
# iniciales.

matplot(0:t, N, type = "l", col = 1, ylim = c(0, 110)) 
# Se grafican los resultados.

abline(h = 1/Interacciones[1, 1], lty = 3) 
# Se agrega una línea en el valor de K.

text(0, 1.02/Interacciones[1, 1], "K", adj = c(0, 0))
# Se agrega la letra K a la línea creada anteriormente.

legend("right", c(expression("Sp.1 " * (Inter[21] == 0.008)), 
                  expression("Sp.2 " * (Inter[12] == 0.005))), 
       lty = 1:2,bty = "n") 
# Se agrega una leyenda en la cual se identifican las 
# curvas de cada especie y se muestra el valor del 
# parámetro Inter. 
