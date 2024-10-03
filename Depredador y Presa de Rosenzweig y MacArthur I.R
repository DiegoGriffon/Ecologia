

#	Modelo de Depredador y Presa de Rosenzweig y MacArthur I


# Se simula la dinámica del modelo de depredador 
# y presa de Rosenzweig y MacArthur (1963), este
# modelo es una modificación al modelo 
# original de Lotka y Volterra consistente en 
# incorporar competencia intraespecifica en la 
# ecuación de la presa (1- αH) 

RosenzweigMacArthur1 <- function(Tiempo, CondIni, Parametros) {
  P <- CondIni[1]
  D <- CondIni[2]
  with(as.list(Parametros), {
    dP.dt <- b * P * (1 - alfa * P) - a * D * P
    dD.dt <- e * a * D * P - s * D
    return(list(c(dP.dt, dD.dt)))
  })
} # Esta función implementa en modelo de depredador 
# y presa de Rosenzweig y MacArthur. 

library(deSolve) # Abre la librería deSolve.

b <- 0.5; a <- 0.01; e <- 0.1; s <- 0.2; alfa <- 0.001 
# Se asignan valores a los parámetros del modelo.

Parametros <- c(b = b, a = a, e = e, s = s, alfa = alfa) 
# Se crea un vector donde se guardan los valores de 
# los parámetros del modelo.

Tiempo <- seq(0, 1000, by = 10) # Se crea un vector en 
# el cual se guarda los tiempos en los cuales se va a 
# evaluar el modelo.

par(mfrow = c(2, 2)) # Se crean 4 espacios 
# para graficar.

Resultado1<- ode(c(P0 = 25, D0 = 5), 
                 Tiempo, RosenzweigMacArthur1, 
                 Parametros) 
# Esta instrucción evalúa el modelo a partir 
# de las densidades iniciales P0 = 25 y D0 = 5.

matplot(Tiempo, (Resultado1 [,2:3]), 
        type = "l", 
        ylab = "Densidades", 
        main="Densidades Originales H = 25, P = 5")

legend("right", c(expression("Presa "), 
                  expression("Depredador")),
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.

Resultado2<- ode(c(P0 = 500, D0 = 15), 
                 Tiempo, RosenzweigMacArthur1, 
                 Parametros) # Esta instrucción 
# evalúa el modelo a partir de las densidades 
# iniciales P0 = 25 y D0 = 5.

matplot(Tiempo, (Resultado2 [,2:3]), 
        type = "l", ylab = "Densidades",
        main="Densidades Originales H = 500, P = 15")

legend("right", c(expression("Presa"), 
                  expression("Depredador")), 
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.

Resultado3<- ode(c(P0 = 300, D0 = 50), 
                 Tiempo, 
                 RosenzweigMacArthur1, 
                 Parametros) # Esta instrucción 
# evalúa el modelo a partir de las densidades 
# iniciales P0 = 25 y D0 = 5.

matplot(Tiempo, (Resultado3 [,2:3]), 
        type = "l", ylab = "Densidades", 
        main="Densidades Originales H = 300, P = 50")

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), lty = 1:2, bty = "n", col=1:2)
# Se agrega una leyenda que identifica las curvas.

# Diagrama de fases
plot(Resultado1[, 2], Resultado1 [, 3], 
     type = "l", ylab = "P", xlab = "H", 
     main="Diagrama de Fases", col=4) 
# Se grafica un diagrama de fases de D contra P. 

points(25, 5, cex = 1.5, pch = 19)  
# Se agrega un punto en las densidades 
# iniciales estipuladas en Resultado 1.

lines(Resultado2[, 2], Resultado2[, 3], col=2) 
# Se agrega una línea que describe la trayectoria 
# del sistema bajo la condición inicial estipulada 
# en Resultados 2.

points(500, 15, cex = 1.5) 
# Se agrega un punto en las densidades 
# iniciales estipuladas en Resultado 2.

lines(Resultado3[, 2], Resultado3[, 3], col=3) 
# Se agrega una línea que describe la trayectoria 
#cdel sistema bajo la condición inicial estipulada 
# en Resultados 3.

points(300, 50, cex = 1.5, pch = 2) 
# Se agrega un punto en las densidades 
#iniciales estipuladas en Resultado 2.


# Rosenzweig, M.  MacArthur, R. 1963. Graphical 
# representation and stability conditions of 
# predator-prey interaction, American Naturalist 
# 97, 209-223.

