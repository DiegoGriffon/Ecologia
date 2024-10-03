
#         Modelo de Depredador y Presa 
#         de Rosenzweig y MacArthur III

# Se simula la dinámica del modelo de depredador 
# y presa de Rosenzweig y MacArthur (1963), 
# en este caso la modificación al modelo 
# original de Lotka y Volterra consistente en 
# incorporar competencia intraespecífica en la 
# ecuación de la presa (1- αH) y una respuesta 
# funcional tipo II por parte de depredador 
# (H/(D+H)).
# Donde “α” es el inverso de la capacidad de carga 
# de la presa, “a” es la tasa máxima de captura de 
# presas y “D” es un parámetro que se introduce 
# para que la respuesta funcional se sature.

RosenzweigMacArthur <- function(Tiempo, CondIni, p) {
  Presa <- CondIni[1]
  Depredador <- CondIni[2]
  with(as.list(p), {
    dPresa.dt <- b * Presa * (1 - alfa * Presa) - a * Presa/(D + Presa) * Depredador 
    dDepredador.dt <- e * a *  Presa/(D + Presa) * Depredador - s * Depredador
    return(list(c(dPresa.dt, dDepredador.dt)))
  })
} # Esta función implementa en modelo de depredador 
# y presa de Rosenzweig y MacArthur. 

library(deSolve) # Abre la librería deSolve.

b <- 0.8; e <- 0.07; s <- 0.2; a <- 5
D <- 400; alfa <- 0.001; H <- 0:(1/alfa) 
# Se asignan valores a los parámetros del modelo.

Parametros <- c(b = b, e = e, a = a, D = D, 
                alfa = alfa, H= H) 
# Se crea un vector donde se guardan los valores de 
# los parámetros del modelo.

Tiempo <- seq(0, 1000, by = 10) 
# Se crea un vector en el cual se guarda los tiempos 
# en los cuales se va a evaluar el modelo.


# Evaluando el modelos bajo diferentes
# condiciones iniciales

par(mfrow = c(2, 2)) # Se crean 4 espacios 
# para graficar.

Resultado1<- ode(c(Presa0 = 25, Depredador0 = 5), 
                 Tiempo, 
                 RosenzweigMacArthur, 
                 Parametros) 
# Esta instrucción evalúa el modelos a partir
# de las densidades iniciales: Presa0 = 25 y 
# Depredador0 = 5.

matplot(Tiempo, (Resultado1 [,2:3]), 
        type = "l", 
        ylab = "Densidades", 
        main="Densidades Originales: Presa = 25, 
        Depredador = 5")

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), 
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.

Resultado2<- ode(c(Presa0 = 500, Depredador0 = 15), 
                 Tiempo, 
                 RosenzweigMacArthur,
                 Parametros) 
# Esta instrucción evalúa el modelos a partir de 
# las densidades iniciales: Presa0 = 25 
# y Depredador0 = 5.

matplot(Tiempo, (Resultado2 [,2:3]), 
        type = "l", 
        ylab = "Densidades", 
        main="Densidades Originales: Presa = 500, 
        Depredador = 15")

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), 
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.

Resultado3<- ode(c(Presa0 = 300, Depredador0 = 50), 
                 Tiempo, 
                 RosenzweigMacArthur, 
                 Parametros) 
# Esta instrucción evalúa el modelos a partir de 
# las densidades iniciales: Presa0 = 25 y 
# Depredador0 = 5.

matplot(Tiempo, (Resultado3 [,2:3]), 
        type = "l", 
        ylab = "Densidades", 
        main="Densidades Originales: Presa = 300, 
        Depredador = 50")

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), 
       lty = 1:2, 
       bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.


# Diagrama de fases

plot(Resultado1[, 2], Resultado1 [, 3], 
     type = "l", 
     ylab = "Depredador", 
     xlab = "Presa", 
     main="Diagrama de Fases", col=4) 
# Se grafica un diagrama de fases de Depredador
#  contra Presa para el Resultado 1. 

points(25, 5, cex = 1.5, pch = 19)  
# Se agrega un punto en las densidades iniciales 
# estipuladas en el Resultado 1.

lines(Resultado2[, 2], Resultado2[, 3], col=2) 
# Se agrega una línea que describe la trayectoria 
# del sistema bajo la condición inicial estipuladas
# en el Resultado 2.

points(500, 15, cex = 1.5) # Se agrega un punto en 
# las densidades iniciales estipuladas en  
# el Resultado 2.

lines(Resultado3[, 2], Resultado3[, 3], col=3) 
# Se agrega una línea que describe la trayectoria 
# del sistema bajo la condición inicial estipulada 
# en el Resultado 3.

points(300, 50, cex = 1.5, pch = 2) 
# Se agrega un punto en las densidades iniciales
# estipuladas en  el Resultado 3.

# Rosenzweig, M.  MacArthur, R. 1963. Graphical 
# representation and stability conditions of 
# predator-prey interaction, American Naturalist 
# 97, 209-223.
