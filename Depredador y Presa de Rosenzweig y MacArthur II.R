
#	        Modelo de Depredador y Presa 
#         de Rosenzweig y MacArthur II

# Se simula la dinámica del modelo de depredador 
# y presa de Rosenzweig y MacArthur (1963), en 
# este caso la modificación al modelo original 
# de Lotka y Volterra, consistente en incorporar
# una respuesta funcional tipo II al
#  depredador: a*Presa/(D + Presa). 

# "D" es un parámetro que se introduce
# para que la respuesta funcional 
# se sature.

# Respuesta funcional tipo II

a <- 0.5 # Tasa máxima de consumo
D <- 25 # Densidad de presa en la cual la 
# tasa de consumo es la mitad de su valor máximo
Presa <- seq(1, 200, by = 1)

RespuestaFuncionalTipoII <- a * Presa / (D + Presa)

plot(Presa, RespuestaFuncionalTipoII, 
     type = "l",
     xlab = "Densidad de la Presa ", 
     ylab = "Tasa de consumo del Depredador")


# Modelo Rosenzweig y MacArthur con Respuesta funcional tipo II

RosenzweigMacArthurII <- function(Tiempo, CondIni, Parametros) {
  Presa <- CondIni[1]
  Depredador <- CondIni[2]
  with(as.list(Parametros), {
    dPresa.dt <- b * Presa * - a *  Presa/(D + Presa) * Depredador 
    dDepredador.dt <- e * a *  Presa/(D + Presa) * Depredador  - s * Depredador
    return(list(c(dPresa.dt, dDepredador.dt)))
  })
}  # Esta función implementa en modelo de  
# depredador y presa de Rosenzweig y MacArthur. 

library(deSolve) # Abre la librería deSolve.

b <- 1.5; e <- 0.07; s <- 0.2; a <- 5; D <- 20 
# Se asignan valores a los parámetros del modelo.

Parametros <- c(b = b, e = e, a = a, D = D) 
# Se crea un vector donde se guardan los valores 
# de los parámetros del modelo.

Tiempo <- seq(0, 1000, by = 10) 
# Se crea un vector en el cual se guarda los 
# tiempos en los cuales se va a evaluar el modelo.


# Evaluando el modelos bajo diferentes
# condiciones iniciales

par(mfrow = c(2, 2)) # Se crean 4 espacios 
# para graficar.

Resultado1<- ode(c(Presa0 = 85, Depredador0 = 40), 
                 Tiempo, RosenzweigMacArthurII, 
                 Parametros) 
# Esta instrucción evalúa el modelo a partir 
# de las densidades iniciales Presa0 = 25 
# y Depredador0 = 5.

matplot(Tiempo, (Resultado1 [,2:3]), 
        type = "l", ylab = "Densidades", 
        main="Densidades Originales: Presa = 25,
        Depredador = 5")

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), 
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.

Resultado2<- ode(c(Presa0 = 500, Depredador0 = 15), 
                 Tiempo, 
                 RosenzweigMacArthurII, 
                 Parametros) 
# Esta instrucción evalúa el modelo a partir 
# de las densidades iniciales Presa0 = 25 
# y Depredador0 = 5.

matplot(Tiempo, (Resultado2 [,2:3]), 
        type = "l", ylab = "Densidades", 
        main="Densidades Originales: Presa = 500, 
        Depredador = 15")

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), 
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.

Resultado3<- ode(c(Presa0 = 300, Depredador0 = 45), 
                 Tiempo, 
                 RosenzweigMacArthurII, 
                 Parametros) 
# Esta instrucción evalúa el modelo a partir 
# de las densidades iniciales: Presa0 = 25 
# y Depredador0 = 5.

matplot(Tiempo, (Resultado3 [,2:3]), 
        type = "l", 
        ylab = "Densidades", 
        main="Densidades Originales: Presa = 300, 
        Depredador = 50")

legend("right", c(expression("Presa "), 
                  expression("Depredador ")),
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.

Resultado4<- ode(c(Presa0 = 350, Depredador0 = 30), 
                 Tiempo, 
                 RosenzweigMacArthurII, 
                 Parametros) 
# Esta instrucción evalúa el modelos a partir 
# de las densidades iniciales Presa0 = 140 
# y Depredador0 = 45.

matplot(Tiempo, (Resultado4 [,2:3]), 
        type = "l", 
        ylab = "Densidades", 
        main="Densidades Originales: Presa = 150, 
        Depredador = 45")

legend("right", c(expression("Presa "), 
                  expression("Depredador ")),
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.


# Diagrama de fases

par(mfrow = c(1, 1))

plot(Resultado1[, 2], Resultado1 [, 3], 
     type = "l", 
     ylab = "Depredador", 
     xlab = "Presa", 
     main="Diagrama de Fases", 
     col=4, xlim = c(0, 500), 
     ylim = c(0, 50))

# Se grafica un diagrama de fases de P contra H. 

points(85, 40, cex = 1.5, pch = 19)  
# Se agrega un punto en las densidades 
# iniciales estipuladas en Resultado 1.

lines(Resultado2[, 2], Resultado2[, 3], col=2) 
# Se agrega una línea que describe la trayectoria 
# del sistema bajo la condición inicial estipulada 
# en Resultados 2.

points(500, 15, cex = 1.5) 
# Se agrega un punto en las densidades iniciales 
# estipuladas en Resultado 2.

lines(Resultado3[, 2], Resultado3[, 3], col=3) 
# Se agrega una línea que describe la trayectoria 
# del sistema bajo la condición inicial estipulada 
# en Resultados 3.

points(300, 45, cex = 1.5, pch = 2) 
# Se agrega un punto en las densidades iniciales 
# estipuladas en Resultado 3.

lines(Resultado4[, 2], Resultado4[, 3], col=6) 
# Se agrega una línea que describe la trayectoria 
# del sistema bajo la condición inicial estipulada 
# en Resultados 4.

points(350, 30, cex = 1.5, pch = 3) 
# Se agrega un punto en las densidades iniciales 
# estipuladas en Resultado 4.

# Rosenzweig, M.  MacArthur, R. 1963. Graphical 
# representation and stability conditions of 
# predator-prey interaction, American Naturalist 
# 97, 209-223.

