

#  Modelo de Depredador y Presa de Lotka y Volterra


DepredadorPresaLV <- function(t, CondIni, Parametros) {
  P <- CondIni[1]
  D <- CondIni[2]
  with(as.list(Parametros), {
    dP.dt <- b * P - a * P * D
    dD.dt <- e * a * D * P - s * D
    return(list(c(dP.dt, dD.dt)))
  })
} 

# Esta función implementa el modelo de depredador 
# y presa de Lotka y Volterra. 

library(deSolve) # Abre la librería deSolve.

b <- 0.5; a <- 0.01; e <- 0.1; s <- 0.2 # Se asignan 
# valores a los parámetros del modelo.

Parametros <- c(b = b, a = a, s = s, e = e) # Se crea 
# un vector donde se guardan los valores de los parámetros
# del modelo.

Tiempo <- seq(0, 100, by = 0.1) # Se crea un vector en 
# el cual se guarda los tiempos en los cuales se va a 
# evaluar el modelo.

Resultado1 <- ode(c(P0 = 25, D0 = 5), 
                  Tiempo, 
                  DepredadorPresaLV, 
                  Parametros) # Esta instrucción 
# evalúa el modelo a partir de las densidades 
# iniciales V0 = 25 y P0 = 5.

matplot(Tiempo, (Resultado1 [,2:3]), 
        type = "l", 
        ylab = "Densidades") # Se grafican los resultados

legend("right", c(expression("Presa"), 
                  expression("Depredador")), 
       lty = 1:2, bty = "n", col=1:2) # Se agrega una 
# leyenda que identifica las curvas.

# /////////////////////////////////////////////////////////////////////////////////////
# Se van a graficar 3 casos diferentes y un resumen de 
# los tres en un diagrama de fases

par(mfrow = c(2, 2)) # Se crean 4 espacios 
# para graficar.

matplot(Tiempo, (Resultado1 [,2:3]), 
        type = "l", ylab = "Densidades",
        main="Densidades Originales D = 25, P = 5") 
# Se grafica el primer resultado (Resultado1) en 
# el primer espacio.

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), 
       lty = 1:2, bty = "n", col=1:2) # Se agrega 
# una leyenda que identifica las curvas.

Resultado2<- ode(c(P0 = 500, D0 = 15), Tiempo, 
                 DepredadorPresaLV, Parametros) 
# Esta instrucción evalúa el modelo a partir de 
# las densidades iniciales D0 = 500 y P0 = 15.

matplot(Tiempo, (Resultado2 [,2:3]), 
        type = "l", 
        ylab = "Densidades", 
        main="Densidades Originales D = 500, P = 15") #
# Se grafica el segundo resultado (Resultado2) en 
# el segundo espacio.

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), 
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.

Resultado3 <- ode(c(P0 = 300, D0 = 50), 
                  Tiempo, DepredadorPresaLV,
                  Parametros) # Esta instrucción 
# evalúa el modelo a partir de las densidades 
# iniciales D0 = 300 y P0 = 50.

matplot(Tiempo, (Resultado3 [,2:3]), 
        type = "l", 
        ylab = "Densidades", 
        main="Densidades Originales D = 300, P = 50") 
# Se grafica el tercer resultado (Resultado3) en 
# el tercer espacio.

legend("right", c(expression("Presa "), 
                  expression("Depredador ")), 
       lty = 1:2, bty = "n", col=1:2) # Se agrega una 
# leyenda que identifica las curvas.

plot(Resultado1[, 2], Resultado1 [, 3],
     type = "l", ylab = "P", xlab = "D", 
     main="Diagrama de Fases", col=4) 
# Se grafica un diagrama de fases de P contra H. 

points(25, 5, cex = 1.5, pch = 19)  # Se agrega un 
# punto en las densidades iniciales estipuladas en  
# Resultado 1.

arrows(x0 = c(1300, -20, 500), 
       y0 = c(125, 175, 0), 
       x1 = c(650,-20, 950), 
       y1 = c(200, 100, 2), 
       length = 0.1) # Se agregan tres flechas 
# paralelas a las trayectorias de sistema.

lines(Resultado2[, 2], Resultado2[, 3], col=2) 
# Se agrega una línea que describe la trayectoria 
# del sistema bajo la condición inicial estipulada 
# en Resultados 2.

points(500, 15, cex = 1.5) # Se agrega un punto 
# en las densidades iniciales estipuladas en 
# Resultado 2.

lines(Resultado3[, 2], Resultado3[, 3], col=3) 
# Se agrega una línea que describe la trayectoria
# del sistema bajo la condición inicial estipulada 
# en Resultados 3.

points(300, 50, cex = 1.5, pch = 2) # Se agrega un 
# punto en las densidades iniciales estipuladas en  
# Resultado 2.

abline(h = b/a, lty = 2) # Se agrega la nulclina 
# de la presa.

abline(v = s/(e * a), lty = 2) # Se agrega la 
# nulclina del depredador.

