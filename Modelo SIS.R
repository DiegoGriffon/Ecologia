
#          Modelo SIS


library(deSolve)

# Modelo SIS:
ModeloSIS <- function(Tiempo, CondicionesIniciales, Parametros) {
  with(as.list(c(CondicionesIniciales, Parametros)), {
    dS <- -beta * S * I + gamma * I
    dI <- beta * S * I - gamma * I
    return(list(c(dS, dI)))
  })
}

# Valores de los parámetros:
Parametros <- c(beta = 0.3, gamma = 0.1)

# Condiciones iniciales:
Condiciones_Iniciales <- c(S = 0.99, I = 0.01)

# Tiempo:
Tiempo <- seq(0, 100, by = 1)

# Evaluar el modelo:
Resultado <- ode(y = Condiciones_Iniciales, 
                 times = Tiempo, 
                 func = ModeloSIS, 
                 parms = Parametros)

# Se convierten los resultados a porcentajes:
Resultado[, "S"] <-Resultado[, "S"]*100
Resultado[, "I"] <-Resultado[, "I"]*100

# Graficar los resultados:
plot(Resultado[, "time"], Resultado[, "S"], 
     col = "green",
     main = "Modelo SIS",
     xlab = "Tiempo", 
     type = "l",
     ylim = c(0, 100),
     ylab = "Proporción de la Población")
lines(Resultado[, "time"], Resultado[, "I"], col = "red", lwd = 2)
legend("topright", legend = c("Susceptibles", "Infectados"), col = c("green", "red"), lwd = 2)

# Valor del equilibrio endémico de 
# infectados el final de la simulación:
Resultado[100, "I"]

# Valor teórico:
(1 -  (Parametros[2]/Parametros[1]))*100
# este valor es igual a: (1-gamma/beta)100



