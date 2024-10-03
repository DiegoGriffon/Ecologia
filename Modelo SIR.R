
# 	Modelo SIR de Kermack y McCormick

library (deSolve) # se carga la librería deSolve

# Modelo SIR:
SIR <- function(Tiempo, CondIni, Parametros) {
  {
    S <- CondIni[1]
    I <- CondIni[2]
    R <- CondIni[3]
  }
  with(as.list(Parametros), { 
    
    dS.dt <- -beta * S * I
    dI.dt = beta * S * I - gamma * I
    dR.dt = gamma * I
    return(list(c(dS.dt, dI.dt, dR.dt)))
  })
} # Se crea una función que ejecuta el modelo SIR

# Condiciones iniciales
# (tamaños poblacionales):
S = 100; I = 1; R = 0
# Se la asignan valores a las densidades

# Valores de los parámetros:
Parametros <- c(beta = 0.002, gamma = 0.03) 
# Se la asignan valores a los parámetros

# Tiempo:
Tiempo <- seq(0, 250, by = 0.1)
# Se define el tiempo durante el cual 
# se va a evaluar el modelo


# Evaluar el modelo:
SIR.resultado <- data.frame(ode(c(S, I, R), 
                                Tiempo, SIR, 
                                Parametros)) 
# Se integra numéricamente el modelo 

# Gráfica de los resultados:
matplot(Tiempo, SIR.resultado [, -1], 
        type = "l", 
        lty = 1:3,
        col = 1:3,
        ylab = "Población") 
# Se grafican los resultados

legend("right", c("R", "I", "S"), 
       lty = 3:1, col = 3:1, bty = "n") 
# Se crea una leyenda

# Kermack, W. O. y McCormick, W. G. 1927. 
# A contribution to the mathematical theory 
# of epidemics. Proceedings of the Royal 
# Society, Series A, 115:700–721.
