
#	Modelo de Competencia de Lotka-Volterra (continuo) A.

LotkaVolterraComp2 <- function(t, n, parametros) { 
  with(as.list(parametros), {
    dn1dt <- r1 * n[1] * (1 - Inter11 * n[1] - Inter12 * n[2])
    dn2dt <- r2 * n[2] * (1 - Inter22 * n[2] - Inter21 * n[1])
    list(c(dn1dt, dn2dt))})} # Esta función implementa 
# el modelo de competencia  Lotka-Volterra en tiempo continuo.

library(deSolve) # Abre la librería deSolve que permite 
# realizar operaciones con ecuaciones diferenciales ordinarias.

parametros <- c(r1 = 1, r2 = 0.1, Inter11 = 0.2, 
                Inter21 = 0.1, Inter22 = 0.02, 
                Inter12 = 0.01) # Asigna valores a los 
# parámetros del modelo.El parámetro llamado Inter es 
# igual a αij/K.

N0 <- c(2, 1) # Asigna valores a las densidades iniciales.

Resultado <- ode(y = N0, 
                 times = 1:1000, 
                 func = LotkaVolterraComp2, 
                 parms = parametros) 
# Crea un objeto en el cual se guardan los resultados, 
# para obtener estos se utiliza el comando “ode”.

matplot(Resultado[, 1], Resultado[, -1], 
        type = "l", 
        col = 1, 
        ylab = "Densidades", 
        xlab = "Tiempo") 
# Grafica los resultados.

legend("right", c(expression("Sp.1 " * (Inter[12] == 0.1)), 
                  expression("Sp.2 " * (Inter[21] == 0.01))), 
       lty = 1:2,
       bty = "n") # Se agrega una leyenda en la 
# cual se identifican las curvas de cada especie y se 
# muestra los valores del parámetro Inter para
# las interacciones inter-específicas. 


