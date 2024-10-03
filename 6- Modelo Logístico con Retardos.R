
# Modelo Logístico con Retardos de Hutchinson (1948)

library(deSolve)

?lagvalue

# Definir la función del modelo logístico con retardo
LogísticaConRetardos <- function(t, Inicial, Parametros) {
  N <- Inicial
  r <- Parametros["r"]
  K <- Parametros["K"]
  tau <- Parametros["tau"]
  
  if (t - tau <= 0) {
    N_tau <- Inicial[1]  # Usar el valor inicial, si t - tau 
    # es negativo
  } else {
    N_tau <- lagvalue(t - tau)# Si  es positivo, usar lagvalue 
    # para obtener el tamaño de la población en el 
    # tiempo (t - tau)
  }
  
  dN <- r * N * (1 - N_tau / K)
  list(c(dN))
}

# Valores de los parámetros del modelo:
Parametros <- c(r = 0.1, K = 100, tau = 15)

# Condiciones iniciales:
N0 <- 10
Tiempo <- seq(0, 2000, by = 1)

# Evaluar la ecuación diferencial con retardo

?dede

Resultado <- dede(y = N0, 
            times = Tiempo, 
            func = LogísticaConRetardos, 
            parms = Parametros)

# Graficar los resultados
plot(Resultado, main = "Modelo Logístico con Retardo", 
     xlab = "Tiempo", 
     ylab = "Densidad")

# Hutchinson, G. E. 1948. Circular causal systems 
# in ecology. Annals of the New York Academy of 
# Sciences 50: 221–246


