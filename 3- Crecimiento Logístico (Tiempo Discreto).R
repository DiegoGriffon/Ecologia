#        Simulación  de Crecimiento Logístico en Tiempo Discreto

# Para comenzar, se declaran las condiciones iniciales (N0), 
# el valor de los parámetros (K y R) y el número de 
# generaciones a evaluar:

N0 <- 2
alfa <- 0.01 # Este valor es igual a 1/K  (alfa =1/K). 
# Es decir, que en este caso K=100
R <- 1
t <-15

# Para evaluar el crecimiento logístico se puede construir 
# una función que ejecute las operaciones propias de este 
# modelo:

LogisticaDiscreta <- function(alfa, R , N0, t ) {
  N <- c(N0, numeric(t))
  for (i in 1:t) N[i + 1] <- { N[i] + R * N[i] * (1 - alfa * N[i]) }
  return(N) } 

# Para utilizar esta función basta con ejecutar la siguiente 
# instrucción:

Nts <- LogisticaDiscreta (alfa, R , N0, t)  # Se crea un 
# vector (Nts) y se introducen en él los valores de los 
# resultados.

plot(0:15, Nts)

# Los incrementos en este caso se pueden calcular así:
IncrementosL <- Nts[2:16] - Nts[1:15] # Este vector guarda 
# las diferencias entre las densidades.

plot(Nts[1:t], IncrementosL) # Gráfica los incrementos en 
# función de las densidades.

# Para determinar los incrementos per capita se puede 
# utilizar la siguiente instrucción:

IncrementosPerCapitaL <- IncrementosL /Nts[1:t] # Este vector 
# guarda los incrementos per capita

plot(Nts[1:t], IncrementosPerCapitaL) # Gráfica los incrementos 
# per capita en función de las densidades.

# Para evaluar el comportamiento del modelo bajo diferentes 
# valores de N0 se puede seguir la siguiente secuencia de 
# pasos:

set.seed(73)

N0s <- c(round (runif(15, min = 1, max = 1/alfa*1.5), 0))  
# Construye un vector con valores aleatorios entre 1 y 1,5K,  
# estos serán los diferentes valores de N0. El comando runif 
# permite escoger números aleatorios tomados de una 
# distribución uniforme. A este comando hay que especificarle 
# cuantos números se desean (15 en este caso) y los valores 
# máximos y mínimos entre los cuales tomar estos valores. 
# El comando round es utilizado para redondear los valores 
# obtenidos, es este caso se le pide que los lleve a números
# enteros (el  0 al final de la instrucción indica que se 
# quieren cero decimales).

N <- sapply(N0s, function(n) LogisticaDiscreta (N0 = n, alfa = 0.01,
                                                R = 0.5, t = 15)) 
# Se aplica la función logística partiendo de cada uno de los
# valores de N0s. 

matplot(0:t, N, type = "l", lty = 1, lwd = 0.75, col = 1)

# Para evaluar el efecto de diferentes  valores de alfa 
# se puede seguir el siguiente procedimiento: 

set.seed(73)

a.s <- 1/runif(15, min = 50, max = 1000) # Se construye un 
# vector con 15 entradas. Estas representarán los diferentes 
# valores de alfa a evaluar. Estos valores son tomados al 
# azar de una distribución uniforme (utilizando el comando 
# runif). Se especifica que los valores de K estén acotados 
# entre 50 y 1000.

N <- sapply(a.s, function(a) LogisticaDiscreta (alfa = a, R = 1, N0 <- 2, t = 15)) 
# Aplica la función de crecimiento logística con los 
# valores de a.s.

matplot(0:t, N, type = "l", ylim = c(0, 1000), lty = 1, lwd = 0.75, col = 1)

# Para evaluar el efecto de diferentes valores de R 
# se siguen los siguientes pasos:

R.s <- seq(1.3, 2.8, by = 0.3) #crea valores de R 
# entre 1,3 y 2,8  (comando seq) separados por 
# intervalos de 0,3 (orden: by).

Ns <- sapply(R.s, function(r) LogisticaDiscreta (R = r, N0 <- 2, alfa = 0.01,  t = 15)) 

matplot(0:t, Ns, type = "l", col = 1)

# Para evaluar sensibilidad a las condiciones iniciales
# se puede ejecutar las siguientes instrucciones:

N.iniciales <- c(97, 98); t <- 30

Ns <- sapply(N.iniciales, function(n0) LogisticaDiscreta (N0 = n0, R = 2.7, alfa = 0.01,  t = t))

matplot(0:t, Ns, type = "l", col = 1:2)
