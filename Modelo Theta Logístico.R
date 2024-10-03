
#	Modelo Theta Logístico

# Función que implemente 
# el modelo Theta Logístico: 

ThetaLogistica  <- function(tiempo, y, parms) { 
  n <- y[1]; with(as.list(parms), {
    dN.dt <- r * n * (1 - (alfa * n)^theta); 
    return(list(c(dN.dt))) }) 
  } 
# Esta función implementa el modelo theta logístico.

# Para evaluar el comportamiento del modelo, se puede utilizar 
# la siguiente secuencia de instrucciones:

library(deSolve) # Se carga la librería “deSolve” para poder 
# utilizar el comando “ode” para evaluar numéricamente 
# ecuaciones diferenciales ordinarias.

?deSolve
?ode

parametros <- c(r <- 0.75, alfa <- 0.01, theta = 0) # Se asignan
# valores a los parámetros y se los guarda en un vector.

theta <- c(0.5, 1, 2) # se asignan valoresvalores al 
# parámetro theta. Los diferentes valores empleados establecen  
# relaciones lineales (1), cóncavas (0,5) y convexas (2).

tiempos <- seq(0.1,40, by = 0.1)

thetaN <- sapply(theta, function(th) { 
  parametros ["theta"] <- th; ode(y = 1, tiempos, 
                                  ThetaLogistica,
                                  parametros)[, 2]}) 
# Esta instrucción evalúa numéricamente la función theta logística  
# aplicando el comando “ode” para cada uno de los valores de theta.
# El comando "ode" produce una matriz de dos columnas, en la primera
# guarda el valor de la variable independiente (típicamente el tiempo),
# en la segunda el valor obtenido de la aplicación de la función evaluada.
# Por esto, al final del código aparece [, 2], esto guarda los valores 
# de la segunda columna (el resultado). 

matplot(tiempos, thetaN, type = "l")

legend("bottomright", legend = paste("theta =", c(2, 1, 0.5)), 
       lty = 3:1, bty = "n")


# Efecto del valor de Theta en la tasas de 
# crecimiento per capita:

r <- 0.75; alfa <- 0.01 # En esta línea 
# de comandos se asignan valores a los parámetros r y alfa 
#(definida como: alfa =1/K).

N <- 0:110 # Se defienden diferentes valores de densidad.

# Para aplicar la función basta ejecutar la siguiente instrucción:

theta.resul <- sapply(theta, function(th) { 1 - (alfa * N)^th }) 
# Esta instrucción aplica una función (que representa el componente 
#denso dependiente del modelo) para diferentes valores de theta  
# y de N. 

matplot(N, theta.resul, type = "l", col = 1)

abline(h = 0) # agrega una línea horizontal en cero

legend("topright", legend = paste("theta =", c(2, 1, 0.5)),
       lty = 3:1) # agrega una leyenda que identifica 
# los diferentes valores de Theta.

# M.E. Gilpin, F.J. Ayala. 1973. Global models of growth 
# and competition, Proc. Natl. Acad. Sci. USA 70, 3590-3593.

