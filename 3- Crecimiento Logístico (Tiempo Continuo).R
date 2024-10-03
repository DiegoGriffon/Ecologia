
#          Crecimiento Logístico en Tiempo Continuo

K <- 100; r <- 0.3; N0 <- 2; t <- 1:50 # Se definen los valores 
# de los parámetros, las condiciones iniciales y el tiempo 
# a evaluar.

Nt <- K*N0*exp(r*t)/(K+N0*(exp(r*t)-1)) # Se implementa el 
# modelo (la solución analítica) sobre todos los valores 
# de tiempo definidos y se guardan los resultados en un 
# vector llamado Nt.

plot(t,Nt, type = "l")

# Para evaluar el modelo con varios valores de r se puede 
# usar la siguiente secuencia de instrucciones:

r<-c(1, 1.5, 2, 2.5, 3, 3.5) #diferentes valores de r (tómese 
# en cuenta que se incluyen valores altos)

resul <- sapply(r,function (ri) K*N0*exp(ri*t)/(K+N0*(exp(ri*t)-1)))

matplot(t, resul, type = "l", ylab = "N", col = 1)

# El siguiente conjunto de instrucciones permite construir una 
# gráfica dN/dt contra N, a partir de diferentes valores de N en 
# donde se muestre la dirección de las fuerzas que actúan en el 
# sistema.

tasa <- expression(r * N * (1 - alpha * N)) # Esta instrucción 
# crea un objeto tipo "expression" (un comando similar a 
# funtion) que contiene las operaciones propias del modelo de 
# crecimiento logístico. 

r <- 1; alpha <- 0.01; N <- 0:120 # Se asignan valores a los 
# parámetros y a la variable.

?eval

plot(N, eval(tasa), type = "l", 
     ylab = "Tasa de Crecimiento (dN/dt)",xlab = "N") 
# Esta instrucción gráfica el resultado de ejecutar 
# la expresión "tasa" sobre los valores asignados. Esto se logra 
# aplicando el comando "eval" sobre el objeto tipo expression que 
# se creó anteriormente (tasa).

abline(h = 0); legend("topright", "r=1", lty = 1) # Se introduce 
# una  línea para representar el crecimiento cero y una leyenda.

N <- c(0, 10, 50, 100, 115) # Se asignan algunos valores 
# particulares de N.

points(N, eval(tasa), cex = 1.5) # Este comando permite graficar
# puntos particulares, en este caso los obtenidos al aplicar 
# la expresión "tasa" sobre los valores de N.

arrows(20, 2, 80, 2, length = 0.1, lwd = 3) # Se agrega una 
# flecha para representar el campo vectorial

arrows(122, -2, 109, -2, length = 0.1, lwd = 3) # Se agrega 
# otra flecha para representar el campo vectorial

