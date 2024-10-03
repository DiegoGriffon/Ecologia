# Simulación de Crecimiento Geométrico

# Para comenzar se introducen las condiciones iniciales 
# (N inicial), el valor del parámetro y el número de 
# generaciones a evaluar:

N0 <- 1
lambda <- 2
tiempo <- 0:10 

# El modelo de crecimiento geométrico puede ser expresado
# de la siguiente manera:

Nt <- N0 * lambda^tiempo # Esta instrucción ejecuta las 
# operaciones propias del modelo de crecimiento geométrico 
# sobre todas las entradas del vector tiempo y guarda los 
# resultados en un vector llamado Nt 

# Para revisar los valores obtenidos basta con escribir 
# el nombre de la variable:

Nt

# Para graficar los valores Nt en función del tiempo se 
# puede ejecutar la siguiente instrucción:

plot(tiempo, Nt)  # gráfica el crecimiento geométrico. 

plot(tiempo, Nt, log = "y")  # Construye el gráfico anterior
# pero transformando a logaritmo el eje de las ordenadas.

# Para calcular las tasas de crecimiento se puede utilizar 
# la siguiente instrucción:

tasas = Nt[2:10]/Nt[1:9] # calcula las tasas de crecimiento, 
# los corchetes indican cuales valores del vector Nt utilizar 
# en la operación.

# Vemos el resultado:

tasas

# Para calcular los incrementos en la densidad se puede 
# utilizar la siguiente instrucción:

IncrementosG = Nt[2:10]-Nt[1:9]

plot(Nt[1:9], IncrementosG) # gráfica los incrementos

# Para calcular los incrementos per cápita se puede utilizar 
# la siguiente instrucción:

IncrementosPerCapitaG <- IncrementosG/Nt[1:9]

plot(Nt[1:9], IncrementosPerCapitaG) 

# En ocasiones es útil crear una función que ejecute una 
# operación, por ejemplo, las operaciones propias del modelo 
# de crecimiento geométrico. Esto último se puede hacer de 
# la siguiente manera:

CrecimientoGeometrico <- function(N0, lambda, tiempo) {NtGeo <- N0*lambda^tiempo}
# Esta instrucción crea una función llamada CrecimientoGeométrico, 
# la cual utiliza como argumentos N0, lambda y tiempo y 
# realiza las operaciones propias del modelo de crecimiento 
# geométrico.

# Para utilizar la función basta con ejecutar la siguiente 
# instrucción:

lambda = 2.2

Nt.funcion <- CrecimientoGeometrico (N0, lambda, tiempo)

# Para graficar los resultados:

plot(tiempo, Nt.funcion)  

# Otra forma de hacer lo mismo, pero en este caso utilizando 
# un ciclo For es:

T <- 11 # Número de generaciones a evaluar.

Resul <- rep(NA, times = T) # Mediante el uso del comando rep 
# se genera un vector con T entradas del tipo NA (NA=no aplica). 
# En este vector se guardarán los resultados.

Resul

for (i in 0:T) {Resul[i] <- CrecimientoGeometrico (N0, lambda, i)} 
# Desarrolla un ciclo for que se ejecuta 11 veces (T) en 
# el cual se aplica la función CrecimientoGeomertrico 
# tomando como argumentos N0 y lambda además del valor 
# del contador i.

plot(tiempo, Resul)  

# Una forma de calcular el crecimiento geométrico a partir 
# de varios N0 (utilizando la función CrecimientGeometrico),
# es ejecutar los siguientes pasos:

N0s <- c(10, 20, 30) #Crea un vector con varios N0 a partir 
# de los cuales se ejecutará la función.

?sapply

Nt.s <- sapply(N0s, function(n) CrecimientoGeometrico (N0 = n, 
                                                       lambda = 2, 
                                                       tiempo=tiempo)) 
# Aplica la función CrecimientoGeométrico a partir de todas 
# las entradas de N0s. En este caso es importante definir 
# los valores de los demás argumentos de la función.

# Es importante apreciar que Nt.s es una matriz, para 
# constatar esto basta con reclamar su contenido escribiendo 
# su nombre:

Nt.s

# Para graficar en función del tiempo los valores de la 
# matriz se puede utilizar la instrucción:

?matplot

matplot(tiempo, Nt.s, pch = 1:3) # Este comando (matplot) es
# específico para graficar el contenido de matrices.

# Para calcular el crecimiento a partir de diferentes valores
# de lambda se puede utilizar la siguiente secuencia de 
# instrucciones:

N0 <- 100; tiempo <- 0:3; lambdas <- c(0.5, 1, 1.5) # Se pueden 
# definir varias variables en una línea de código siempre y 
# cuando estas se separen con puntos y comas. La última de 
# las variables definidas es un vector con tres valores 
# diferentes de lambda.

# Para calcular el crecimiento geométrico a partir de estos 
# valores de lambda basta con ejecutar la siguiente 
# instrucción:

Nt.s <- sapply(lambdas, function(L) CrecimientoGeometrico (lambda = L, N0 = 1, tiempo=tiempo))

matplot(tiempo, Nt.s, xlab = "Tiempo", ylab = "N", pch = 1:3)
