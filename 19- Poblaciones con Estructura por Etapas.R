#  	Poblaciones con Estructura por Etapas

# Se crea una matriz de transición “A”:

A <- matrix(c(0, 0.5, 20, NA, 0, 0, 0, 0.5, 0.9), 
            nr = 3, byrow = TRUE) 

A # La matriz construida corresponde con el ejemplo 
# que se está evaluando (salvo la entrada  a21).

A[2,1]<-0.3 # De esta manera se puede modificar una 
# de las entradas de la matriz (en este caso a21). 

A

N0 <- matrix(c(100, 250, 50), ncol = 1) # Se crea un 
# vector con las densidades iniciales.

# Para obtener las densidades por etapa luego de 
# una generación basta con ejecutar la siguiente 
# instrucción:

N1 <- A %*% N0 

N1

# Para simular el paso de varios años se puede 
# utilizar  la siguiente secuencia de 
# instrucciones:

años<-6

N.projec <- matrix(0, nrow = nrow(A), ncol = años + 1) 
# Se crea una matriz con las dimensiones apropiadas 
# para guardar los resultados.

N.projec

N.projec[, 1] <- N0 # Se asignan las densidades
# iniciales a la primera columna.

N.projec

for (i in 1:años) N.projec[, i + 1] <- A %*% N.projec[, i] 
# Este ciclo for simula el crecimiento de la 
# población durante 6 años.

N.projec

matplot(0:años, t(N.projec), type = "l", 
        lty = 1:3,  
        col = 1, 
        ylab = "Abundancias por etapa", 
        xlab = "año")

legend("topleft", legend = c("Semillas", 
                             "Adultos Pequeños", 
                             "Adultos Grandes"),  
       lty = 1:3, col = 1, bty = "n")

# Para realizar un análisis espectral de este 
# sistema se puede ejecutar la siguiente secuencia 
# de instrucciones:

auto.A <- eigen(A) # El comando eigen calcula 
# los autovalores y autovectores de la matriz
# de transición.

auto.A

AutoDominante <- which.max(auto.A[["values"]]) # Esta
# instrucción señala la posición del auto valor 
# dominante.

L1 <- Re(auto.A[["values"]][ AutoDominante]) # Esta  
# instrucción obtiene la parte real del auto valor 
# dominante  (utilizando  la posición que se encontró 
# en el paso anterior).

L1

# Estructura estable:

w <- Re(auto.A[["vectors"]][, AutoDominante]) # Mediante 
# esta instrucción se obtiene el  autovector asociado 
# al autovalor dominante.

DEE <- w/sum(w) # Se divide el autovector asociado al 
# autovalor dominante entre la suma de todas las 
# densidades, esta es la DEE.

round(DEE, 3)*100 # Esta instrucción redondea a 3 
# decimales y transforma los resultados en porcentajes.

#Valor reproductivo:

M <- eigen(t(A)) # Esta instrucción calcula los 
# autovalores de la matriz transpuesta de A

v <- Re(M$vectors[, which.max(Re(M$values))]) # Calcula 
# el autovector asociado al autovalor dominante de la 
# matriz A transpuesta

VR <- v/v[1] # Se expresan los valores  
# (valores reproductivos) en función de la clase 1 

VR

# Análisis de sensibilidad y elasticidad:

vw.s <- v %*% t(w) #  Producto de los vectores 
# de la estructura estable y de los valores 
# reproductivos

(S <- vw.s/as.numeric(v %*% w)) #  Calcula 
# las sensibilidades

Elas <- (A/L1) * S #  Calcula las elasticidades

round(Elas, 3) #  Se redondean
