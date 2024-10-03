
#        Modelo de Lotka - Volterra Generalizado  

library("network")

# Definiciones básicas (número de especies 
# y tiempo de evaluado)

tamaño = 5 # Número de especies

t = 10 # Número de iteraciones

#------------------------------------------------------------------------------------------------------

#Matrices de parámetros

Ks <- diag(round (1/ runif(tamaño, min = 40, max = 150),2)) 
# Matriz con las K (como 1/k)

Rs<- diag(round(runif(tamaño, min = 0.5, max = 0.7),2)) 
# Matriz con los R

#------------------------------------------------------------------------------------------------------

#Matriz de densidades

Ns<- matrix(NA, nrow = tamaño, ncol = t + 1) 
# Matriz de densidades

Ns[,1]<- round(runif(tamaño, min = 1, max = 10)) 
# Le asigna las densidades iniciales a la matriz 

#------------------------------------------------------------------------------------------------------

#Topología de interacciones

As <- matrix(round(runif(tamaño*2, min = -1, max = 1),0), 
             nrow =tamaño, ncol = tamaño) 
# Crea la matriz de interacción (comunitaria), 
# asigna de forma aleatoria (a partir de una 
# distribución uniforme) los valores {-1, 0, 1} 
# de las interacciones.  Se generan (2 x tamaño) 
# valores.

diag(As[,]) <- c(-1)
# Asigna -1 en la diagonal

#------------------------------------------------------------------------------------------------------

#Dinámica iterativa

for (tiempo in 1:t) { 
  Dummy <-Ns[,tiempo]+Rs%*%Ns[,tiempo]+Rs%*%Ks%*% diag(Ns[,tiempo])%*%As[,] %*% Ns[,tiempo];
  for (i in 1:tamaño) {
    ifelse(Dummy[i]<0, Ns[i,tiempo+1]<-0, Ns[i,tiempo+1]<-Dummy[i])};
} # Este ciclo for ejecuta de forma iterativa las 
# operaciones entre las matrices y vectores propias 
# del modelo L-V generalizado.

#------------------------------------------------------------------------------------------------------

#Comandos para graficar densidades y redes

#Densidades

matplot(0:t+1, t(Ns), 
        xlab = "Tiempo", 
        ylab = "Densidades", 
        pch = 1: tamaño, 
        type = "l", 
        col = 1:tamaño, 
        main="Densidades") 

#Leyenda

Poblaciones <- rep("Sp.", times=tamaño)

Numeros<-c(1:tamaño)

legend("topleft", paste(Poblaciones, Numeros), 
       lty = 1: tamaño, 
       col = 1:tamaño,title = "Poblaciones")

#Red

Red<-network(As)

plot(Red, main="Red Original")


# Para ver una expansión de este modelo a 
# una escala meta-comunitaria: 
# https://github.com/TheoreticalEcologyUCV/EcologicalNetworksDynamics


# May, R.M. 1971. Stability in multi-species community models.
# Mathematical Biosciences, 12: 59-79.
# May. R.M. 1972. Will a large complex system be stable? 
# Nature, 238: 413-414.
# May, R.M. 1973. Stability and Complexity in Model Ecosystems.
# Princeton University Press. 
