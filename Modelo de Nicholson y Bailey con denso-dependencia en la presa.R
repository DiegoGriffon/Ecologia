
#     Modelo de hospedador y parasitoide  
#          de Nicholson y Bailey con 
#         denso-dependencia en la presa

# De acuerdo a la modificación propuesta 
# por Beddington, Free y Lawton (1975) 

# Parámetros:
R <- 1.1; a <- 0.001; c<-3; K<-1000 
# Se asignan valores a los parámetros 
# del modelo. Probar también los 
# siguientes valores: 
# R <- 1.1; a <- 0.001; c<-3; K<-750

# Tiempo:
tiempo <- 100 # Número de iteraciones a realizar.

# Densidades iniciales:
P0 <- 100 # Densidad inicial de los parasitoides. 
H0 <- 100 # Densidad inicial de los hospedadores. 

# Matriz de resultados:
HPs <- matrix(NA, nrow = tiempo, ncol = 2) 
# Matriz en la cual se guardan los resultados.

HPs[1, ] <- c(H0, P0) # Densidades iniciales

# Modelo:
for (t in 1:(tiempo - 1)) HPs[t + 1, ] <- {
  
  H <- R * HPs[t, 1] * exp(1- (HPs[t, 1]/K) -a * HPs[t, 2])
  P <- c*HPs[t, 1] * (1 - exp(-a * HPs[t, 2]))
  
  c(H, P)
  
} # Este ciclo For implementa el modelo de hospedador
#  y parasitoide de Nicholson-Bailey con denso 
# dependencia en el hospedador. 


# Gráfica de la serie de tiempo:
matplot(1:tiempo, HPs, 
        type = "l", 
        col = 1:2, 
        ylab = "Densidades", 
        xlab = "Tiempo", 
        main="Modelo de Nicholson y Bailey con DD")
# Se grafican los resultados.

legend("topright", c(expression("Hospedador"), 
                     expression("Parasitoide")), 
       lty = 1:2, bty = "n", col=1:2) 
# Se agrega una leyenda que identifica las curvas.


# Diagrama de fases:

plot(HPs [, 1], HPs [, 2], 
     type = "l", 
     ylab = "Densidad Parasitoides", 
     xlab = "Densidad Hospedadores", 
     main="Diagrama de Fases", col=1) 
# Se grafica un diagrama de fases de 
# Parasitoides contra Hospedadores.

points(H0, P0, cex = 1.5, col = 2)


# Beddington, J.R. Free, C.A. Lawton, J.H. 1975. 
# Dynamic and complexity in predator-prey models 
# framed in difference equations. Nature, 
# 255: 58-60

