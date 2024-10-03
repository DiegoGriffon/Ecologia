
#     Modelo de hospedador y parasitoide  
#          de Nicholson y Bailey

# Parámetros:
R <- 1.1; a <- 0.001; c<-3 # Se asignan valores 
# a los parámetros del modelo.

# Tiempo:
tiempo <- 51 # Número de iteraciones a realizar

# Densidades de equilibrio:
P_equi <- log(R)/a # Densidad de equilibrio de los 
# parasitoides. 

H_equi <- P_equi * R/(R - 1) # Densidad de equilibrio 
# de los hospedadores. 

# Matriz de resultados:
HPs <- matrix(NA, nrow = tiempo, ncol = 2)

HPs[1, ] <- c(H_equi + 1, P_equi) 
# Densidades iniciales, la densidad de los 
# hospedadores se encuentra una unidad por 
# encima del equilibrio.

# Modelo:
for (t in 1:(tiempo - 1)) HPs[t + 1, ] <- {
  
  H <- R * HPs[t, 1] * exp(-a * HPs[t, 2])
  P <- c*HPs[t, 1] * (1 - exp(-a * HPs[t, 2]))
  
  c(H, P)
  
} # Este ciclo For implementa modelo de hospedador 
# hospedador y parasitoide de Nicholson-Bailey. 

# Gráfica de la serie de tiempo:

matplot(1:tiempo, HPs, 
        type = "l", 
        col = 1:2, 
        ylab = "Densidades", 
        xlab = "Tiempo", 
        main="Modelo de Nicholson y Bailey")
# Se grafican los resultados.

legend("topleft", 
       c(expression("Hospedador"), 
         expression("Parasitoide")), 
       lty = 1:2, 
       bty = "n",
       col=1:2) 
# Se agrega una leyenda que identifica 
# las curvas.

# Diagrama de fases:

plot(HPs [, 1], HPs [, 2], 
     type = "l", 
     ylab = "Densidad Parasitoides",
     xlab = "Densidad Hospedadores", 
     main="Diagrama de Fases", col=1) 
# Se grafica un diagrama de fases de 
# Parasitoides contra Hospedadores.

points(H_equi, P_equi, cex = 1.5, col = 2) 
# Se agrega un punto en las densidades 
# iniciales.


# Nicholson A. J. & Bailey V. A. 1935. The 
# balance of animal populations. Proceedings 
# of the Zoological Society of London, 3, 551-98.


