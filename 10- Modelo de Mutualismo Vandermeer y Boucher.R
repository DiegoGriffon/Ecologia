
# Modelo de Mutualismo Vandermeer y Boucher (1978)  

VandermeerBoucher <- function(N, Alfas, Ks, rs) { 
  N1prox <- N[1]+rs[1]*N[1]*(1-Alfas[1,1]*N[1]/Ks[1] + Alfas[1,2]*N[2]/Ks[1])
  N2prox <- N[2]+rs[2]*N[2]*(1-Alfas[2,2]*N[2]/Ks[2] + Alfas [2,1]*N[1]/Ks[2]) 
  c(N1prox, N2prox)} # Esta función implementa el
# modelo de mutualismo  Vandermeer – Boucher en tiempo discreto. 

Ks<- c(120, 100); rs <-c(1, 1) # Se asignan valores a todos los 
# parámetros del modelo, salvo a los coeficientes de interacción.

t <- 20 # Número de pasos a iterar el modelo.

N <- matrix(NA, nrow = t + 1, ncol = 2) # Esta instrucción 
# crea una matriz en la cual se van a guardar los resultados
# de la iteración del modelo. 

N[1, ] <- c(30,30) # Se asignan densidades iniciales.

layout(matrix(1:2, ncol = 2)) # Esta instrucción crea 
# dos espacios para graficar en la ventana de 
# graficación.

casos <- 2 # Variable Dummy que permite diferenciar 
# dos casos particulares de valores de los coeficientes 
# de interacción inter-específicos.

#           Programa que ejecuta el modelo

for (J in 1:casos) {  
  # Este ciclo for permite evaluar el modelo bajo 
  # dos condiciones diferentes (concernientes
  # a los coeficientes de interacción
  #  inter-específica). 
  if (J==1) { A12<-0.1; A21<-0.1}  else
    if (J==2) {A12<-1; A21<-1} # Este conjunto de condicionales 
  # asigna valores a los coeficientes de interacción 
  # interespecíficos. Los valores asignados corresponden a dos 
  # resultados importantes: si A12.A21 < 1 existe un equilibrio 
  # interno y este es estable, si A12.A21 > 1 las poblaciones 
  # crecen sin límite.
  
  Alfas <- matrix(c(1, A12, A21, 1), ncol = 2, byrow = TRUE) 
  # Esta instrucción crea una matriz que tiene como entradas 
  # los valores de los coeficientes de interacción. Tomar en 
  # cuenta que el comando byrow asegura que la matriz se 
  # construya fila a fila. 
  
  for (i in 1:t) {  
    N[i + 1, ] <- VandermeerBoucher (N[i, ], Alfas, Ks, rs) 
    # Ciclo for que itera la función  VandermeerBoucher.
  } # fin del segundo ciclo for.
  
  matplot(0:t, N, type = "l", col = 1,
          xlab = "Tiempo", ylab = "Densidades") 
  # Se grafican los resultados.
  
  abline(h = Ks[1], lty = 1, col = 2) 
  # Se agrega una línea en el valor de K1.
  
  text(6, Ks[1], "K1", adj = c(0, 0), col = 2) 
  # Se agrega “K1” a la línea creada anteriormente.
  
  abline(h = Ks[2], lty = 2, col = 2) 
  # Se agrega una línea en el valor de K2.
  
  text(8, Ks[2], "K2", adj = c(0, 0), col = 2) 
  # Se agrega “K2” a la línea creada anteriormente.
  
  legend("right", c(expression("Sp.1"), 
                    expression("Sp.2")), 
         lty = 1:2, bty = "n") 
  # Se agrega una leyenda en la cual se identifican 
  # las curvas de cada especie.
  
} # fin del primer ciclo for.

# Vandermeer, J.H. Boucher, D.H. 1978. Varieties 
# of Mutualistic Interaction in Population
# Models. J. theor. Biol. 549-558.


