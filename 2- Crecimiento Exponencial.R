
#       Crecimiento Exponencial

r <- c(-0.03, -0.02, 0, 0.02, 0.03) # Se definen en un 
# vector (r) diferentes valores de la tasa de crecimiento 
# per capita continua.

N0 <- 2; Tiempo <- 1:100 # Se definen el valor de la densidad 
# al inicio y el intervalo a evaluar.

resul <- sapply(r, function(ri) N0 * exp(ri * Tiempo)) # Esta 
# instrucción aplica una función que implementa las 
# operaciones del modelo de crecimiento exponencial a 
# todos los valores del vector r.

layout(matrix(1:2, nrow = 1)) # Esta instrucción genera 
# una ventana de gráficos y la divide en dos.

matplot(Tiempo, resul, type = "l", ylab = "N", col = 1) 

legend("topleft", paste(rev(r)), lty = 5:1, col = 1, 
       bty = "n",title = "r") 
# Esta instrucción  agrega una leyenda al gráfico

matplot(Tiempo, resul, type = "l", ylab = "log(N)",
        log = "y", col = 1)

