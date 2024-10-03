
#    Modelo de metapoblacion de Levins

# Librerías necesarias:
library(deSolve)
?deSolve

library(ggplot2)

# Función que simula el modelo de Levins
Levins <- function(t, y, parms) {
  p <- y[1]
  with(as.list(parms), {
    dp <- ci * p * (1-p) - e * p
    return(list(dp))}) 
}

# Parámetros del modelo:
prms <- c(ci=0.15, e=0.05)

# Frecuencia inicial de parches ocupados:
p_inicial <- 0.01

# Se corre el modelo

?ode
# El comando “ode” es una función fundamental en 
# el paquete deSolve de R. Se utiliza para resolver 
# numéricamente sistemas de ecuaciones diferenciales 
# ordinarias (EDOs). En otras palabras, “ode” permite 
# encontrar la solución a un conjunto de ecuaciones 
# que describen cómo cambian una o más variables con 
# respecto al tiempo (o cualquier otra variable 
# independiente).  

Resultado <- data.frame(ode(y=p_inicial, 
                            times=1:300, 
                            func=Levins, 
                            parms=prms))

# Se grafican los resultados:
ggplot(Resultado) +
  geom_line(aes(x = time, y = X1)) +
  ylim(0, 1) +
  labs(x = "Tiempo", y = "Frecuencia de parches ocupados (p)") +
  annotate("text", x = 70, y = 0.75, 
           label = bquote(over(dp,dt)==cp(1-p)-ep))

# Valor de p al final de la simulación:

Resultado$X1[300]

# Resultado analitico:

1-(prms[2]/prms[1]) # Esto es equivalente a: 1 - e/ci


#







# Tomado de https://hankstevens.github.io/Primer-of-Ecology/meta.html

