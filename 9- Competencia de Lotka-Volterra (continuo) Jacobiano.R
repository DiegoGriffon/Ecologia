
#	Modelo de Competencia de Lotka-Volterra (continuo) B: Jacobiano

dN1dt <- expression(r1 * N1 - r1 * Inter11 * N1^2 - r1 * Inter12 * N1 * N2)
dN2dt <- expression(r2 * N2 - r2 * Inter22 * N2^2 - r2 * Inter21* N1 * N2) 
# Se crean dos objetos tipo “expression” en los cuales 
# se definen las ecuaciones del modelo.

ddN1dN1 <- D(dN1dt, "N1")
ddN1dN2 <- D(dN1dt, "N2")
ddN2dN1 <- D(dN2dt, "N1")
ddN2dN2 <- D(dN2dt, "N2") # Con estas instrucciones 
# se obtienen las derivadas parciales del modelo, esto 
# se logra utilizando el comando "D". Por ejemplo, la 
# primera expresión obtiene para la primera ecuación (sp.1) 
# la derivada parcial con respecto a la variable N1.

J <- expression(matrix(c(eval(ddN1dN1), 
                         eval(ddN1dN2), 
                         eval(ddN2dN1), 
                         eval(ddN2dN2)), 
                       nrow = 2, 
                       byrow = TRUE)) # Con esta instrucción 
# se crea una función que contiene la matriz jacobiana del sistema.

N1Equi <- expression((Inter22 - Inter12)/( Inter22*Inter11 - Inter12*Inter21))
N2Equi <- expression((Inter11 - Inter21)/( Inter22*Inter11 - Inter12*Inter21)) 
# Estas expresiones representan (algebraicamente) las 
# densidades en el equilibrio interno. 

# Se asignan valores a los parámetros:
Inter11<- 0.01; Inter22<- 0.01; Inter12<- 0.001; Inter21<- 0.001 
r1 <- r2 <- 1 

N1 <- eval(N1Equi); N2 <- eval(N2Equi) # Se calculan los valores 
# de las densidades en el equilibrio.

N1; N2

Jacobiano <- eval(J) # Se evalúa el Jacobiano 
# para las densidades de equilibrio.

Autovalores <- eigen(Jacobiano) # Se obtienen las 
# autovalores y autovectores del Jacobiano.

Autovalores$values
