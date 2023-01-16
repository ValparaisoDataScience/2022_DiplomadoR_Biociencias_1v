# -------------------------------------------------------------------------
# Clase 02 - Simular variables aleatorias discretas
# Dr. José Gallardo y Dra. María Angélica Rueda Calderón
# 12 enero 2022
# Curso R para Biociencias.
# -------------------------------------------------------------------------


# Habilita paquete para simular algunas variables aleatorias discretas
library(Rlab)

# Distribuciones de variables discretas
help(dbern)

# Compute P(X=1) and P(X=0) for X Bernoulli(0.7)
dbern(1, 0.7)
dbern(0, 0.7)

# Distribuciones de variables discretas
help(dbinom)


# Ejercicio binomial adaptado de https://rpubs.com/osoramirez/99241
# Se conoce que un musgo es un inhibidor de la floracion en plantas.
# Un fertilizante orgánico puede eliminar el musgo con un 75% de eficacia.
# Si aplico el fertilizandte en 10 plantaciones
# ¿Cuál es la probabilidad de que 5 de ellas pierdan la cosecha?
dbinom(5,10,0.25)*100

# ¿Cuál es la probabilidad de que 1 de ellas pierda la cosecha?
# ¿Cuál es la probabilidad de que 3 de ellas pierda la cosecha?
# ¿Cuál es la probabilidad de que 10 de ellas pierda la cosecha?

# Bernoulli
# Simular un ensayo Bernoulli probabilidad de encontrar un individuo infectado por alguna enfermedad.
set.seed(1) # semilla para fijar resultados cada vez que se corre la simulación
rbern(1, 0.65) # 1 observación, probabilidad de éxito 0.65

# Simular 100 observaciones Bernoulli (muestras independientes)
set.seed(1)
rbern(100, 0.65)

# Observar variable bernoulli
infectados <- rbern(100, 0.65)
table(infectados)

# Simular variable que se distribuye Binomial
# Simular número de parásitos por planta / animal
set.seed(1)
rbinom(100,8,0.5) # 100 observaciones, 8 ensayos, probabilidad de éxito 0.5

# Observar variable binomial
parasitos <- rbinom(100,8,0.5)
table(parasitos)
frec <- table(parasitos)
barplot(frec,col = "coral", main="Distribución de parásitos")

# Investigue y simule variables de su interes con distribución Bernoulli y binomial.
# Observe sus variables con table().
# Observe sus variables con barplot()