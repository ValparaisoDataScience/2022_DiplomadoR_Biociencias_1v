# ----------------------------------------------------------
# Clase 13 - Script inferencia estadística
# Dr. José Gallardo matus
# 11 octubre 2022
# Diplomado R para Biociencias
# ----------------------------------------------------------

# Paquetes
library(UsingR)
library(ggplot2)
library(dplyr)
library(knitr)

# ESTUDIO DE CASO: RELACIÓN ESTATURA PADRES - HIJOS

father.son
plot(father.son$fheight, father.son$sheight, xlab = "ESTATURA PADRES", ylab = "ESTATURA HIJOS")

#  **Pearson's product-moment correlation**
cor.test(father.son$fheight, father.son$sheight)

# ESTUDIO DE CASO: COMPARACIÓN TAMAÑO ENTRE SEXOS
animal <- data.frame(Sexo=rep(c("Male", "Female"), each=10), Peso=c(rnorm(10, 180, 10),rnorm(10, 140, 10)))
boxplot(animal$Peso ~ animal$Sexo, xlab = "Sexo", ylab = "Peso")

# **Two Sample t-test**

test <- t.test(Peso ~ Sexo, animal, alternative = c("two.sided"), var.equal=TRUE)
test

# Imprime resultado prueba estadistica en formato tabla.
pander::pander(test, caption = "prueba de t", digits=3)
