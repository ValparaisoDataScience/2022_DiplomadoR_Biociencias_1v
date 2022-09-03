# ----------------------------------------------------------
# Clase 03 - Importar y simular variables Aleatorias Continua
# Dr. José Gallardo y Dra. María Angélica Rueda Calderón
# 03 septiembre 2022
# Diplomado R para Biociencias.
# ----------------------------------------------------------

# Remover objetos de la sesión de trabajo
rm(list = ls())

# ¿Cómo instalar paquetes?
install.packages("xlsx")

# Luego de instalar inhabilite la función 
# install.packages("xlsx")

# Instale paquetes readr y readxl, luego inhabilite


# ¿Cómo habilitar paquetes?
library(xlsx)

# Habilite paquetes readr y readxl


# ¿Cómo importar datos datos a R? (Formatos .txt, .csv, .xlsx)

# Importa base de datos en formato .txt

datos_txt <- read.delim("/cloud/project/Clase_03/datos.txt")

# Importa base de datos en formato .csv

datos_csv <- read_csv("/cloud/project/Clase_03/datos.csv")

# Importa base de datos en formato .xlsx

datos_xlsx <- read_excel("/cloud/project/Clase_03/datos.xlsx")

# ¿Qué tipos de distribuciones hay en paquete stats?

help(Distributions)
help(Normal)

help(seq)
help(sample)

# Simular base de datos con variable aleatoria continua con distribución normal paso a paso
set.seed(1) #semilla para fijar resultados cada vez que se corre la simulación
Animal <- seq(1:100)
Talla <- rnorm(100, 77, 5)
Peso <- rnorm(100, 6078, 1190)
Sexo <- sample(c("Hembra","Macho"), size = 100, replace = TRUE)
datos <- data.frame(Animal, Talla, Peso, Sexo)

# Explore y grafique el objeto "datos" con summary(), hist(), boxplot()
# plot()

# Simula una variable cuantitatica continua de su interes personal.
# Use set.set() y rnomr(). 
# Guarde su variables como un objeto.
# Simule un tratamiento asociado a su variable de interes.
# Use sample()
# Guarde su tratamiento como un objeto.
# Elabore un data.frame y explore con summary() y otras funciones aprendidas en clase.




























# Respuesta a ejercicios
summary(datos)
hist(datos$Peso)
boxplot(datos$Peso ~ datos$Sexo)
plot(density(datos$Talla))
plot(ecdf(datos$Peso))
