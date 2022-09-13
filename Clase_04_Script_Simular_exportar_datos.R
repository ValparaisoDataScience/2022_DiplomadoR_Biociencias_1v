# ----------------------------------------------------------
# Clase 04 - Simular y exportar datos
# Dra. María Angélica Rueda Calderón
# 06 septiembre 2022
# Diplomado R para Biociencias.
# ----------------------------------------------------------

# Cargar paquetes
library(utils)
library(Rlab)
library(xlsx)

# Remover objetos de la sesión de trabajo
rm(list = ls())

## SIMULAR DATA FRAME

set.seed(1)
Animal <- seq(1:100)
Madurez <- rbern(100, 0.65)
Parasitos <- rbinom(100,8,0.6) 
Sexo <- sample(c("Hembra","Macho"), size = 100, replace = TRUE)
Cataratas <- sample(c("Alto","Medio","Bajo"), size = 100, replace = TRUE)

datos_all <- data.frame(Animal, Madurez, Parasitos, Sexo, Cataratas)

### TABLAS Y GRAFICAS

mi_tabla <- table(datos_all$Cataratas)

mi_tabla
barplot(mi_tabla,   # Datos
        main = "Gráfico de barras", # Título
        xlab = "Nivel de cataratas",# Etiqueta eje X
        ylab = "Frecuencia",        # Etiqueta eje Y
        border = "black",           # Color borde
        col = c("darkgrey", "darkblue", "red"))

## COMO EXPORTAR DATOS DESDE R

# Exportar objeto datos_all en formato **.txt, .csv y .xlsx**
  
write.table(x = datos_all, file = "datos_all.txt", 
            sep = "\t", row.names = FALSE, 
            col.names = TRUE)


write.csv(x = datos_all, file = "datos_all.csv",
          row.names = FALSE)


write.xlsx(datos_all, "datos_all.xlsx", 
           sheetName = "Base_datos", 
           col.names = TRUE, row.names = FALSE)
