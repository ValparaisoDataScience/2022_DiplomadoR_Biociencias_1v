# ----------------------------------------------------------
# Clase 06 - Script Manipulación de datos con tidyr y dplyr
# Dr. José Gallardo Matus
# 13 de septiembre 2022
# Diplomado R para Biociencias.
# ----------------------------------------------------------

# Habilita librerías
library(readxl) # Para importar datos desde excel a R

library(tidyr) # Para manipular datos

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos


# LIBRERÍA DPLYR: EL OPERADOR PIPE (TUBERÍA).

# dplyr usa el operador pipe %>% como una tubería para enlazar un data.frame con una o más funciones.

x <- rnorm(5)
y <- rnorm(5)
dat <- data.frame(x,y)
dat
max(dat) 
dat %>% max
dat %>% arrange(y) # Ordena filas de un data.frame por el valor de alguna columna

# LIBRERÍA TIDYR: ORDENAR DATOS MESSY.

# Importar messy datos
messy <- read_excel("Peces.xlsx") # Carga el set de datos
summary(messy)

# Importar messy datos con datos faltantes
messy <- read_excel("Peces.xlsx", na="NA")
summary(messy)
head(messy)  # Muestra los primeros datos del data set

# Filtrar variable peso con tubería
messy %>% filter(Variable == "peso")

# Colapsar columna Especie
messy %>% filter(Variable == "peso") %>% gather("Especie","Peso",3:5)

# Crear objeto Peso
Peso <- messy %>% filter(Variable == "peso") %>% gather("Especie","Peso",3:5)

# Eliminar columna "variable" y renombrar variable réplica.
Peso <- Peso[,-1]
colnames(Peso)[1] <- "peces"
Peso$peces <- c(1:9)
head(Peso) 

# TRABAJANDO CON DATOS REALES

# https://lter.kbs.msu.edu/datatables/51

agronomic_data <- read_excel("agronomic_data.xlsx", sheet = 1)
head(agronomic_data)
agronomic_data$Sample_id <- as.factor(agronomic_data$Sample_id)
agronomic_data$Crop <- as.factor(agronomic_data$Crop)
agronomic_data$Year <- as.factor(agronomic_data$Year)
agronomic_data$yield_bu_A <- as.numeric(agronomic_data$yield_bu_A)
summary(agronomic_data)

# FUNCIÓN SELECT()
# Permite extraer o seleccionar variables/columnas específicas de un data.frame.
select(agronomic_data, Crop, Year)

# FUNCIÓN SELECT() CON PIPE
agronomic_data %>% select(Crop, Year)

# FUNCIÓN FILTER() CON PIPE
# **filter()**: Para filtrar desde una tabla de datos un subconjunto de filas.
# Ej. solo un nivel de de un factor, observaciones que cumplen algún criterio (ej. > 20).
agronomic_data  %>% filter(Crop == "Glycine max")
agronomic_data  %>% filter(Crop == "Zea mays")

# MÚLTIPLES FUNCIONES Y TUBERÍAS
agronomic_data %>% select(Crop, Year, yield_bu_A) %>% 
  filter(Crop == "Glycine max")

# FUNCIÓN SUMMARIZE()
agronomic_data %>% select(Crop, Year, yield_bu_A) %>% 
          summarize(n = n(), 
                    Promedio_yield = mean(yield_bu_A), 
                    Maximo_yield = max(yield_bu_A))

# FUNCIÓN SUMMARIZE() removiendo NA antes de calcular
agronomic_data %>% select(Crop, Year, yield_bu_A) %>% 
  summarize(n = n(), 
            Promedio_yield = mean(yield_bu_A, na.rm=T), 
            Maximo_yield = sd(yield_bu_A, na.rm=T))

# FUNCIÓN SUMMARIZE() + GROUP_BY()
# Permite agrupar filas con base a los niveles de alguna variable o factor.
agronomic_data %>% group_by(Crop) %>% 
  summarize(n = n(), 
            mean_yield = mean(yield_bu_A, na.rm=T), 
            sd_yield = sd(yield_bu_A, na.rm=T))

agro_tab <- agronomic_data %>% group_by(Crop) %>% 
  summarize(n = n(), 
            mean_yield = mean(yield_bu_A, na.rm=T), 
            sd_yield = sd(yield_bu_A, na.rm=T))


# FUNCIÓN MUTATE()
# Permite calcular nuevas variables "derivadas", ej. proporciones, tasas, log.
# Calcularemos el coeficiente de variación como  sd / mean * 100
agro_tab %>% mutate(CV_yield = sd_yield/mean_yield*100)


# EJERCICIO
# Usando agronomic_data calcule n, promedio y desviación estándar de yield_bu_A por Year.
# luego calcule CV usando mutate()

# JOINING DATA.FRAME
agronomic_factor <- read_excel("agronomic_data.xlsx", sheet = 2)
agronomic_factor$Treatment <- as.factor(agronomic_factor$Treatment)
agronomic_factor$Replicate <- as.factor(agronomic_factor$Replicate)
summary(agronomic_factor)

inner_data <- inner_join(agronomic_data, agronomic_factor, "Sample_id")
left_data <- left_join(agronomic_data, agronomic_factor, "Sample_id")

# EJERCICIO
# Unas las tablas agronomic_data y agronomic_factor usando right_join y full_join
