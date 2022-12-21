# ----------------------------------------------------------
# Clase 20 - Script análisis multivariado
# Dr. José Gallardo Matus
# 15 de noviembre 2022
# Diplomado en Análisis de Datos con R e Investigación reproducible para Biociencias
# ----------------------------------------------------------

# Remover objetos de la sesión de trabajo
rm(list = ls())

# Paquetes
library(readxl)
library(ggplot2)
library(dplyr)
library(knitr)
library(psych) # Graficas de correlación
library(factoextra) # distancia euclideana
library(vegan) # 	Community Ecology Package: Ordination, Diversity and Dissimilarities

### ESTUDIO DE CASO 1.
# Matriz de distancia y análisis de cluster para datos discretos de diversidad ambiental.
# Importar y explorar datos de biodiversidad ambiental.
# MULTIVARIATE ANALYSIS OF ECOLOGICAL DATA de los autores Michael Greenacre y Raul Primicerio
# https://www.fbbva.es/microsite/multivariate-statistics/

# Importa la hoja 1 set de datos bioenv.xlsx

bioenv <- read_excel("bioenv.xlsx", sheet = 1)
summary(bioenv)
bioenv$Sitio <- as.factor(bioenv$Sitio)
bioenv$Sediment <- as.factor(bioenv$Sediment)
str(bioenv)

# Elabora matriz de correlaciones no paramétrica de spearman **cor()**
# usando la información de abundancia de especies (a,b,c,d y e).

t_bioenv <- t(bioenv[2:6])
colnames(t_bioenv) <- c("s1",	"s2",	"s3",	"s4",	"s5",	"s6",	"s7",	"s8",	"s9",	"s10",	"s11",	"s12",	"s13",	"s14",	"s15",	"s16",	"s17",	"s18",	"s19",	"s20",	"s21",	"s22",	"s23",	"s24",	"s25",	"s26",	"s27",	"s28",	"s29",	"s30")
rownames(t_bioenv) <- c("a","b","c","d","e")
res <- cor(t_bioenv, method = "spearman")
round(res[c(1:7),c(1:7)], 2)

# Calcula Indice de disimilaridad de Bray-Curtis
# usando la función **vegdist()** de la librería **vegan**. 
# https://www.statology.org/bray-curtis-dissimilarity/

Bray_curtis <- vegdist(bioenv[2:6], method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE)
Bray_curtis
class(Bray_curtis)
as.matrix(Bray_curtis)[1:6,1:6]

# Cluster jerárquico para datos de biodiversidad ambiental.
# Cluster jerarquico usando la función **hclust()**.
# Consida el método UPGMA usando el argumento *method="average"*.
# Grafica dendograma con la función **plot()**. 
# TIPS: Use el argumento *hang=-1* para mejorar el diseño del cluster.
# TIPS 2: abline permite agregar una linea horizontal en un indice de similaridad de 0.5.
fit <- hclust(Bray_curtis, method="average")
plot(fit, hang=-1)
abline(h = 0.5, col="red")


# ESTUDIO DE CASO 2
# Matriz de distancia y análisis de cluster para datos continuos de diversidad ambiental.
# Drake y Arias (1997) investigaron el efecto de dos diferentes sistemas de cultivo de dorada *Sparus aurata* (Policultivo y Monocultivo)
# en un sistema de lagunas de la bahía de Cádiz. 
# https://doi.org/10.2307/1352243 
# Datos: Densidad media (no. individuos x 225 cm-2) de especies bénticas (Po = polychaete;  Mo = molluscan; My = mysidacean; Cu = cumacean; Am  =  amphipod; Di  =  dipteran)
# en 5 sitios de cultivo diferentes (Policultivo: A, B, C; Monocultivo: D y E).

# Importar y explorar datos de bentos.
dbentos <- read_excel("dat_densidad_bentos.xlsx", sheet = 1)
summary(dbentos)
dbentos$Especies <- as.factor(dbentos$Especies)
str(dbentos)

# Indice de disimilaridad de Bray-Curtis usando la función **vegdist()** de la librería **vegan**. 
# Recomendación del autor: calcular la matriz de distancia de Bray-Curtis sobre los datos dos veces transformados con raiz cuadrada (**sqrt()**). 

t_dbentos <- t(dbentos[2:6])
colnames(t_dbentos) <- c("e1",	"e2",	"e3",	"e4",	"e5",	"e6",	"e7",	"e8",	"e9",	"e10",	"e11",	"e12",	"e13",	"e14",	"e15",	"e16",	"e17",	"e18",	"e19",	"e20", "e21")
rownames(t_dbentos) <- c("A","B","C","D","E")
head(t_dbentos[1:5,1:6])

# Transformadas
# Primera raíz cuadrada
rt_dbentos <- sqrt(t_dbentos)
head(rt_dbentos[1:5,1:6])
# Segunda raíz cuadrada
rrt_dbentos <- sqrt(rt_dbentos)
head(rrt_dbentos[1:5,1:6])

# Matriz de distancia.
Bray_bentos <- vegdist(rrt_dbentos, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE)
Bray_bentos

# Análisis de cluster jerarquico.
# Recuerde que Policultivo: A, B, C; Monocultivo: D y E.
fit_1 <- hclust(Bray_bentos, method="average")
plot(fit_1, hang=-1)
rect.hclust(fit_1, k = 2, border = c(2,4))

# Opciones de visualización
dend <- color_branches(fit_1, k = 2)
plot(dend)

dend <- color_labels(fit_1, k = 3)
plot(dend)

dend <- dend %>%
  color_branches(k = 3)
plot(dend)

dend <- dend %>%
  color_branches(k = 3) %>%
  set("branches_lwd", c(2,2,2))
plot(dend)

dend <- fit_1 %>%
  color_branches(k = 3) %>%
  set("branches_lwd", c(2,1,2)) %>%
  set("branches_lty", c(1,2,1))

plot(dend)

