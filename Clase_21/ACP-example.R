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


# ANÁLISIS MULTIVARIADO CALIDAD DE AGUA HUMEDAL YALI
# Rivera et al. 2019
# https://doi.org/10.1016/j.marpolbul.2019.06.054


# a) Importa datos
Yali <-read_excel("Yali_PCA.xlsx",sheet=1)
Yali$Station <- as.factor(Yali$Station )
head(Yali[,4:10]) %>% pander(caption ="Organic and inorganic parameters measured in the water.")
str(Yali)

Yali <- Yali %>% filter(Season == "Summer")
Yali_mat <- as.matrix(Yali[,-c(1:4)])
row.names(Yali_mat) <- Yali$Station
dim(Yali_mat)
str(Yali_mat)

class(Yali_mat) <- "numeric"

# b) Correlacion entre variables

pairs.panels(Yali_mat[,1:7], method = "pearson")
pairs.panels(Yali_mat[,8:14], method = "pearson")

# realiza PCA
Yalipca <- prcomp(Yali_mat, scale = TRUE)
Yalipca

# Varianza explicada
get_eigenvalue(Yalipca)
fviz_eig(Yalipca)

# Grafica por sitio
fviz_pca_ind(Yalipca,
             repel = TRUE,
             habillage = Yali$Location,
             addEllipses = TRUE,
             pointsize = 3)

fviz_pca_var(Yalipca)

fviz_pca_biplot(Yalipca,
                repel = TRUE,
                habillage = Yali$Location,
                addEllipses = TRUE,
                pointsize = 3)


# ESTUDIO DE CASO: TORTUGAS Y PLÁSTICO
# Robson et al. 2020
# https://doi.org/10.1016/j.envpol.2020.114918
# Comparación acumulación de plástico intestinal en tortugas marinas brasil.

# a) Importa datos
Tortugas <-read_excel("Dataset_permanova.xlsx",sheet=2)
Tortugas$Area<-as.factor(Tortugas$Area)
Tortugas$Plastic<-as.factor(Tortugas$Plastic)
head(Tortugas[,1:7])%>% pander(caption ="Ingesta de plástico y dieta.")


# b) Crea nuevas variables estandarizadas
val_estandarizado <- Tortugas %>%
  select(Chlorophyta, Rhdophyta, Phaeophyceae, Land, Animal) %>%
  mutate(
    Chlorophyta1 = (Chlorophyta - mean(Chlorophyta)) / sd(Chlorophyta),
    Rhdophyta1 = (Rhdophyta - mean(Rhdophyta)) / sd(Rhdophyta),
    Phaeophyceae1 = (Phaeophyceae - mean(Phaeophyceae)) / sd(Phaeophyceae),
    Land1 = (Land - mean(Land)) / sd(Land),
    Animal1 = (Animal - mean(Animal)) / sd(Animal))

# c) Calcula matriz de distancia
dist_euclidea <- dist(val_estandarizado[6:10])

# d) Realiza PERMANOVA
permanova <- adonis2(dist_euclidea ~ Area*Plastic , method = "bray", data=Tortugas, permutations=999)

permanova %>% pander()

