# ----------------------------------------------------------
# Clase 19 - Script pruebas no paramétricas
# Dr. José Gallardo Matus
# 12 de noviembre 2022
# Diplomado Análisis de datos con R e investigación reproducible para Biociencias.
# ----------------------------------------------------------

# Remover objetos de la sesi?n de trabajo
rm(list = ls())

# Paquetes
library(ggplot2)
library(tidyr)
library(dplyr)
library(FSA) #perform Dunn's Test with Bonferroni correction for p-values
library(rstatix) #perform Dunn's Test with Bonferroni correction for p-values permite agregar significancia a plot
library(pander) # da formato a tablas estadísticas
library(ggpubr) # estética y  estadística
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

# ESTUDIO DE CASO: # ESTUDIO DE CASO: Nº ESPERMIOS - PLOMO SANGUINEO

# [Schmitt et al. 2013](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0075900)
# Relación lineal entre número de copias del gen y su expresión.

# Crea objetos X e Y
# Plomo sangre (X) , Nº espérmios (Y)
X <- c(742,101,313,600)
Y <- c(170,180,210,160)

# Realiza test de correlación
cor.test(X,Y, method = "spearman",
         alternative = "two.sided")

# ESTUDIO DE CASO: Formación de biofilm ($\mu m^2$)
# *Staphylococcus epidermidis* en presencia de plasma humano.
# Adaptado de Skovdal et a. 2021
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8346721/

# Crea objetos tratamiento y control
data <- data.frame(Tratamiento = rep(c("Con plasma", "Sin plasma"), each = 3),
                   Biofilm = c(9, 12, 13, 0, 4, 6))


# Realiza prueba de Mann-Whitney
wilcox.test(Biofilm ~ Tratamiento, data = data, alternative = "greater",
            paired = FALSE)

# Graficas avanzadas
myplot <-  ggplot(data = data, aes(x=Tratamiento, y=Biofilm, color=Tratamiento))+
  geom_jitter(width = 0.10)+
  labs(title="Biofilm con o sin plasma.", x="Tratamiento", y="Biofilm") +
  stat_summary(fun= median, geom="crossbar", width=0.5, color="black")

myplot

# Agrega significancia al plot
myplot + stat_compare_means(method = "wilcox.test", label.y = 12.5, label.x = 1.5)

myplot + stat_compare_means(comparisons = list(c("Con plasma","Sin plasma")), label = "p.signif")


# ESTUDIO DE CASO: GONADOTROFINA TRUCHAS
# https://hal.inrae.fr/hal-02714224/document

# Crea objetos pre y post
data <- data.frame(Tratamiento = rep(c("pre", "post"), each = 4),
                   Gonadotrofina = c(45, 41, 47, 52,49, 50, 52, 50))


# Realiza prueba de Wilcoxon
wilcox.test(Gonadotrofina ~ Tratamiento, data = data,
            alternative = "greater",
            paired = TRUE) %>% pander()

# ESTUDIO DE CASO: DAÑO EN PLANTAS DE NOGAL
# Besoain 2018
# https://www.redagricola.com/cl/bioinoculante-accion-preventiva-ante-enfermedades-fungicas/).
# Fertilizante Vitanica® RZ (con *Bacillus amyloliquefaciens*) tiene acción preventiva ante enfermedades fúngicas en nogal.

# Simula datos
set.seed(1)
nogal <- data.frame(Tratamientos = rep(c("T0", "T1", "T2", "T3", "T4"), each = 9),
                   IDr = c(runif(9, 0.6,0.8),
                           runif(9, 0.1, 0.3),
                           runif(9, 0.2, 0.4),
                           runif(9, 0.35, 0.45),
                           runif(9, 0.05, 0.16)))

nogal$Tratamientos <- as.factor(nogal$Tratamientos)

myplot <-  ggplot(nogal, aes(x=Tratamientos, y=IDr, color=Tratamientos))+
  geom_jitter(width = 0.10)+
  labs(title="Indice de daño en nogal", x="Tratamiento", y="IDr (Indice de daño)") +
  stat_summary(fun= median, geom="crossbar", width=0.5, color="black")
myplot 

# Realiza prueba de kruskal
kruskal.test(IDr ~ Tratamientos, data=nogal) %>% pander()

# Realiza prueba de dunn FSA
table <- dunnTest(IDr ~ Tratamientos,
                  data=nogal,
                  method="bonferroni")

table[["res"]] %>% pander(digits=2)

# Realiza prueba de dunn FSA
dann.test <- nogal %>% dunn_test(IDr ~ Tratamientos, p.adjust.method = "hochberg")

# Agrega significancia al plot
ggboxplot(nogal, x="Tratamientos", y="IDr", fill = "Tratamientos") +
  stat_pvalue_manual(dann.test, y.position = c(0.96,0.9,0.84,0.62,0.55),
                     label = "p.adj.signif", hide.ns = TRUE)

# # ESTUDIO DE CASO: GERMINACIÓN DE SEMILLAS DE PEUMO

# Chacon et al. 1998**
# http://rchn.biologiachile.cl/pdfs/1998/2/Chacon_et_al_1998.pdf
# Germinación depende de tamaño de semilla.


# PRUEBA CHI CUADRADO
# Crea matriz de datos
datos <- c(13, 23, 26, 17, 7, 4)
dim(datos) <- c(3,2)
rownames(datos) <- c('small','medium',"large")
colnames(datos) <- c('Germinated','No germinated')

datos

# Test de Chi-squared en R (chisq.test)
test<-chisq.test(datos, correct = FALSE)

test %>% pander()