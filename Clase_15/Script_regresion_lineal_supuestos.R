# --------------------------------------------------------------
# Clase 15 - Script Regresión Lineal Simple y Supuestos
# Dr. José Gallardo Matus & Dra. Angélica Rueda Calderón
# 15 octubre 2022
# Diplomado Análisis de datos con R e investigación reproducible para Biociencias.
# ---------------------------------------------------------------------------------

# Habilitar librerías
library(readxl)
library(ggplot2)
library(car)
library(lmtest)
library(pander)
library(dplyr)
library(ggpmisc)

# Importa base de datos
# Fuente: Adaptado de Huang et al. 2015
# https://doi.org/10.1016/j.fsigen.2015.05.007

age.aspa <- read_excel("age.aspa.xlsx")
head(age.aspa)

# Formato formula para considerar en el gráfico
formula1 <- y ~ x


# Tema para agrandar tamaño de letra en los ejes del gráfico

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))

# Hace diagrama de dispersión con recta de ajuste lineal y muestra los resultados de la estimación de los betas

q <- age.aspa %>% ggplot(aes(x = ASPA, y = Age)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue", show.legend=TRUE) + scale_x_continuous(n.breaks = 8)
q+My_Theme+ggtitle("Relación edad - % metilación gen ASPA.") 

# Ajusta el modelo de regresió lineal simple
reg <- lm(Age ~ ASPA, data = age.aspa)

# Da el resumen del modelo ajustado
summary(reg)

# Extrae información del modelo de regresión lineal simple

summary(reg$residuals) # Información de los residuos
summary(reg)$sigma  # Desviación estandar residual
summary(reg)$r.squared # Coeficiente de determinación (R cuadrado)
summary(reg)$adj.r.squared # R cuadrado ajustado

# Hace el ANOVA del modelo de regresión lineal simple

anova(reg)

# Realiza la predicción forense de edad.

predict.lm(reg, 
           newdata=data.frame(ASPA=c(0.25,0.50,0.75)), 
           interval="confidence")

#_______________________________________________________________
# Supuestos del modelo de regresión lineal simple
# Equivalentes a los de anova: Independencia, normalidad y homogeneidad de varianzas
#_______________________________________________________________

# INDEPENDENCIA

#H0: Los residuos son independientes entre sí.
#HA: Los residuos no son independientes entre sí (existe autocorrelación).

# Modelo de regresión lineal simple
reg <- lm(Age ~ ASPA, data = age.aspa)

plot(reg$residuals, pch=20, col = "blue")

# Realiza la prueba de Durbin Watson
# dwtest pertenece a library(lmtest)

DW_1 <- dwtest(Age ~ ASPA, data = age.aspa, 
               alternative = c("two.sided"), iterations = 15)

# Muestra los resultados de la prueba en formato tabla
pander::pander((DW_1), caption = "Durbin Watson test")


# LINEALIDAD

#H0: Hay relación lineal entre la variable regresora y la variable predictora.
#HA: No hay relación lineal entre la variable regresora y la variable predictora.

q+My_Theme+ggtitle("Relación edad - % metilación gen ASPA.") 

# HOMOGENEIDAD DE VARIANZAS

#H0: La varianza de los residuos es constante.
#HA: La varianza de los residuos no es constante.

plot(reg, which=3)

# Realiza la prueba de Levene
help(leveneTest)
leveneTest(Age ~ ASPA, data = age.aspa,
           center = "median")

# Realiza la prueba de Breusch-Pagan
bp_test <- bptest(reg)

# Muestra los resultados de la prueba en formato tabla
pander::pander((bp_test), caption = "Breusch-Pagan test")

# NORMALIDAD: GRÁFICO DE CUANTILES

#H0: Los residuos tienen distribución normal.  
#HA**: Los residuos no tienen distribución normal.  

# Realiza el gráfico de cuantiles (QQ plot)
qqPlot(reg) # library(car)

# Extrae los residuos del modelo de regresión lineal simple
lm_residuals <- residuals(object = reg)

# Realiza la prueba de Shapiro
shaptest_1 <- shapiro.test(x= lm_residuals)

# Muestra los resultados de la prueba en formato tabla
pander::pander((shaptest_1), caption = "Shapiro test")

# VALORES ATÍPICOS
plot(reg, which=4)

