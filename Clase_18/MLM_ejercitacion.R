# --------------------------------------------------------------
# Clase 18 - Script Introducción Modelos lineale mixtos
# Dr. José Gallardo Matus & Dra. Angélica Rueda Calderón
# 12 noviembre 2022
# Diplomado Análisis de datos con R e investigación reproducible para Biociencias.
# ---------------------------------------------------------------------------------

# Habilitar paquetes
library(car)
library(lmtest)
library(psych)
library(readxl)
library(ggplot2)
library(dplyr)
library(knitr)
library(nlme)
library(lme4)
library(lmerTest)
library(multcomp)
library(sjPlot)
library(performance)
library(caret)
library(ggeffects) 
library(glmmTMB)

# Importa base de datos
datos <- read_excel("UGELVIK_et_al_2017.xlsx")
datos <- na.omit(datos)
datos <- datos[,c("Tank","Weight","Nbfemales", "DaysPI", "Status")]
datos$Tank <- as.factor(datos$Tank)
datos$Status <- as.factor(datos$Status)
datos_new <- datos[,c("Weight","Nbfemales", "DaysPI")]

pander::pander(head(datos, caption = "Base de datos",10))

# Histograma de la variable respuesta
ggplot(datos, aes(x=Weight))+
  geom_histogram(color="slateblue4", fill="steelblue1", bins=6)+ labs(x="Weight (g)")+
  theme_classic()+theme(text = element_text(size=20,face="bold",colour="black"))

# Ajuste modelo lineal 
ML <- lm(Weight ~ Status+ Nbfemales+ DaysPI, data = datos)

# Ver los resultados del modelo ajustado anteriormente
tab_model(ML, show.se = TRUE, show.aic=TRUE)

# Cumplimento de supuestos

# Independencia
plot(ML$residuals, pch=20, col = "blue")


dwtest(Weight ~ Status+ Nbfemales+ DaysPI, 
       data = datos,
       alternative = c("two.sided"), 
       iterations = 15)

# Homogeneidad de varianzas
plot(ML, 1, pch=20, col = "blue")

bptest(ML)

# Normalidad

plot(ML, 2, pch=20, col = "blue")

lm_residuals <- residuals(object = ML)
shapiro.test(x= lm_residuals)

# Multicolinealidad

pairs.panels(datos_new)

# Linealidad

ggplot(datos_new, aes(x = DaysPI, y = Weight))+
geom_point() + xlab("DaysPI") +  ylab("Weight (g)")+ 
  geom_smooth(method=lm)+theme_classic()+theme(text = element_text(size=20,face="bold",colour="black"))

# Modelos lineal mixtos

# Ajustar modelo lineal mixto
MLM <- lmer(Weight ~ Status+ Nbfemales+ DaysPI + (1|Tank), data = datos)

summary(MLM)

# Gráfico residuos vs predichos
plot(MLM)

# Mostrar los resultados del modelo ajustado en formato tabla html
tab_model(MLM,p.val = "kr", show.se = TRUE, show.aic=TRUE)

# Calcular R2 del MLM
r2_nakagawa(MLM)

# Comparación de modelos con AIC y BIC
AIC(ML, MLM)
BIC(ML, MLM)

