### --------------------------------------------------------------------------------
### Script Regresión lineal múltiple y supuestos
### Dr. José A. Gallardo & Dra. Angélica Rueda
### 18 de octubre de 2022
### Diplomado Análisis de datos con R e investigación reproducible para Biociencias.
### --------------------------------------------------------------------------------

### Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(car)
library(lmtest)
library(psych)
library(agridat)
library(gridExtra)

# Importar datos estudio de maíz
# [heady.fertilizer](https://cran.rstudio.com/web/packages/agridat/agridat.pdf)

data(heady.fertilizer)
dat <- heady.fertilizer

# Seleccionar solo observaciones de maíz
d1 <- subset(dat, crop=="corn")

# Seleccionar solo las variables de interés
d2 <- d1[,c(3,5,6)]

# Ajustar modelo de regresión lineal múltiple 
m1 <- lm(yield ~ N + P + sqrt(N) + sqrt(P) + sqrt(N*P), data=d2)

# Imprime resultado del modelo m1 con función summary()
summary(m1)$coefficients%>%kable()

# Supuesto de independencia: PRUEBA DE DURBIN-WATSON
dwtest(yield ~ N + P + sqrt(N) + sqrt(P) + sqrt(N*P), 
       data=d2,
       alternative = c("two.sided"), 
       iterations = 15)

# Supuesto de linealidad: método gráfico

plot1 <- ggplot(data = d2, aes(x = N, y = yield)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Nitrógeno", y = "Rendimiento") +
  scale_shape_manual(values=c(1,2)) +
  stat_smooth(method='loess',formula=y~x, se=T)+
  scale_color_brewer(palette="Set1") + 
  theme(legend.position="none") +
  theme(panel.border=element_blank(), axis.line=element_line())

plot2  <- ggplot(data = d2, aes(x = P, y = yield)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Fósforo", y = "Rendimiento") +
  scale_shape_manual(values=c(1,2)) +
  stat_smooth(method='loess',formula=y~x, se=T)+
  scale_color_brewer(palette="Set1") + 
  theme(legend.position="none") +
  theme(panel.border=element_blank(), axis.line=element_line())

grid.arrange(plot1, plot2, ncol=2, nrow =1)

# Supuesto de homocedasticidad: PRUEBA DE BREUSCH-PAGAN
bptest(m1)

# Supuesto de multicolinealidad
pairs.panels(d2)

# Factor de inflación de la varianza (VIF)
vif(m1) %>% kable(digits=2, col.names = c("VIF"))

# Datos influyentes
influencePlot(m1)

# Supuesto de normalidad
shapiro.test(x= rstudent(m1)) 

# Eliminar el residuo de la observación 90
shapiro.test(x= rstudent(m1)[-90])

# Ajusta modelo de regresión múltiple completo
lm0<- lm(yield ~ N + P + sqrt(N) + sqrt(P) + sqrt(N*P), data=d2)

# Imprime resultado de modelo de regresión múltiple
summary(lm0)$coef %>% kable()

# Ajusta modelo de regresión múltiple reducido
lm1<- lm(yield ~ N + P + N:P , data=d2)

# Imprime resultado de modelo de regresión múltiple
summary(lm1)$coef %>% kable()

# Ajusta modelo de regresión múltiple reducido
lm2<- lm(yield ~ sqrt(N) + sqrt(P) + sqrt(N*P), data=d2)

# Imprime resultado de modelo de regresión múltiple
summary(lm2)$coef %>% kable()

# Compara modelos usando residuales
anova(lm0, lm1, lm2) %>% kable()

# Compara modelos usando AIC
AIC(lm0, lm1, lm2) %>% kable()


# Compara modelos usando BIC
BIC(lm0, lm1, lm2) %>% kable()

