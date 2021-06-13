install.packages("car")
install.packages("pastecs")

library(tidyverse)
library(car)
library(pastecs)

# Entonces solo guardamos el conjunto en una variable
iris
dat <- iris

# Ahora utilizamos head para observar las primeras observaciones
head(dat)
str(dat)

#rango
min(dat$Sepal.Length)
max(dat$Sepal.Length)

range.1 <- range(dat$Sepal.Length)
range.1

# min
range.1[1]

#el max con 
range.1[2]

fun.range <- function(x){
  range <- max(x) - min(x)
  return(range)
}

fun.range(iris$Sepal.Length)
max(iris$Sepal.Length)
min(iris$Sepal.Length)

#media
mean(dat$Sepal.Length)

lapply(dat[,1:4], mean)

#mediana y cuantilos
median(dat$Sepal.Length)
quantile(dat$Sepal.Length, 0.25) # first quartile
quantile(dat$Sepal.length, 0.75) # third quartile

#otros cuantilos
quantile(dat$Sepal.Length, 0.4) # 4to decil
quantile(dat$Sepal.length, 0.98) # percentil 98
IQR(dat$Sepal.Length) # diferencia entre el 3er y 1er cuantil
quantile(dat$Sepal.Length, 0.75) - quantile(dat$Sepal.Length, 0.25)

#varianza y SD

sd(dat$Sepal.Length) # desviacion estandar
var(dat$Sepal.Length) # varianza
lapply(dat[, 1:4], sd)

#Summary
summary(dat)
by(dat, dat$Species, summary)
stat.desc(dat)

# coeficiente de variainza
sd(dat$Sepal.Length) / mean(dat$Sepal.Length)

# Moda
tab <- table(dat$Sepal.Length) # numero de ocurrencias
mode.1 <- sort(tab, decreasing = TRUE) # ordenar de mayor a menor
mode.1

# moda cualitativa
sort(table(dat$Species), decreasing = TRUE)

# o por medio de
summary(dat$Species)

# tabla de contingencia
dat$size <- ifelse(dat$Sepal.Length) < median(dat$Sepal.Length, "small", "big")
table(dat$size)
table(dat$Species, dat$size)

# tambien funcionan las xtabs
xtabs(~ dat$Species + dat$size)

# table con frecuencia relativa
prop.table(table(dat$Species, dat$size))

#porcentajes por fila
round(prop.table(table(dat$Species, dat$size), 1), 2)

#porcentajes por columna
round(prop.table(table(dat$Species, dat$size), 2), 2)

#redondeo a 2 digitos con round()

# Parte II - Graficos

# Barplot
barplot(table(dat$size)) # el uso de table() es obligatorio
barplot(table(dat$size, dat$Species)) # pueden sustituir iris por dat

# barplot de frecuencias
barplot(prop.table(table(dat$size)))

# en ggplot2
ggplot(data=dat, aes(x=dat$Spexies, fill=dat$size)) + 
  geom_bar() + 
  theme_minimal()

# Histogram
hist(dat$Sepal.Length)

ggplot(dat) +
  aes(x = Sepal.Length) +
  geom_histogram()

# Boxplot

boxplot(dat$Sepal.Length)

boxplot(dat$Sepal.Length ~ dat$Species)

ggplot(data=dat, mapping =aes(x=Species, y=Sepal,Length, fill = Species))+
  scale_fill_brewer(palette="Greys") +
  stat_boxplot(geom = "errorbar", wodth = 0.25) +
  geom_boxplot()

# Scatterplot 
plot(dat$Sepal.Length, dat$Petal.Length)

ggplot(dat) +
  aes(x = Sepal.Length, y = Petal.Length) +
  geom_point()

# Scatterplot por grupos

ggplot(dat) +
  aes(x= Sepal.Length, y = Petal.Length, colour = Species) +
  geom_point() +
  scale_color_hue()

#Alternativa 2

ggplot(dat) +
  aes(x = Sepal.Length, y = Petal.Length, color = Species) +
  geom_point()

# QQ-Plot

# Ver normalidad

# Dibuje los puntos en el qq-plot
qqnorm(dat$Sepal.Length)

# Dibuje la linea de referencia o linea de Henry
qqline(dat$Sepal.Length)

library(car) # el paquete debe estar instalado
#install.package("car")
qqplot(dat$Sepal.Length)

qqPlot(log(dat$Sepal.Length))

#Metodo 1
qplot(sample = log2(Sepal.Length), data = dat)

#metodo 2
ggplot(dat, aes(sample = Sepal.Length)) + stat_qq()

# Analisis por grupo
qqPlot(dat$Sepal.Length, grous = dat$size)

#Agrupamiento donde se coolorea por tamanno y forma por tamanno
qqplot(
  sample = Sepal.Length, data = dat,
  col = size, shape = size
)

# Grafico de Densidad
plot(density(dat$Sepal.Length))
plot(density(log(dat$Sepal.Length)))
  
ggplot(dat) +
  aes(x = Sepal.Length) +
  geom_density()

# Conocimiento Extra -- Prueba de Normalidad Shapiro Wilks

shapiro.test(dat$Sepal.Length)
shapiro.test(log(dat$Sepal.Length))

# valor de p es mayor a 0.05, por lo que la distribucion no difiere significativamente de la normal.
# en otras palabras, se asume la normalidad.

lapply(dat[1:4], shapiro.test)
