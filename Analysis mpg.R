library(tidyverse)



datos <- read.csv("C:\\users\\carlo\\Downloads\\mpg.csv")

#PLAN DE ACCION
#Agregar Variable general de mpg ((cty+hwy) /2)

#Vamos a sacar algunos graficos para observar mejor y ver como se comportan los datos
# 1.Cantidad por marca
# 2.Cantidad por Transmision
# 3.Mileage in city 30 bins -> histograma
# 4.Mileage in highways 30 bins -> histograma
# 5.Mileage in highways 30 bins -> histograma 30 bins
# 6.Cantidad porFuel types
# 7.Cantidad por Vehicle Class
# 8.Comparacion entre displacement y mpg (tanto para cty, hwy and general) y realizar una curva de tendencia
#  8.1 Cada dato de este histograma esta con un color correspondiente al tipo de auto (class) -> Histograma
#
#
#
#Posteriormente se sacara la media, varianza y desviacion estandar de cada marca pero ligada a un tipo de vehiculo.
#Esto debido a que nuestro analisis es ver que marcas producen los autos mas eficiente, pero dado que ciertas marcas producen 
#unicamente cierto tipo de vehiculos, el futuro analisis sera enfocado en comparar para cada tipo de vehiculo, que marca produce ese tipo mejor

# 9. Dividir nuestros datos principales en secciones de datos para posteriormente utlizar las formulas
#  9.1 Filtrar datos por marca y tipo
#  9.2 Creamos una lista con todos los subrgrupos para posteriormente sacar la media, varianza y desviacion estandar de cada subgrupo
#  9.3 Creamos un loop donde se saquen estas mediciones y se imprima una tabla que junte toda la informacion


#Agregamos Variable general de mpg
datos <- mutate(datos, mpg.general = (cty+hwy)/2)



# 1.Cantidad de Vehiculos por marca (BARRAS)
grafico.manufacturer <- ggplot(data=datos, aes(x=manufacturer, fill = manufacturer)) + geom_bar() + theme(legend.position="none") 

# 2.Cantidad de vehiculos por transmision (BARRAS)
grafico.transmission <- ggplot(data=datos, aes(x=trans, fill = trans)) + geom_bar() + theme_minimal() + theme(legend.position="none")

# 3.Histograma de mpg en ciudad (HISTOGRAMA)
grafico.mpg.ciudad <- ggplot(data=datos) + aes(x=cty) + geom_histogram()

# 4.Histograma de mpg en highway (HISTOGRAMA)
grafico.mpg.highway <- ggplot(data=datos) + aes(x=hwy) + geom_histogram()

# 5.Histograma de mpg en general (HISTOGRAMA)
grafico.mpg.general <- ggplot(data=datos) + aes(x=mpg.general) + geom_histogram()

# 6.Cantidad de vehiculos por tipo de combustible (BARRAS)
grafico.fuel <- ggplot(data=datos, aes(x=fl, fill = fl)) + geom_bar() + theme(legend.position="none") 

# 7.cantidad de vehiculos por clase (BARRAS)
grafico.class <- ggplot(data=datos, aes(x=class, fill = class)) + geom_bar() + theme(legend.position="none") 

# 8.1 Histograma de comparacion entre displacement (medida utlizada para el tamano del motor) y mpg cty
grafico.comparacion.displ.cty <- ggplot(data = datos, mapping = aes(x = displ, y = cty)) + 
  geom_point(mapping=aes(color=class)) +
  geom_smooth()

# 8.2 Histograma de comparacion entre displacement (medida utlizada para el tamano del motor) y mpg hwy
grafico.comparacion.displ.hwy <- ggplot(data = datos, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping=aes(color=class)) +
  geom_smooth()

# 8.3 Histograma de comparacion entre displacement (medida utlizada para el tamano del motor) y mpg general 
grafico.comparacion.displ.mpggeneral <- ggplot(data = datos, mapping = aes(x = displ, y = mpg.general)) + 
  geom_point(mapping=aes(color=class)) +
  geom_smooth()








# 9.1
audi.compact <- filter(datos, manufacturer == "audi" & class == "compact")
audi.midsize <- filter(datos, manufacturer == "audi" & class == "midsize")

chevrolet.suv <- filter(datos, manufacturer == "chevrolet" & class == "suv")
chevrolet.2seater <- filter(datos, manufacturer == "chevrolet" & class == "2seater")
chevrolet.midsize <- filter(datos, manufacturer == "chevrolet" & class == "midsize")

dodge.minivan <- filter(datos, manufacturer == "dodge" & class == "minivan")
dodge.pickup  <- filter(datos, manufacturer == "dodge" & class == "pickup")
dodge.suv  <- filter(datos, manufacturer == "dodge" & class == "suv")

ford.suv  <- filter(datos, manufacturer == "ford" & class == "suv")
ford.pickup  <- filter(datos, manufacturer == "ford" & class == "pickup")
ford.subcompact  <- filter(datos, manufacturer == "ford" & class == "subcompact")

honda.subcompact <- filter(datos, manufacturer == "honda" & class == "subcompact")

hyundai.midsize <- filter(datos, manufacturer == "hyundai" & class == "midsize")
hyundai.subcompact <- filter(datos, manufacturer == "hyundai" & class == "subcompact")

jeep.suv <- filter(datos, manufacturer == "jeep" & class == "suv")

landrover.suv <- filter(datos, manufacturer == "land rover" & class == "suv")

lincoln.suv <- filter(datos, manufacturer == "lincoln" & class == "suv")

mercury.suv <- filter(datos, manufacturer == "mercury" & class == "suv")

nissan.compact <- filter(datos, manufacturer == "nissan" & class == "compact")
nissan.midsize <- filter(datos, manufacturer == "nissan" & class == "midsize")
nissan.suv <- filter(datos, manufacturer == "nissan" & class == "suv")

pontiac.midsize <- filter(datos, manufacturer == "pontiac" & class == "midsize")

subaru.suv <- filter(datos, manufacturer == "subaru" & class == "suv")
subaru.subcompact <- filter(datos, manufacturer == "subaru" & class == "subcompact")
subaru.compact <- filter(datos, manufacturer == "subaru" & class == "compact")

toyota.suv <- filter(datos, manufacturer == "toyota" & class == "suv")
toyota.midsize <- filter(datos, manufacturer == "toyota" & class == "midsize")
toyota.compact <- filter(datos, manufacturer == "toyota" & class == "compact")
toyoya.pickup <- filter(datos, manufacturer == "toyota" & class == "pickup")

volkswagen.compact <- filter(datos, manufacturer == "volkswagen" & class == "compact")
volkswagen.subcompact <- filter(datos, manufacturer == "volkswagen" & class == "subcompact")
volkswagen.midsize <- filter(datos, manufacturer == "volkswagen" & class == "midsize")

#Aca Creamos una Lista

lista.de.subrgrupos <- list(audi.compact, audi.midsize, chevrolet.suv , chevrolet.2seater, 
   chevrolet.midsize, dodge.minivan,dodge.pickup, dodge.suv, ford.suv, ford.pickup, 
   ford.subcompact, honda.subcompact, hyundai.midsize,hyundai.subcompact, jeep.suv, landrover.suv,
   lincoln.suv, mercury.suv, nissan.compact, nissan.midsize, nissan.suv, pontiac.midsize,
   subaru.suv, subaru.subcompact, subaru.compact,toyota.suv, toyota.midsize, toyota.compact,
   toyoya.pickup, volkswagen.compact, volkswagen.subcompact, volkswagen.midsize
  )

#Creamos el loop para realizar las mediciones y realizar una tabla. La idea es guardar el tipo y sus valores

marcas <- list()
tipos <- list() 
media.mpg.general <- list()
varianza.mpg.general <- list()
desviacion.estandar.mpg.general <- list()

for (i in lista.de.subrgrupos) {
 marcas <- c(marcas,i[1,2])
 tipos <- c(tipos, i[1,12])
 media.mpg.general <- c(media.mpg.general,mean(i$mpg.general))
 varianza.mpg.general <- c(varianza.mpg.general, var(i$mpg.general))
 desviacion.estandar.mpg.general <- c(desviacion.estandar.mpg.general, sd(i$mpg.general))
}

#Vamos a crear una tabla para observar los datos obtenidos anteriormente
tabla.datos.subgrupos <- cbind(marcas, tipos, media.mpg.general,varianza.mpg.general, desviacion.estandar.mpg.general)


tabla.datos.subgrupos= as.data.frame((as.data.frame(tabla.datos.subgrupos)))
print(tabla.datos.subgrupos)


#Mostrar graficos
grafico.manufacturer
grafico.transmission
grafico.mpg.ciudad
grafico.mpg.highway
grafico.mpg.general
grafico.fuel
grafico.class
grafico.comparacion.displ.cty
grafico.comparacion.displ.hwy
grafico.comparacion.displ.mpggeneral






#Pruebas T para cada tipo de vehiculo

#Por tipo o sea compara toyota suv contra nissan suv.....

#       Primeramente se comparan los autos compactos de Audi, Nissan
#       Subaru, Toyota y Volkswagen

#       Para esto primeramente debemos comparar las varianzas entre los subconjuntos
#       Pruebas F 

prueba.f.compactos.audi.vs.nissan <- var.test(audi.compact$mpg.general,nissan.compact$mpg.general)
prueba.f.compactos.audi.vs.subaru <- var.test(audi.compact$mpg.general,subaru.compact$mpg.general)
prueba.f.compactos.audi.vs.toyota <- var.test(audi.compact$mpg.general,toyota.compact$mpg.general)
prueba.f.compactos.audi.vs.volkswagen <- var.test(audi.compact$mpg.general,volkswagen.compact$mpg.general)

prueba.f.compactos.nissan.vs.subaru <- var.test(nissan.compact$mpg.general,subaru.compact$mpg.general)
prueba.f.compactos.nissan.vs.toyota <- var.test(nissan.compact$mpg.general,toyota.compact$mpg.general)
prueba.f.compactos.nissan.vs.volkswagen <- var.test(nissan.compact$mpg.general,volkswagen.compact$mpg.general)

prueba.f.compactos.subaru.vs.toyota <- var.test(subaru.compact$mpg.general,toyota.compact$mpg.general)
prueba.f.compactos.subaru.vs.volkswagen <- var.test(subaru.compact$mpg.general,volkswagen.compact$mpg.general)

prueba.f.compactos.toyota.vs.volkswagen <- var.test(toyota.compact$mpg.general,volkswagen.compact$mpg.general)

#     Muestra Pruebas
prueba.f.compactos.audi.vs.nissan  # p-value = 0.9456, NO hay diferencia
prueba.f.compactos.audi.vs.subaru  # p-value = 0.1481, No hay diferencia
prueba.f.compactos.audi.vs.toyota  # p-value = 0.03431, SI hay diferencia
prueba.f.compactos.audi.vs.volkswagen #  p-value = 0.003103, SI hay diferencia

prueba.f.compactos.nissan.vs.subaru  #p-value = 0.3117, NO hay diferencia
prueba.f.compactos.nissan.vs.toyota  #p-value = 0.6065, No hay diferencia
prueba.f.compactos.nissan.vs.volkswagen  #p-value = 0.4825, NO hay diferencia

prueba.f.compactos.subaru.vs.toyota  #p-value = 0.02676, SI hay diferencia
prueba.f.compactos.subaru.vs.volkswagen  #p-value = 0.01304, SI hay diferencia

prueba.f.compactos.toyota.vs.volkswagen  #p-value = 0.4254, NO hay diferencia


#Inicio de pruebas T

t.test(toyota.compact$mpg.general, audi.compact$mpg.general,var.equal = TRUE, alternative= "greater")
#



#Pruebas F de Midsize
#Audi, Chevrolet, Hyundai, Nissan,Toyota, Pontiac, volkswagen

prueba.f.midsize.audi.vs.chevrolet <- var.test(audi.midsize$mpg.general,chevrolet.midsize$mpg.general)
prueba.f.midsize.audi.vs.hyundai <- var.test(audi.midsize$mpg.general,hyundai.midsize$mpg.general)
prueba.f.midsize.audi.vs.nissan <- var.test(audi.midsize$mpg.general,nissan.midsize$mpg.general)
prueba.f.midsize.audi.vs.toyota <- var.test(audi.midsize$mpg.general,toyota.midsize$mpg.general)
prueba.f.midsize.audi.vs.pontiac <- var.test(audi.midsize$mpg.general,pontiac.midsize$mpg.general)
prueba.f.midsize.audi.vs.volkswagen <- var.test(audi.midsize$mpg.general,volkswagen.midsize$mpg.general)

prueba.f.midsize.chevrolet.vs.hyundai <- var.test(chevrolet.midsize$mpg.general,hyundai.midsize$mpg.general)
prueba.f.midsize.chevrolet.vs.nissan <- var.test(chevrolet.midsize$mpg.general,nissan.midsize$mpg.general)
prueba.f.midsize.chevrolet.vs.toyota <- var.test(chevrolet.midsize$mpg.general,toyota.midsize$mpg.general)
prueba.f.midsize.chevrolet.vs.pontiac <- var.test(chevrolet.midsize$mpg.general,pontiac.midsize$mpg.general)
prueba.f.midsize.chevrolet.vs.volkswagen <- var.test(chevrolet.midsize$mpg.general,volkswagen.midsize$mpg.general)

prueba.f.midsize.hyundai.vs.nissan <- var.test(hyundai.midsize$mpg.general,nissan.midsize$mpg.general)
prueba.f.midsize.hyundai.vs.toyota <- var.test(hyundai.midsize$mpg.general,toyota.midsize$mpg.general)
prueba.f.midsize.hyundai.vs.pontiac <- var.test(hyundai.midsize$mpg.general,pontiac.midsize$mpg.general)
prueba.f.midsize.hyundai.vs.volkswagen <- var.test(hyundai.midsize$mpg.general,volkswagen.midsize$mpg.general)

prueba.f.midsize.nissan.vs.toyota <- var.test(nissan.midsize$mpg.general,toyota.midsize$mpg.general)
prueba.f.midsize.nissan.vs.pontiac <- var.test(nissan.midsize$mpg.general,pontiac.midsize$mpg.general)
prueba.f.midsize.nissan.vs.volkswagen <- var.test(nissan.midsize$mpg.general,volkswagen.midsize$mpg.general)


prueba.f.midsize.toyota.vs.pontiac <- var.test(toyota.midsize$mpg.general,pontiac.midsize$mpg.general)
prueba.f.midsize.toyota.vs.volkswagen <- var.test(toyota.midsize$mpg.general,volkswagen.midsize$mpg.general)

prueba.f.midsize.pontiac.vs.volkswagen <- var.test(pontiac.midsize$mpg.general,volkswagen.midsize$mpg.general)


#Llamar a las variables y observar resultados
prueba.f.midsize.audi.vs.chevrolet #p-value = 0.4112, NO hay diferencia 
prueba.f.midsize.audi.vs.hyundai #p-value = 0.4298, NO hay diferencia
prueba.f.midsize.audi.vs.nissan #  p-value = 0.231, NO HAY DIFERENCIA
prueba.f.midsize.audi.vs.toyota # p-value = 0.4434, NO HAY DIFERENCIA
prueba.f.midsize.audi.vs.pontiac # p-value = 0.9719, NO HAY DIFERENCIA
prueba.f.midsize.audi.vs.volkswagen #  p-value = 0.4786, NO HAY DIFERENCIA

prueba.f.midsize.chevrolet.vs.hyundai #
prueba.f.midsize.chevrolet.vs.nissan #
prueba.f.midsize.chevrolet.vs.toyota #
prueba.f.midsize.chevrolet.vs.pontiac #
prueba.f.midsize.chevrolet.vs.volkswage# 

prueba.f.midsize.nissan.vs.toyota #
prueba.f.midsize.nissan.vs.pontiac #
prueba.f.midsize.nissan.vs.volkswagen #

prueba.f.midsize.toyota.vs.pontiac #
prueba.f.midsize.toyota.vs.volkswagen #

prueba.f.midsize.pontiac.vs.volkswagen #

#Pruebas T Midsize


# Pruebas f de subcompact
# marcas ford, honda, hyundai, subaru, volkswagen
prueba.f.subcompact.ford.vs.honda <- var.test(ford.subcompact$mpg.general,honda.subcompact$mpg.general)
prueba.f.subcompact.ford.vs.hyundai <- var.test(ford.subcompact$mpg.general,hyundai.subcompact$mpg.general)
prueba.f.subcompact.ford.vs.subaru <- var.test(ford.subcompact$mpg.general,subaru.subcompact$mpg.general)
prueba.f.subcompact.ford.vs.volkswagen <- var.test(ford.subcompact$mpg.general,volkswagen.subcompact$mpg.general)

prueba.f.subcompact.honda.vs.hyundai <- var.test(honda.subcompact$mpg.general,hyundai.subcompact$mpg.general)
prueba.f.subcompact.honda.vs.subaru <- var.test(honda.subcompact$mpg.general,subaru.subcompact$mpg.general)
prueba.f.subcompact.honda.vs.volkswagen <- var.test(honda.subcompact$mpg.general,volkswagen.subcompact$mpg.general)

prueba.f.subcompact.hyundai.vs.subaru <- var.test(hyundai.subcompact$mpg.general,subaru.subcompact$mpg.general)
prueba.f.subcompact.hyundai.vs.volkswagen <- var.test(hyundai.subcompact$mpg.general,volkswagen.subcompact$mpg.general)

prueba.f.subcompact.subaru.vs.volkswagen <- var.test(subaru.subcompact$mpg.general,volkswagen.subcompact$mpg.general)

#   Muestra Pruebas
prueba.f.subcompact.ford.vs.volkswagen #
prueba.f.subcompact.ford.vs.subaru #
prueba.f.subcompact.ford.vs.hyundai #
prueba.f.subcompact.ford.vs.honda #

prueba.f.subcompact.honda.vs.volkswagen #
prueba.f.subcompact.honda.vs.subaru #
prueba.f.subcompact.honda.vs.hyundai #


prueba.f.subcompact.hyundai.vs.volkswagen #
prueba.f.subcompact.hyundai.vs.subaru #

prueba.f.subcompact.subaru.vs.volkswagen #

#Pruebas T de SUBCOMPACT

# Pruebas f de pickup
# marcas dodge, ford, toyota
prueba.f.pickup.ford.vs.dodge <- var.test(ford.pickup$mpg.general,dodge.pickup$mpg.general)
prueba.f.pickup.ford.vs.toyota <- var.test(ford.pickup$mpg.general,toyoya.pickup$mpg.general)

prueba.f.pickup.toyota.vs.dodge <- var.test(toyoya.pickup$mpg.general,dodge.pickup$mpg.general)


#     Muestra pruebas
prueba.f.pickup.ford.vs.dodge #
prueba.f.pickup.ford.vs.toyota #

prueba.f.pickup.toyota.vs.dodge #

#Pruebas T PICKUPS

#Prueba f suv
#chevrolet dodge ford jeep landrover lincoln mercury nissan subaru toyota
prueba.f.suv.chevrolet.vs.dodge <- var.test(chevrolet.suv$mpg.general,dodge.suv$mpg.general)
prueba.f.suv.chevrolet.vs.ford <- var.test(chevrolet.suv$mpg.general,ford.suv$mpg.general)
prueba.f.suv.chevrolet.vs.jeep <- var.test(chevrolet.suv$mpg.general,jeep.suv$mpg.general)
prueba.f.suv.chevrolet.vs.landrover <- var.test(chevrolet.suv$mpg.general,landrover.suv$mpg.general)
prueba.f.suv.chevrolet.vs.lincoln <- var.test(chevrolet.suv$mpg.general,lincoln.suv$mpg.general)
prueba.f.suv.chevrolet.vs.mercury <- var.test(chevrolet.suv$mpg.general,mercury.suv$mpg.general)
prueba.f.suv.chevrolet.vs.nissan <- var.test(chevrolet.suv$mpg.general,nissan.suv$mpg.general)
prueba.f.suv.chevrolet.vs.subaru <- var.test(chevrolet.suv$mpg.general,subaru.suv$mpg.general)
prueba.f.suv.chevrolet.vs.toyota <- var.test(chevrolet.suv$mpg.general,toyota.suv$mpg.general)

prueba.f.suv.dodge.vs.ford <- var.test(dodge.suv$mpg.general,ford.suv$mpg.general)
prueba.f.suv.dodge.vs.jeep <- var.test(dodge.suv$mpg.general,jeep.suv$mpg.general)
prueba.f.suv.dodge.vs.landrover <- var.test(dodge.suv$mpg.general,landrover.suv$mpg.general)
prueba.f.suv.dodge.vs.lincoln <- var.test(dodge.suv$mpg.general,lincoln.suv$mpg.general)
prueba.f.suv.dodge.vs.mercury <- var.test(dodge.suv$mpg.general,mercury.suv$mpg.general)
prueba.f.suv.dodge.vs.nissan <- var.test(dodge.suv$mpg.general,nissan.suv$mpg.general)
prueba.f.suv.dodge.vs.subaru <- var.test(dodge.suv$mpg.general,subaru.suv$mpg.general)
prueba.f.suv.dodge.vs.toyota <- var.test(dodge.suv$mpg.general,toyota.suv$mpg.general)

prueba.f.suv.ford.vs.jeep <- var.test(ford.suv$mpg.general,jeep.suv$mpg.general)
prueba.f.suv.ford.vs.landrover <- var.test(ford.suv$mpg.general,landrover.suv$mpg.general)
prueba.f.suv.ford.vs.lincoln <- var.test(ford.suv$mpg.general,lincoln.suv$mpg.general)
prueba.f.suv.ford.vs.mercury <- var.test(ford.suv$mpg.general,mercury.suv$mpg.general)
prueba.f.suv.ford.vs.nissan <- var.test(ford.suv$mpg.general,nissan.suv$mpg.general)
prueba.f.suv.ford.vs.subaru <- var.test(ford.suv$mpg.general,subaru.suv$mpg.general)
prueba.f.suv.ford.vs.toyota <- var.test(ford.suv$mpg.general,toyota.suv$mpg.general)

prueba.f.suv.jeep.vs.landrover <- var.test(jeep.suv$mpg.general,landrover.suv$mpg.general)
prueba.f.suv.jeep.vs.lincoln <- var.test(jeep.suv$mpg.general,lincoln.suv$mpg.general)
prueba.f.suv.jeep.vs.mercury <- var.test(jeep.suv$mpg.general,mercury.suv$mpg.general)
prueba.f.suv.jeep.vs.nissan <- var.test(jeep.suv$mpg.general,nissan.suv$mpg.general)
prueba.f.suv.jeep.vs.subaru <- var.test(jeep.suv$mpg.general,subaru.suv$mpg.general)
prueba.f.suv.jeep.vs.toyota <- var.test(jeep.suv$mpg.general,toyota.suv$mpg.general)

prueba.f.suv.landrover.vs.lincoln <- var.test(landrover.suv$mpg.general,lincoln.suv$mpg.general)
prueba.f.suv.landrover.vs.mercury <- var.test(landrover.suv$mpg.general,mercury.suv$mpg.general)
prueba.f.suv.landrover.vs.nissan <- var.test(landrover.suv$mpg.general,nissan.suv$mpg.general)
prueba.f.suv.landrover.vs.subaru <- var.test(landrover.suv$mpg.general,subaru.suv$mpg.general)
prueba.f.suv.landrover.vs.toyota <- var.test(landrover.suv$mpg.general,toyota.suv$mpg.general)

prueba.f.suv.lincoln.vs.mercury <- var.test(lincoln.suv$mpg.general,mercury.suv$mpg.general)
prueba.f.suv.lincoln.vs.nissan <- var.test(lincoln.suv$mpg.general,nissan.suv$mpg.general)
prueba.f.suv.lincoln.vs.subaru <- var.test(lincoln.suv$mpg.general,subaru.suv$mpg.general)
prueba.f.suv.lincoln.vs.toyota <- var.test(lincoln.suv$mpg.general,toyota.suv$mpg.general)

prueba.f.suv.mercury.vs.nissan <- var.test(mercury.suv$mpg.general,nissan.suv$mpg.general)
prueba.f.suv.mercury.vs.subaru <- var.test(mercury.suv$mpg.general,subaru.suv$mpg.general)
prueba.f.suv.mercury.vs.toyota <- var.test(mercury.suv$mpg.general,toyota.suv$mpg.general)

prueba.f.suv.nissan.vs.subaru <- var.test(nissan.suv$mpg.general,subaru.suv$mpg.general)
prueba.f.suv.nissan.vs.toyota <- var.test(nissan.suv$mpg.general,toyota.suv$mpg.general)

prueba.f.suv.subaru.vs.toyota <- var.test(subaru.suv$mpg.general,toyota.suv$mpg.general)

#       Mostrar Pruebas
prueba.f.suv.chevrolet.vs.dodge #
prueba.f.suv.chevrolet.vs.ford #
prueba.f.suv.chevrolet.vs.jeep #
prueba.f.suv.chevrolet.vs.landrover #
prueba.f.suv.chevrolet.vs.lincoln # 
prueba.f.suv.chevrolet.vs.mercury #
prueba.f.suv.chevrolet.vs.nissan #
prueba.f.suv.chevrolet.vs.subaru #
prueba.f.suv.chevrolet.vs.toyota #

prueba.f.suv.dodge.vs.ford #
prueba.f.suv.dodge.vs.jeep #
prueba.f.suv.dodge.vs.landrover #
prueba.f.suv.dodge.vs.lincoln #
prueba.f.suv.dodge.vs.mercury #
prueba.f.suv.dodge.vs.nissan #
prueba.f.suv.dodge.vs.subaru #
prueba.f.suv.dodge.vs.toyota #

prueba.f.suv.ford.vs.jeep #
prueba.f.suv.ford.vs.landrover #
prueba.f.suv.ford.vs.lincoln #
prueba.f.suv.ford.vs.mercury # 
prueba.f.suv.ford.vs.nissan #
prueba.f.suv.ford.vs.subaru #
prueba.f.suv.ford.vs.toyota #

prueba.f.suv.jeep.vs.landrover #
prueba.f.suv.jeep.vs.lincoln #
prueba.f.suv.jeep.vs.mercury #
prueba.f.suv.jeep.vs.nissan #
prueba.f.suv.jeep.vs.subaru #
prueba.f.suv.jeep.vs.toyota #

prueba.f.suv.landrover.vs.lincoln #
prueba.f.suv.landrover.vs.mercury #
prueba.f.suv.landrover.vs.nissan #
prueba.f.suv.landrover.vs.subaru #
prueba.f.suv.landrover.vs.toyota #

prueba.f.suv.lincoln.vs.mercury #
prueba.f.suv.lincoln.vs.nissan #
prueba.f.suv.lincoln.vs.subaru #
prueba.f.suv.lincoln.vs.toyota #

prueba.f.suv.mercury.vs.nissan #
prueba.f.suv.mercury.vs.subaru #
prueba.f.suv.mercury.vs.toyota #
 
prueba.f.suv.nissan.vs.subaru #
prueba.f.suv.nissan.vs.toyota #

prueba.f.suv.subaru.vs.toyota #


#PRUEBAS T SUV