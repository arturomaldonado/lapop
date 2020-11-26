library(ggmap)
library(ggplot2)
library(raster)
library(maptools)
library(maps)
library(mapdata)
library(rgdal)
library(classInt)
library(rio)
library(pacman)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)

# first map of LA -----------------------------------------------------
#la<- readOGR("LatinAmerica.shp", layer="LatinAmerica")
#la@data
#View(la@data)

am <- readOGR("Americas.shp", layer="Americas")
am@data
View(am@data)
plot(am)

am <- shapefile("Americas.shp")
plot(am)
View(am@data)

#Eliminando los países que no están en el BA
am <- am[-c(1, 2, 4, 6, 13, 16, 21, 22, 24, 25, 31, 32, 39, 40, 43, 45, 49, 50, 51, 52), ]
View(am@data)
plot(am)


#Adecuar mapa: borrar países que no se usan en BA
#Reproducir mapa con áreas grises sin datos
#Hacer shiny con mapa por diferentes var de batería B
#Incluir shiny en Markdown

#la <- la[la$OBJECTID !=2,] #Eliminar Guyana Frncesa
#la <- la[la$OBJECTID !=3,] #Eliminar Guyana
#la <- la[la$OBJECTID !=7,] #Eliminar Surinam
#la <- la[la$OBJECTID !=9,] #Eliminar Venezuela
#la <- la[la$OBJECTID !=50,] #Eliminar Belice
#View(la@data)
#plot(la)

#Variable que quiero plotear
lapop<-import("LAPOP_Merge_2004_2018.dta")
#lapop <- subset(lapop, wave==2018)
lapop$pais = as.factor(lapop$pais)
levels(lapop$pais) <- c("México", "Guatemala", "El Salvador", "Honduras",
                        "Nicaragua","Costa Rica", "Panamá", "Colombia", 
                        "Ecuador", "Bolivia", "Perú", "Paraguay", "Chile",
                        "Uruguay", "Brasil","Venezuela", "Argentina", 
                        "Rep. Dom.", "Haití", "Jamaica", "Guyana", "Trinidad y Tobago", 
                        "Belice", "Surinam", "Bahamas", "Barbados", "Granada", "Santa Lucía",
                        "Dominica", "San Vicente y las Granadinas", 
                        "San Cristobal y Nieves","Estados Unidos", "Canada")
table(lapop$pais)

#Variables de confianza
table(lapop$b13) #Congreso
table(lapop$b21) #Partidos
table(lapop$b31) #Corte Suprema
table(lapop$b47a) #Elecciones

table(lapop$b13[lapop$wave==2018])

lapop$b13r = ((lapop$b13-1)/6*100)
lapop$b21r = ((lapop$b21-1)/6*100)
lapop$b31r = ((lapop$b31-1)/6*100)
lapop$b47r = ((lapop$b47a-1)/6*100)

#Creando un dataframe para las variables y el año
library(descr) # Para poder usar la función crosstab y compmeans

#Para la variable: congreso
tabla <- as.data.frame(
  compmeans(lapop$b13r[lapop$wave==2018], lapop$pais[lapop$wave==2018], lapop$weight1500[lapop$wave==2018], plot=FALSE))
varnames <- c("media_cong", "n_cong", "sd_cong")
colnames(tabla) <- varnames
tabla$pais <- row.names(tabla)
tabla <- tabla[-34, ]

#Para la variable Partidos
tab.part <- as.data.frame(
  compmeans(lapop$b21r[lapop$wave==2018], lapop$pais[lapop$wave==2018], lapop$weight1500[lapop$wave==2018], plot=FALSE))
varnames2 <- c("media_part", "n_part", "sd_part")
colnames(tab.part) <- varnames2
tab.part$pais <- row.names(tab.part)
tab.part <- tab.part[-34, ]

#Uniendo los datos en el dataframe tabla
tabla$media_part <- tab.part$media_part
tabla$n_part <- tab.part$n_part
tabla$sd_part <- tab.part$sd_part

#Creando el vector código para hacer el match con el shapefile
tabla$OBJECTID <- NA
tabla <- within(tabla, {
  OBJECTID[pais=="Argentina"] <- 1
  OBJECTID[pais=="Barbados"]<- 2
  OBJECTID[pais=="Bahamas"]<- 3
  OBJECTID[pais=="Belice"]<-4
  OBJECTID[pais=="Bolivia"]<-5
  OBJECTID[pais=="Brasil"]<-6
  OBJECTID[pais=="Canada"]<-7
  OBJECTID[pais=="Chile"]<-8
  OBJECTID[pais=="Colombia"]<-9
  OBJECTID[pais=="Costa Rica"]<-10
  OBJECTID[pais=="Dominica"]<-11
  OBJECTID[pais=="Rep. Dom."]<-12
  OBJECTID[pais=="Ecuador"]<-13
  OBJECTID[pais=="El Salvador"]<-14
  OBJECTID[pais=="Granada"]<-15
  OBJECTID[pais=="Guatemala"]<-16
  OBJECTID[pais=="Guyana"]<-17
  OBJECTID[pais=="Haití"]<-18
  OBJECTID[pais=="Honduras"]<-19
  OBJECTID[pais=="Jamaica"]<-20
  OBJECTID[pais=="México"]<-21
  OBJECTID[pais=="Surinam"]<-22
  OBJECTID[pais=="Nicaragua"]<-23
  OBJECTID[pais=="Paraguay"]<-24
  OBJECTID[pais=="Perú"]<-25
  OBJECTID[pais=="Panamá"]<-26
  OBJECTID[pais=="San Cristobal y Nieves"]<-27
  OBJECTID[pais=="Santa Lucía"]<-28
  OBJECTID[pais=="Trinidad y Tobago"]<-29
  OBJECTID[pais=="Uruguay"]<-30
  OBJECTID[pais=="San Vicente y las Granadinas"]<-31
  OBJECTID[pais=="Venezuela"]<-32
  OBJECTID[pais=="Estados Unidos"]<-33
})

tabla <- tabla[order(tabla$OBJECTID),]


names(tabla)
row.names(tabla)<- row.names(am)
mapa <- SpatialPolygonsDataFrame(am, tabla)
View(mapa@data)

library(RColorBrewer)
plotvar<-mapa$media_cong
ncolor <-3
plotcol <-brewer.pal(ncolor, "Greens")
class <- classIntervals(round(plotvar,2), ncolor, style="equal")
colcode <- findColours(class, plotcol)

plot(mapa, col=colcode, border="grey", axes=T)
title(main="Confianza en congreso", cex=3)
legend("bottomright", legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.5)


m <- leaflet(am)
m %>% addPolygons(
  fillColor = ~pal(mapa$media_cong),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  popup = ~ popup_cong) 
  
pal <- colorNumeric("Greens", domain=mapa$media_cong)
popup_cong <- paste0("Prom.Variable ", as.character(round(mapa$media_cong, 2)))
leaflet() %>%
  addPolygons(data=mapa,
              fillColor = ~pal(mapa$media_cong),
              fillOpacity = 0.7,
              weight = 0.2,
              smoothFactor = 0.2,
              popup= ~popup_cong) %>%
  addLegend(pal=pal,
            values=mapa$media_cong,
            position="bottomright",
            title="Confianza en congreso")


#leaflet(data=mapa.cong) %>% addTiles() %>%
 # addPolygons(fillColor = topo.colors(3, alpha=NULL), stroke=F)






