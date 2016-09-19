library(plyr)
library(ggplot2)
##### Genero data frame que contiene usuario-lugar-cantidad 
usuario_lugar <- read.csv(file = 'count_tips_con_user_grafo.csv',header = F)
names(usuario_lugar)<- c('usuario','lugar','rating')
lugares <- read.csv(file = 'NYC-Venues.csv',sep ='\t',encoding='UTF-8')
length(unique(usuario_lugar$usuario))
length(unique(lugares$numVenueId))
str(lugares)
str(usuario_lugar)
lugares$numVenueId <- as.numeric(lugares$numVenueId)
usuario_lugar <- merge(x = usuario_lugar,y = lugares[,c('lat','lon','numVenueId','name','address','city')], by.x=c('lugar'),by.y=c('numVenueId'),all.x=T)
#################################################################################
# Grafica un mapa

# create circles data frame from the centers data frame
make_circles <- function(centers, radius, nPoints = 100){
    # centers: the data frame of centers with ID
    # radius: radius measured in kilometer
    #
    meanLat <- (centers$lat)
    # length per longitude changes with lattitude, so need correction
    radiusLon <- radius /111 / cos(meanLat/57.3) 
    radiusLat <- radius / 111
    circleDF <- data.frame(ID = rep(centers$usuario, each = nPoints))
    angle <- seq(0,2*pi,length.out = nPoints)
    
    circleDF$lon <- unlist(lapply(centers$lon, function(x) x + radiusLon * cos(angle)))
    circleDF$lat <- unlist(lapply(centers$lat, function(x) x + radiusLat * sin(angle)))
    return(circleDF)
}

library(ggmap)
graficar_mapa_mundo <- function(puntos,guardar=FALSE){
    densidad <- sum(puntos[,c('rating')])
    NumPoints <- nrow(puntos)    
    mp <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp <- ggplot() +   mapWorld
    #Now Layer the cities on top
    mp <- mp+ geom_point(data = puntos,aes(x=lon, y=lat) ,color="blue", size=3) +
        ggtitle(
            paste("Visitas de Usuario: ", puntos[,c('usuario')], " con ", NumPoints, " lugares y  con  ", densidad, " vistas totales ", sep = "")
        )
    
    if (guardar == FALSE) {
        mp  
    } else (
        ggsave(paste("Visitas de Usuario", puntos[,c('usuario')], ".pdf", sep = ""), mp, width = 9, height = 9)
    )
}

graficar_mapa_interseccion <- function(puntos, guardar=FALSE){
    if (nrow(puntos)==1) {
        bbox <- c(puntos[,c('lon')],puntos[,c('lat')])
        zoom <- 'auto'
    }else {
        bbox<- make_bbox(lon,lat,puntos,f = 2)
        zoom <-calc_zoom(bbox)
    }
    NumPoints <- nrow(puntos) 
    mapgilbert <- get_map(location = bbox, source = "google",zoom = zoom, maptype = "terrain")
    # plotting the map with some points on it
    the_map <- ggmap(mapgilbert) +
        geom_point(data = puntos, aes(x = lon, y = lat), size = 4, color = "red") +
        ggtitle(
            paste("Vecinos de usuario: ", puntos[,c('usuario')], " con ", NumPoints, " vecinos ", sep = "")
        )
    
    if (guardar == FALSE) {
        the_map  
    } else (
        
        ggsave(paste("Vecinos_de_usuario", puntos[,c('usuario')], ".pdf", sep = ""), the_map, width = 9, height = 9)
    )
    
}

zona_plot <- function(myZona,datos,radio=5,guardar=FALSE) {
    distancias <- distCosine(myZona[,c('lon','lat')],datos[1:nrow(datos),c('lon','lat')],6378.1)
    tmp <- datos[distancias<=radio,]
    if (nrow(tmp)==1) {
        bbox <- c(tmp[,c('lon')],tmp[,c('lat')])
        zoom <- 'auto'
    }else {
        bbox <- make_bbox(lon = lon, lat = lat, data = tmp,f = 3)
        zoom <- calc_zoom(bbox)
    }
    mymap <- get_map(location = bbox, source = "google",zoom = zoom, maptype = "terrain")
    # now we want to count up how many areas there are
    densidad <- sum(tmp[,c('rating')])
    NumPoints <- nrow(tmp)
    myCircles <- make_circles(myZona, 1)
    
    the_map <- ggmap(mymap) +
        geom_point(data = tmp, mapping = aes(x = lon, y = lat), size = 4, color = "black") +
        geom_polygon(data = myCircles, aes(lon, lat, group = ID), color = "red", alpha = 0)+
        ggtitle(
            paste("Zona de Usuario: ", myZona[,c('usuario')], " con ", NumPoints, " lugares y  con  ", densidad, " vistas totales ", sep = "")
        )
  
    if (guardar == FALSE) {
        the_map  
    } else (
        
    ggsave(paste("zona_usuario", myZona[,c('usuario')], ".pdf", sep = ""), the_map, width = 9, height = 9)
    )
}
#################################################################################
library(geosphere)
calcular_densidad_zona <- function(centro, datos, radio = 11){
     distancias <- distCosine(centro[,c('lon','lat')],datos[1:nrow(datos),c('lon','lat')],6378.1)
     densidad <- sum(datos[distancias<=radio,c('rating')])
     return (densidad)
 }

calcular_interseccion <- function(user,datos,radio=10){
    distancias <- distCosine(user[,c('lon','lat')],datos[,c('lon','lat')],6378.1)
    radioTotal <- radio*2
    return (datos[distancias<=radioTotal,])
}     
#################################################################################
generar_zona_por_usuario <- function(datos,radio=10){
    # datos tiene que tener la variables lat y lon
    # radio es el radio en km
    max_densidad <- 0
    mejor_centro <- datos[1,]
    
    for (i in 1:nrow(datos)) {
        lugar_i <- datos[i,]
        densidad_i <- calcular_densidad_zona(lugar_i,datos,radio)
        if (densidad_i >= max_densidad){
            mejor_centro <- lugar_i
            mejor_centro$densidad <- densidad_i
            mejor_centro$radio <- radio
            max_densidad <- densidad_i
        }
        
    }
    return(as.vector(as.matrix(mejor_centro)))

}
user_test <- usuario_lugar[usuario_lugar$usuario==32,]
generar_zona_por_usuario(user_test,1)

zonas <- ddply(usuario_lugar,c('usuario'),generar_zona_por_usuario,1)
names(zonas) <-  c("usuario", "lugar","usuarioID","rating" , "lat" ,    "lon"   ,  "name"  ,  "address", "city","densidad","radio"  )
zonas$lat <- as.numeric(zonas$lat)
zonas$lon <- as.numeric(zonas$lon)
head(zonas)
tail(zonas,n=10)
zona_test <- zonas[zonas$usuario==32,]
graficar_mapa_mundo(user_test,guardar = F)
zona_plot(zona_test,user_test,1,guardar = F)
intersect_user_test <- calcular_interseccion(zona_test,zonas,1)
graficar_mapa_interseccion(intersect_user_test,guardar = T)
graficar_mapa(user_test)
write.table(x = zonas[,c('usuario','lat','lon','radio')],file ="users_tips_grafo_zona.csv",sep = ",",row.names = FALSE, col.names = T,quote = FALSE)

########################### calcular la interseccion para los usuarios de NY
user_ny <- read.csv(file = 'count_tips_con_user_grafo_ny.csv',header = F)
names(user_ny)<- c('usuario','lugar','rating')
str(user_ny)
user_id_ny <- unique(user_ny$usuario)
user_ny_interseccion <- data.frame(usuario=NA,cant_vecinos=NA)
 for (i in 1:length(user_id_ny)) {
     user_id <- user_id_ny[i]
     zona_i <- zonas[zonas$usuario==user_id,]
     intersect_user_i <- calcular_interseccion(zona_i,zonas,1)
     intersect_user_i_df <- data.frame(usuario=user_id,cant_vecinos=nrow(intersect_user_i))
     user_ny_interseccion <- rbind(user_ny_interseccion,intersect_user_i_df)
     #print(paste(user_id,' ',nrow(intersect_user_i),sep = ' '))
 }
user_ny_interseccion <- user_ny_interseccion[-1,]
write.table(x = user_ny_interseccion,file ="user_ny_interseccion_zona.csv",sep = ",",row.names = FALSE, col.names = T,quote = FALSE)

for (i in 1:3){
    i <- 10
    user_id <- user_id_ny[i]
    user_test <- usuario_lugar[usuario_lugar$usuario== user_id,]
    zona_test <- zonas[zonas$usuario==user_id,]
    intersect_user_test <- calcular_interseccion(zona_test,zonas,1)
    graficar_mapa_mundo(user_test,guardar = T)
    zona_plot(zona_test,user_test,1,guardar = T)
    graficar_mapa_interseccion(intersect_user_test,guardar = T)
}

graficar_mapa_mundo(user_test,guardar = T)
zona_plot(zona_test,user_test,1,guardar = T)
intersect_user_test <- calcular_interseccion(zona_test,zonas,1)

hist(user_ny_interseccion$cant_vecinos)
max(user_ny_interseccion$cant_vecinos)
sum(user_ny_interseccion$cant_vecinos==1)
which(user_ny_interseccion$cant_vecinos==1)
user_ny_interseccion[ 226,]
