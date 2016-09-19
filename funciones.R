#Funcion que genera un csv con el par usuario-usario y su distancia en la red
calcular_distancia_usuario_usuario <- function(grafo){
        
    sink("grafo_distancias_user_user.csv")
    for(i in 1:100){
        for(j in (i+1):100){
            d<-distances(grafoUserTips,v = valid_names[i],to = valid_names[j])
            usuarioDist<-paste(valid_names[i],valid_names[j],d[1,1],sep = "\t")
            cat(usuarioDist)
            cat("\n")
        }
    }
    sink()
    
}
    

library(RCurl)
library(RJSONIO)
library(plyr)

geoCode <- function(address,verbose=TRUE) {
    if(verbose) cat(address,"\n")
    u <- url(address)
    doc <- getURL(u)
    x <- fromJSON(doc,simplify = FALSE)

    if(x$status=="OK") {
        lat <- x$results[[1]]$geometry$location$lat
        lng <- x$results[[1]]$geometry$location$lng
        location_type <- x$results[[1]]$geometry$location_type
        formatted_address <- x$results[[1]]$formatted_address
        address_components1_LN <-x$results[[1]]$address_components[[1]]$long_name
        address_components1_SN <-x$results[[1]]$address_components[[1]]$short_name
        address_components2_LN <-x$results[[1]]$address_components[[2]]$long_name
        address_components2_SN <-x$results[[1]]$address_components[[2]]$short_name
        address_components3_LN <-x$results[[1]]$address_components[[3]]$long_name
        address_components3_SN <-x$results[[1]]$address_components[[3]]$short_name
        address_components4_LN <-tryCatch({x$results[[1]]$address_components[[4]]$long_name},error=function(err){return (NA)})
        address_components4_SN <-tryCatch({x$results[[1]]$address_components[[4]]$short_name},error=function(err){return (NA)})
        address_components5_LN <-tryCatch({x$results[[1]]$address_components[[5]]$long_name},error=function(err){return (NA)})
        address_components5_SN <-tryCatch({x$results[[1]]$address_components[[5]]$short_name},error=function(err){return (NA)})
        address_components6_LN <-tryCatch({x$results[[1]]$address_components[[6]]$long_name},error=function(err){return (NA)})
        address_components6_SN <-tryCatch({x$results[[1]]$address_components[[6]]$short_name},error=function(err){return (NA)})
        return(c(lat, lng, 
                 location_type, 
                 formatted_address,
                 address_components1_LN,
                 address_components1_SN,
                 address_components2_LN,
                 address_components2_SN,
                 address_components3_LN,
                 address_components3_SN,
                 address_components4_LN,
                 address_components4_SN,
                 address_components5_LN,
                 address_components5_SN,
                 address_components6_LN,
                 address_components6_SN
                 
                 ))
    } else {
        return(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    }
}

url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
}

procesarLocation <- function(locations){
    locations$county<-NA
    for(i in 1:nrow(locations)){
        county<-NA
        for(j in 5: ncol(locations))
            if(grepl("County",locations[i,j])){
                county<- locations[i,j]
            }
        locations[i,]$county<-county
    }
    return (locations)
}