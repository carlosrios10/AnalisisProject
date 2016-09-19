# Geocoding script for large list of addresses.
# Shane Lynn 10/10/2013
#load up the ggmap library
library("ggmap")
# get the input data
infile <- 'NYC-Venues'
data <- read.csv(paste0( infile, '.csv'),sep ='\t',colClasses = 'character')
data$lat <- as.numeric(data$lat)
data$lon <- as.numeric(data$lon)
str(data)
# get the address list, and append "Ireland" to the end to increase accuracy 
# (change or remove this if your address already include a country etc.)
addresses = data
data[920:924,c('lon','lat')]
#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
    #use the gecode function to query google servers
    geo_reply <- revgeocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    #now extract the bits that we need from the returned list
    answer <- data.frame(    lat =NA,
                             lon =NA,
                             location_type =NA,
                             formatted_address =NA,
                             address_components1_LN=NA, 
                             address_components1_SN =NA,
                             address_components1_type =NA,
                             address_components2_LN =NA,
                             address_components2_SN =NA,
                             address_components2_type =NA,
                             address_components3_LN =NA,
                             address_components3_SN =NA,
                             address_components3_type =NA,
                             address_components4_LN =NA,
                             address_components4_SN =NA,
                             address_components4_type =NA,
                             address_components5_LN =NA,
                             address_components5_SN =NA,
                             address_components5_type =NA,
                             address_components6_LN =NA,
                             address_components6_SN =NA,
                             address_components6_type =NA,
                             address_components7_LN =NA,
                             address_components7_SN =NA,
                             address_components7_type =NA,
                             address_components8_LN =NA,
                             address_components8_SN =NA,
                             address_components8_type =NA)
                             
    answer$status <- geo_reply$status
    
    #if we are over the query limit - want to pause for an hour
    while(geo_reply$status == "OVER_QUERY_LIMIT"){
        print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
        time <- Sys.time()
        print(as.character(time))
        Sys.sleep(60*60)
        geo_reply = revgeocode(address, output='all', messaging=TRUE, override_limit=TRUE)
        answer$status <- geo_reply$status
    }
    
    #return Na's if we didn't get a match:
    if (geo_reply$status != "OK"){
        return(answer)
    }   
    #else, extract what we need from the Google server reply into a dataframe:
    answer$lat <- tryCatch({geo_reply$results[[1]]$geometry$location$lat},error=function(err){return (NA)})
    answer$lon <- tryCatch({geo_reply$results[[1]]$geometry$location$lng},error=function(err){return (NA)})
    answer$location_type <- tryCatch({geo_reply$results[[1]]$geometry$location_type},error=function(err){return (NA)})
    answer$formatted_address <- tryCatch({geo_reply$results[[1]]$formatted_address},error=function(err){return (NA)})
    answer$address_components1_LN <-tryCatch({geo_reply$results[[1]]$address_components[[1]]$long_name},error=function(err){return (NA)})
    answer$address_components1_SN <-tryCatch({geo_reply$results[[1]]$address_components[[1]]$short_name},error=function(err){return (NA)})
    answer$address_components1_type <-tryCatch({paste(geo_reply$results[[1]]$address_components[[1]]$types,collapse=",")},error=function(err){return (NA)})
    answer$address_components2_LN <-tryCatch({geo_reply$results[[1]]$address_components[[2]]$long_name},error=function(err){return (NA)})
    answer$address_components2_SN <-tryCatch({geo_reply$results[[1]]$address_components[[2]]$short_name},error=function(err){return (NA)})
    answer$address_components2_type <-tryCatch({paste(geo_reply$results[[1]]$address_components[[2]]$types,collapse=",")},error=function(err){return (NA)})
    answer$address_components3_LN <-tryCatch({geo_reply$results[[1]]$address_components[[3]]$long_name},error=function(err){return (NA)})
    answer$address_components3_SN <-tryCatch({geo_reply$results[[1]]$address_components[[3]]$short_name},error=function(err){return (NA)})
    answer$address_components3_type <-tryCatch({paste(geo_reply$results[[1]]$address_components[[3]]$types,collapse=",")},error=function(err){return (NA)})
    answer$address_components4_LN <-tryCatch({geo_reply$results[[1]]$address_components[[4]]$long_name},error=function(err){return (NA)})
    answer$address_components4_SN <-tryCatch({geo_reply$results[[1]]$address_components[[4]]$short_name},error=function(err){return (NA)})
    answer$address_components4_type <-tryCatch({paste(geo_reply$results[[1]]$address_components[[4]]$types,collapse=",")},error=function(err){return (NA)})
    answer$address_components5_LN <-tryCatch({geo_reply$results[[1]]$address_components[[5]]$long_name},error=function(err){return (NA)})
    answer$address_components5_SN <-tryCatch({geo_reply$results[[1]]$address_components[[5]]$short_name},error=function(err){return (NA)})
    answer$address_components5_type <-tryCatch({paste(geo_reply$results[[1]]$address_components[[5]]$types,collapse=",")},error=function(err){return (NA)})
    answer$address_components6_LN <-tryCatch({geo_reply$results[[1]]$address_components[[6]]$long_name},error=function(err){return (NA)})
    answer$address_components6_SN <-tryCatch({geo_reply$results[[1]]$address_components[[6]]$short_name},error=function(err){return (NA)})
    answer$address_components6_type <-tryCatch({paste(geo_reply$results[[1]]$address_components[[6]]$types,collapse=",")},error=function(err){return (NA)})
    answer$address_components7_LN <-tryCatch({geo_reply$results[[1]]$address_components[[7]]$long_name},error=function(err){return (NA)})
    answer$address_components7_SN <-tryCatch({geo_reply$results[[1]]$address_components[[7]]$short_name},error=function(err){return (NA)})
    answer$address_components7_type <-tryCatch({paste(geo_reply$results[[1]]$address_components[[7]]$types,collapse=",")},error=function(err){return (NA)})
    answer$address_components8_LN <-tryCatch({geo_reply$results[[1]]$address_components[[8]]$long_name},error=function(err){return (NA)})
    answer$address_components8_SN <-tryCatch({geo_reply$results[[1]]$address_components[[8]]$short_name},error=function(err){return (NA)})
    answer$address_components8_type <-tryCatch({paste(geo_reply$results[[1]]$address_components[[8]]$types,collapse=",")},error=function(err){return (NA)})
    
    return(answer)
    
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
    print("Found temp file - resuming from index:")
    geocoded <- readRDS(tempfilename)
    startindex <- nrow(geocoded)
    print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, nrow(addresses))){
    print(paste("Working on index", ii, "of", nrow(addresses)))
    #query the google geocoder - this will pause here if we are over the limit.
    result = getGeoDetails(c(addresses[ii,c('lon')],addresses[ii,c('lat')])) 
    print(result$status)     
    result$index <- ii
    result$venueId <- addresses$venueId[ii]
    result$numVenueId <- addresses$numVenueId[ii]
    #append the answer to the results file.
    geocoded <- rbind(geocoded, result)
    #save temporary results as we are going along
    saveRDS(geocoded, tempfilename)
}

#now we add the latitude and longitude to the main data
data_gecoded <- merge(addresses,geocoded,by.x=c('venueId'),by.y=c('venueId'),all.x=T)

#finally write it all to the output files
saveRDS(data_gecoded, paste0( infile ,"_geocoded.rds"))
write.table(data_gecoded, file=paste0( infile ,"_geocoded.csv"), sep=",", row.names=FALSE)