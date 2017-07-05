library(XML)
library(RCurl)
distance2Points <- function(origin,destination){
  origin=gsub(" ", "+", origin)
  destination=gsub(" ", "+", destination)
  results <- list();
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  time <- xmlValue(xmlChildren(xpathApply(xmlfile,"//duration")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  time <- as.numeric(time)/60
  distance <- distance/1000
  #results[['time']] <- time
  results <- distance
  return(results)
}

dis <- distance2Points("Sangli",AgroMarket("Latur", 10, 1900, 2950)$Place)
dis
district <- c("Sangli","Satara","Solapur","Kolhapur")

fun <- function(){
  z <- 1
   return(get(z))
}

fun
