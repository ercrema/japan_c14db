#' @title Extracts Geographic Coordinates From Address using Google API
#' @param x address to be geocoded
#' @pama api Google API code
#' @example
#' x <- c("群馬県前橋市三河町1-9-18","群馬県前橋市三河町1-9-18","東京都文京区後楽1-3-61","群馬県前橋市三河町1-9-18")
#' locations<-extractCoordinates(x)

extractCoordinates <- function(x,api="AIzaSyDnAh9C2tYLaixm9x2D2h-gp35E1CPm5Vo")
{
  # Set Up
  require(ggmap)
  register_google(key = api)
  
  # Search only unique addresses
  uniqueX = unique(x)
  if (any(is.na(uniqueX)))
  { uniqueX=uniqueX[-which(is.na(uniqueX))]}
  if(length(uniqueX)>0)
  {
    coord<-geocode(uniqueX)
    coord<-as.data.frame(coord)
    
    # Match Entry
    coordinates <- data.frame(address=x,lat=numeric(length=length(x)),lon=numeric(length=length(x)))
    coordinates$lat=NA
    coordinates$lon=NA
    
    for (i in 1:length(uniqueX))
    {
      k=which(x==uniqueX[i])
      coordinates$lon[k] = coord$lon[i]
      coordinates$lat[k] = coord$lat[i]
    }
    
  } else {
    coordinates <- data.frame(address=x,lat=NA,lon=NA)
  }
  return(coordinates)
  
}
