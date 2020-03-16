checkPrefecture <- function(lat,lon,pref)
{
  require(revgeo)
  require(dplyr)
  require(rgdal)
  jwgs84 = readOGR(dsn="./gis",layer='japan_wgs84')
  jjgd2000 = readOGR(dsn="./gis",layer='japan_jgd2000')
  
  lat[which(is.na(lat))]=0
  lon[which(is.na(lon))]=0
  
  tmp.input=data.frame(lat=lat,lon=lon,pref=pref,stringsAsFactors=FALSE)
  comb <- do.call(paste, c(as.list(tmp.input[c("lat","lon","pref")]), sep = "."))
  tmp.input$id <- match(comb, unique(comb))
  
  tmp.unique = unique(tmp.input)
  site_wgs84 = tmp.unique
  site_jgd2000 = tmp.unique
  coordinates(site_wgs84) <- c("lon","lat")
  coordinates(site_jgd2000) <- c("lon","lat")
  proj4string(site_wgs84) <- proj4string(jwgs84)
  proj4string(site_jgd2000) <- proj4string(jjgd2000)
  
  tmp.unique$extractedWGS84=over(site_wgs84,jwgs84)$prefecture
  tmp.unique$extractedJGD2000=over(site_jgd2000,jjgd2000)$prefecture
  
  tmp.unique$check=(tmp.unique$pref==tmp.unique$extractedWGS84)& (tmp.unique$pref==tmp.unique$extractedJGD2000)
  tmp.input=left_join(tmp.input,tmp.unique,by='id')
  return(return(tmp.input$check))
}
