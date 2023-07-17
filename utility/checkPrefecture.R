checkPrefecture <- function(lat,lon,pref,map)
{
  require(revgeo)
  require(dplyr)
  require(rnaturalearth)
  require(sf)
  
  
  lat[which(is.na(lat))]=0
  lon[which(is.na(lon))]=0
  
  tmp.input=data.frame(lat=lat,lon=lon,pref=pref,stringsAsFactors=FALSE)
  comb <- do.call(paste, c(as.list(tmp.input[c("lat","lon","pref")]), sep = "."))
  tmp.input$id <- match(comb, unique(comb))
  tmp.unique = unique(tmp.input)
  row.names(tmp.unique)=NULL
  
  # Control routine for handling floating point nightmare
  if (anyDuplicated(tmp.unique$id))
  {
    ids = as.numeric(names(which(table(tmp.unique$id)>1)))
    for (i in 1:length(ids))
    {
      tmp.index = which(tmp.unique$id==ids[i])
      if (diff(tmp.unique$lat[tmp.index])<0.00001)
      {
        tmp.unique$lat[tmp.index]=tmp.unique$lat[tmp.index[1]]
      }
      if (diff(tmp.unique$lon[tmp.index])<0.00001)
      {
        tmp.unique$lon[tmp.index]=tmp.unique$lon[tmp.index[1]]
      }
      if (diff(tmp.unique$lon[tmp.index])>0.0001|diff(tmp.unique$lat[tmp.index]))
      {
        stop('Processing error - check source code')
      }
    }
  }

  tmp.unique = unique(tmp.unique)
  tmp.unique = st_as_sf(tmp.unique,coords=c('lon','lat'),crs=4326)
  extracted = st_intersects(tmp.unique,map)
  bin = lapply(extracted,length) |> unlist()
  iii = which(bin==1)
  tmp.unique$name = NA
  tmp.unique$name[iii] = map$pref[unlist(extracted)]
  tmp.unique$check = (tmp.unique$pref==tmp.unique$name)
  tmp.unique$check[which(is.na(tmp.unique$name))]  <- NA
  tmp.input=left_join(tmp.input,tmp.unique,by='id')
  return(return(tmp.input$check))
}
