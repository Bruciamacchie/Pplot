librarian::shelf(tidyverse, sf)


pts <- st_read("/Users/maxm1/pCloudSync/Forets/StBarbe/Photos.gpkg") |> 
  st_zm() |> 
  st_transform(4326) |> 
  dplyr::select(Num) |> 
  rename(name = Num)

nom="gpxfile.gpx"    
st_write(pts, dsn=nom, layer="waypoints", driver="GPX", append=T, delete_dsn=T, quiet =T)
