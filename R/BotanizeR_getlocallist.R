
BotanizeR_getlocallist <- function(long = NA, lat = NA, radius = 1,
                                   backbone_list = NA){
  
  # 1. Controls ----
  # Package dependencies
  library(sp)
  library(rgeos)
  library(rgbif)
  library(dplyr)
  
  # Arguments

  # 2. Code ----
  #lat	= 51.545483
  #long = 9.905548
  
  sp_point <- sp::SpatialPoints(coords = matrix(c(long, lat), 1, 2))
  
  sp_polygon <- rgeos::gBuffer(sp_point, width = radius)
  #plot(sp_polygon)
  
  sp_polygon <- rgeos::writeWKT(sp_polygon, byid = FALSE)
  
  # as.data.frame(name_lookup("Tracheophyta")$data)
  species <- rgbif::occ_data(taxonKey = 7707728,
                             geometry = sp_polygon)$data$species
  
  species <- as.data.frame.table(table(species))
  names(species) <- toupper(names(species))
  
  if(all(is.na(backbone_list))){
    return(species)
  } else {
    species <- inner_join(backbone_list, species, by = "SPECIES")
    return(species)
  }
}



 
