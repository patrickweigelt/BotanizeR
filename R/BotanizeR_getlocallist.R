#' GBIF species list
#'
#' Using coordinates and a buffer radius, this function retrieves a certain
#' species list from [GBIF](https://www.gbif.org/) database.
#'
#' @param long a numeric, defining the longitude of the wanted location.
#' 
#' @param lat a numeric, defining the latitude of the wanted location.
#' 
#' @param radius a numeric, defining the radius of the buffer around the
#' wanted location.
#' 
#' @param backbone_list a data.frame with the same columns as the data
#' returned by data("BotanizeR_species").
#' 
#' @return
#' A species list.
#'
#' @references
#'     Weigelt, P., Denelle, P., Brambach, F. & Kreft, H. (2021) A flexible
#'     R-package with Shiny-App for practicing plant identification in times of
#'     online teaching and beyond. submitted.
#'
#' @examples
#' # Example
#' 
#' @export

BotanizeR_getlocallist <- function(long = NA, lat = NA, radius = 1,
                                   backbone_list = NA){
  
  # 1. Controls ----
  # Arguments
  if(!is.numeric(long)){
    stop("'long' must be a numeric corresponding to the longitude of the wanted
         location.")
  }
  
  if(!is.numeric(lat)){
    stop("'lat' must be a numeric corresponding to the latitude of the wanted
         location.")
  }
  
  if(!is.numeric(radius)){
    stop("'radius' must be a numeric corresponding to the radius of the buffer
         around the wanted location.")
  }
  
  if(!is.na(backbone_list)){
    if(!is.data.frame(backbone_list)){
      stop("'backbone_list' must be a data.frame with the same columns than
           the data.frame returned by data('BotanizeR_species').")
    }
  }
  
  # 2. Code ----
  sp_point <- sp::SpatialPoints(coords = matrix(c(long, lat), 1, 2))
  
  sp_polygon <- rgeos::gBuffer(sp_point, width = radius)
  
  sp_polygon <- rgeos::writeWKT(sp_polygon, byid = FALSE)
  
  # as.data.frame(name_lookup("Tracheophyta")$data)
  species <- rgbif::occ_data(taxonKey = 7707728,
                             geometry = sp_polygon)$data$species
  
  species <- as.data.frame.table(table(species))
  names(species) <- toupper(names(species))
  
  if(all(is.na(backbone_list))){
    return(species)
  } else {
    if(nrow(species) > 0){
      species <- dplyr::inner_join(backbone_list, species, by = "SPECIES")
      return(species)
    } else {
      return(backbone_list[0, ])
    }
  }
}
