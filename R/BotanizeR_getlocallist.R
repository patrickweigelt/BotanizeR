#' Get local species list
#'
#' Using longitude and latitude coordinates and a buffer radius, this function 
#' retrieves a plant species list from [GBIF](https://www.gbif.org/) and 
#' formats it for use in [BotanizeR::BotanizeR_quiz()] or the BotanizeR Shiny 
#' app. If a `backbone_list` is supplied, it will be subset for the species 
#' found in [GBIF](https://www.gbif.org/) and returned.
#'
#' @param long a numeric, defining the longitude of the wanted location in 
#' decimal degrees.
#' 
#' @param lat a numeric, defining the latitude of the wanted location in 
#' decimal degrees.
#' 
#' @param radius a numeric, defining the radius of the buffer around the
#' wanted location in degrees.
#' 
#' @param backbone_list a data.frame with `SPECIES` column containing the 
#' species to be subset. If this list shall be used as `species_list` in 
#' [BotanizeR::BotanizeR_quiz()], It needs to contain at least the following 
#' columns: *NAMNR*, *TAXONNAME*, *SPECIES* and *GENUS*. See 
#' [BotanizeR::BotanizeR_quiz()] for more info.
#' 
#' @return
#' A `data.frame` of the same structure like `backbone_list` including only 
#' those species found in [GBIF](https://www.gbif.org/) for the given 
#' coordinates and radius. An additional column `FREQ`is added providing the 
#' frequency of each for selecting more common species. If `backbone_list` is 
#' `NA` the species list obtained from [GBIF](https://www.gbif.org/) is 
#' formatted to contain all essential columns to run 
#' [BotanizeR::BotanizeR_quiz()] or to use it in the BotanizeR Shiny app.
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
    stop("'long' must be a numeric > -180 and < 180 defining the longitude of 
         the wanted location in decimal degrees.")
  }
  
  if(!is.numeric(lat)){
    stop("'lat' must be a numeric > -90 and < 90 defining the latitude of 
         the wanted location in decimal degrees.")
  }
  
  if(!is.numeric(radius)){
    stop("'radius' must be a numeric defining to the radius of the buffer
         around the wanted location in degrees.")
  }
  
  if(!is.na(backbone_list)){
    if(!is.data.frame(backbone_list)){
      stop("'backbone_list' must be a data.frame with the same columns as
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
