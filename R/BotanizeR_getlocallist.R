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
#' wanted location in degrees. `lat - radius` needs to be > -90 and 
#' `lat + radius` needs to be < 90.
#' 
#' @param taxonKey a numeric, defining the taxonKey of the target taxonomic 
#' group used by [GBIF](https://www.gbif.org/) passed on to [rgbif::occ_data]. 
#' Default of 7707728 represents Tracheophyta. Use [rgbif::name_lookup] to 
#' find taxonKey of alternative taxa.
#'  
#' @param limit a numeric, defining the maximum number of occurrences retrieved 
#' from [GBIF](https://www.gbif.org/) passed on to [rgbif::occ_data]. 
#' Increasing it may significantly reduce speed.
#' 
#' @param backbone_list a data.frame with a `SPECIES` column containing the 
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
#'     R package with Shiny app to practice plant identification for 
#'     online teaching and beyond. *PLANTS, PEOPLE, PLANET*, 
#'     [https://doi.org/10.1002/ppp3.10226](https://doi.org/10.1002/ppp3.10226).
#'
#' @examples
#' # Example
#' 
#' # Species list for Germany with IDs from floraweb.de
#' data(BotanizeR_species)
#' 
#' 
#' # Load species list for Britain and Germany with species and IDs from 
#' # https://www.floraweb.de and https://www.brc.ac.uk/plantatlas/
#' data(BotanizeR_species)
#' 
#' # Subset the species list for species occurring in Cairngorms National Park
#' BotanizeR_Cairngorms <- 
#'   BotanizeR_getlocallist(long = -3.5966, lat = 57.0877, 
#'                          radius = 0.5, taxonKey = 7707728, 
#'                          limit = 10000, backbone_list = BotanizeR_species)
#' 
#' 
#' 
#' # Create a new species list for the Island of Bornholm, Denmark
#' BotanizeR_Bornholm <- 
#'   BotanizeR_getlocallist(long = 14.9184, lat = 55.1273, 
#'                          radius = 0.5, taxonKey = 7707728, 
#'                          limit = 10000, backbone_list = NA)
#' 
#' @export

BotanizeR_getlocallist <- function(long = NA, lat = NA, radius = 0.1, 
                                   taxonKey = 7707728, limit = 10000,  
                                   backbone_list = NA){
  
  # 1. Controls ----
  # Arguments
  if(!is.numeric(long)){
    stop("'long' must be a numeric >= -180 and <= 180 defining the longitude of 
         the wanted location in decimal degrees.")
  } else {
    if(long < -180 | long > 180){
      stop("'long' must be a numeric >= -180 and <= 180 defining the longitude 
           of the wanted location in decimal degrees.")
    }
  }
  
  if(!is.numeric(lat)){
    stop("'lat' must be a numeric > -90 and < 90 defining the latitude of 
         the wanted location in decimal degrees.")
  } else {
    if(lat <= -90 | lat >= 90){
      stop("'lat' must be a numeric > -90 and < 90 defining the latitude of 
           the wanted location in decimal degrees.")
    }
  }
  
  if(!is.numeric(radius)){
    stop("'radius' must be a numeric defining the radius of the buffer
         around the wanted location in degrees. 'lat' - 'radius' needs to be 
         > -90 and 'lat' + 'radius' needs to be < 90.")
  } else {
    if(lat - radius <= -90 | lat + radius >= 90){
      stop("'radius' must be a numeric defining the radius of the buffer 
           around the wanted location in degrees. 'lat' - 'radius' needs to be 
           > -90 and 'lat' + 'radius' needs to be < 90.")
    }
  }

  if(!is.numeric(taxonKey)){
    stop("'taxonKey' must be numeric representing a valid GBIF taxonKey.")
  }

  if(!is.numeric(limit)){
    stop("'limit' must be a numeric <= 100000 defining the maximum number of 
         occureences retreived from GBIF.")
  }
  
  if(!all(is.na(backbone_list))){
    if(!is.data.frame(backbone_list)){
      stop("'backbone_list' must NA or a data.frame wit a `SPECIES` column 
           containing the species to be subset.")
    }
  }
  
  # 2. Code ----
  sp_point <- sp::SpatialPoints(coords = matrix(c(long, lat), 1, 2))
  
  sp_polygon <- rgeos::gBuffer(sp_point, width = radius)
  
  sp_polygon <- rgeos::writeWKT(sp_polygon, byid = FALSE)
  
  # as.data.frame(name_lookup("Tracheophyta")$data)
  species <- rgbif::occ_data(taxonKey = taxonKey,
                             geometry = sp_polygon,
                             limit = limit)$data
  
  if(all(is.na(backbone_list))){
    
    if(!is.null(species)){
      if(length(which(species$taxonRank == "SPECIES")) > 0){
        species_count <- 
          as.data.frame.table(
            table(species$species[which(species$taxonRank == "SPECIES")]))
        names(species_count)[1] <- "species"
        species <- 
          unique(species[which(species$taxonRank == "SPECIES"),
                         c("acceptedScientificName","genus","species")])
        
        species <- dplyr::left_join(species_count, species, by = "species")
        
        species <- species[!duplicated(species$species),]
        
        names(species) <- toupper(names(species))
        names(species)[3] <- "TAXONNAME"
        species$NAMNR <- NA
        species$COUNT <- 0
        species$SCORE <- 0
        species$ATTEMPTS <- 0
        species$INCLUDE <- 1
        species <- species[,c("NAMNR", "TAXONNAME", "SPECIES", "GENUS", 
                              "COUNT", "SCORE", "ATTEMPTS", "INCLUDE", "FREQ")]
      }
    }
    
    return(species)
    
  } else {
    
    if(!is.null(species)){
      if(length(which(!is.na(species$species))) > 0){
        species <- 
          as.data.frame.table(
            table(species$species[!is.na(species$species)]))
        names(species) <- c("SPECIES","FREQ")
        species <- dplyr::inner_join(backbone_list, species, by = "SPECIES")
        return(species)
      } 
    }
    
    return(backbone_list[0, ])
    
  }
}
