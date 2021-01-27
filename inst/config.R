library(BotanizeR)
# 1. Config ----

### Default:
# List of species
data(floraweb_species)
species_list <- floraweb_species[which(floraweb_species$SUMMER==1 |
                                         floraweb_species$BioDiv2005==1), ]
## hints and images
# floraweb:
image_floraweb = TRUE
hints_floraweb = c("map","description", "status", "habitat", "family",
                   "German name")
image_folders = c("www/pictures_Clemens_400", "www/drawings_Schulz_400")
# image_folders = c("~/ShinyApps/BotanizeR/WWW/pictures_Clemens_400", 
# "~/ShinyApps/BotanizeR/WWW/drawings_Schulz_400")
hints_custom = NULL
chorology = "Chorology"

### Winter:
# species_list <- read.csv("floraweb_species_winter.csv") # for winter list

## hints and images
# floraweb:
# image_floraweb = FALSE # for winter list
# hints_floraweb =  c("German name", "family", "status") # for winter list
# # hints_floraweb =  NULL # for winter list

# # image_folders = c("www/pictures_Clemens_400", "www/drawings_Schulz_400")
# image_folders = c("~/ShinyApps/BotanizeR/WWW/pictures_Clemens_400", 
# "~/ShinyApps/BotanizeR/WWW/drawings_Schulz_400") # This is needed on server
# hints_custom = NULL
# chorology = "Chorology"
