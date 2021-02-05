library(BotanizeR)
# 1. Config ----

### In this config file you specify the default starting values for the BotanizeR Shiny app:

## List of species ----

species_list_selected <- c("Germany_all","Germany_winter","Germany_summer","UK&Ireland_all")[3]


## Hints and images ----
# floraweb:
image_floraweb = TRUE
hints_floraweb = c("map","description", "status", "habitat", "family",
                   "German name")
image_ukplantatlas = FALSE
# hints_ukplantatlas = c("mapuk","familyuk","ecology","statusuk","trends",
#                        "perennation","lifeform","woodiness","clonality")
hints_ukplantatlas = NULL

image_folders = c("www/pictures_gehoelze_winter_500")
# image_folders = c("~/ShinyApps/BotanizeR/WWW/pictures_Clemens_400", 
# "~/ShinyApps/BotanizeR/WWW/drawings_Schulz_400")
hints_custom = c("ownhint_1","COMMONNAME") # column names of own hints to check at start
hints_custom_omit <- c("NAMNR","TAXONNAME","SPECIES","GENUS","EPITHET","AUTHOR",  
                       "NATIVE","SUMMER","WINTER","BioDiv2005","COUNT","SCORE",
                       "ATTEMPTS","INCLUDE", "imagelink_1","imagelink_2")

chorology = "chorology"
chorology = NULL

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
