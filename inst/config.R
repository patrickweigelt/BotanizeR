library(BotanizeR)
# 1. Config ----

### In this config file you specify the default starting values for the BotanizeR Shiny app:


# Shall setup tab be shown?
setup = TRUE

# Shall online ressources in setup tab be shown
online_ressources = TRUE

### System ----
system_path <- ""
#system_path <- "~/ShinyApps/BotanizeR/"

### List of species ----
species_list_path <- "BotanizeR_Indonesia.csv"
species_list_path <- ""

species_list_filter <- "All species" # If set to "All species" the only option shown will be "All species" 
species_list_filter <- c("All species","Germany","Germany_winter","Germany_summer","UK_Ireland","UK_Ireland_Sussex")
species_list_filter <- c("Germany","Germany_BioDiv","Germany_winter","Germany_summer","UK_Ireland","UK_Ireland_Sussex")

species_list_selected <- "All species" # If set to "All species" no filters will be applied
species_list_selected <- "UK_Ireland_Sussex"


### Hints and images ----
# floraweb:
image_floraweb = TRUE
image_ukplantatlas = TRUE

hints_floraweb = NULL
hints_floraweb = c("map","description", "status", "habitat", "family",
                   "German name")

hints_ukplantatlas = NULL
# hints_ukplantatlas = c("mapuk","familyuk","ecology","statusuk","trends",
#                        "perennation","lifeform","woodiness","clonality")

image_folders = NULL
# image_folders = c("WWW/pictures_gehoelze_winter_500")
# image_folders = c("WWW/pictures_Clemens_400","WWW/drawings_Schulz_400")

hints_custom = NULL
hints_custom = c("ownhint_English_name") # column names of own hints to check at start

imagelinks_custom = NULL
# imagelinks_custom = c("imagelink_1")

chorology = NULL
# chorology = "chorology"


### Winter:
# species_list_path <- "floraweb_species_winter.csv" # for winter list
# 
# # hints and images
# floraweb:
# hints_floraweb =  c("German name", "family", "status") # for winter list
# # hints_floraweb =  NULL # for winter list
# 
# # image_folders = c("www/pictures_Clemens_400", "www/drawings_Schulz_400")
# image_folders = c("WWW/pictures_gehoelze_winter_560", 
#                  "WWW/drawings_Schulz_560")
# hints_custom = NULL
# chorology = "Chorology"
