# 1. Config ----

### In this config file you specify the default starting values for the BotanizeR Shiny app:

### TODO split up default vs user config into two files

# ~~~~~~~~~~~~~~

# Shall setup tab be shown?
setup <- TRUE

# Shall online ressources in setup tab be shown
online_ressources <- TRUE
# online_ressources <- FALSE

# Shall subsetting by local GBIF occurrences be shown?
gbif <- TRUE

# Shall species be drawn using probabilities updated depending on practicing success or with uniform probabilities?
dynamic_probabilities <- TRUE

### Include Google analytics
# Default
analytics <- FALSE
# Put your Google analytics ID
analytics <<- "UA-97979680-1"

### URL for Twitter
BotanizeR_URL <- ""
BotanizeR_URL <- "https://gift.uni-goettingen.de/shiny/BotanizeR/"
#BotanizeR_URL <- "https://gift.uni-goettingen.de/shiny/BotanizeR_winter/"
#BotanizeR_URL <- "https://gift.uni-goettingen.de/shiny/BotanizeR_Indonesia/"
#BotanizeR_URL <- "https://gift.uni-goettingen.de/shiny/BotanizeR_summer/"


### System ----
system_path <- ""
#system_path <- "~/ShinyApps/BotanizeR/"

### List of species ----
species_list_path <- ""
# species_list_path <- "Indonesia_species.csv"
# species_list_path <- "species_summer.csv"

species_list_filter <- "All species" # If set to "All species" the only option shown will be "All species" 
# species_list_filter <- c("All species","Germany","Germany_winter","Germany_summer","UK_Ireland","UK_Ireland_Sussex")
species_list_filter <- c("Germany","Germany_BioDiv","Germany_winter","Germany_summer","UK_Ireland","UK_Ireland_Sussex")
# species_list_filter <- c("Lore_Lindu_Sulawesi","Roadside_Sumatra")

species_list_selected <- "All species" # If set to "All species" no filters will be applied
species_list_selected <- "UK_Ireland_Sussex"
# species_list_selected <- "Lore_Lindu_Sulawesi"


### Hints and images ----
# floraweb:
image_floraweb <- FALSE
image_ukplantatlas <- FALSE
image_floraweb <- TRUE
image_ukplantatlas <- TRUE

hints_floraweb <- NULL
#hints_floraweb <- c("map","description", "status", "habitat", "family",
#                   "German name")
#hints_floraweb <- c("map","description", "status", "habitat")

hints_ukplantatlas <- NULL
hints_ukplantatlas <- c("mapuk","familyuk","ecology","statusuk","trends",
                       "perennation","lifeform","woodiness","clonality")

image_folders <- NULL
# image_folders <- c("WWW/pictures_gehoelze_winter_500")
# image_folders <- c("WWW/pictures_Clemens_400","WWW/drawings_Schulz_400")
# image_folders <- c("WWW/pictures_Trees_LoreLinduNP_Sulawesi_700")

hints_custom <- NULL
hints_custom <- c("ownhint_English_name") # column names of own hints to check at start
# hints_custom <- c("ownhint_Family","ownhint_Habit","ownhint_Elevation","ownhint_Distribution") # column names of own hints to check at start
#hints_custom <- c("ownhint_Gruppe", "ownhint_Familie", "ownhint_Deutscher_Name", "ownhint_Zeigerwert", "ownhint_Bluetezeit")

imagelinks_custom <- NULL
# imagelinks_custom <- c("imagelink_1")

chorology <- NULL
# chorology <- "chorology"


instance_description <- 
  'This particular instance of the BotanizeR Shiny app exemplifies a few 
  particular use cases. Here, you can practice species from the Floras 
  of Germany, Britain and Ireland based on information retrieved live 
  from the websites of 
  <a href="https://www.floraweb.de/" target=_blank>FloraWeb</a> 
  and the 
  <a href="https://www.brc.ac.uk/plantatlas/" target=_blank>
  UK & Ireland Plant Atlas</a>. 
  You can choose which of the Floras to practice species from, which 
  species subset to use and which information from each of the websites 
  to show. In addition, you can choose to include images of woody 
  plants of Germany in winter state as an example of images provided by 
  the host of the app or the French common name as an example of a hint 
  provided by the host of the app. The app starts with a list of 314 
  common or characteristic species from Central Germany, including 
  images from all three resources mentioned above. In the settings tab 
  you can switch to the entire Flora of Britain and Ireland including 
  only images and hints from the 
  <a href="https://www.brc.ac.uk/plantatlas/" target=_blank>
  UK & Ireland Plant Atlas</a> 
  or to a list of 128 woody species from Germany with images available 
  in winter state. If you disable other image sources you can practice 
  these species only based on their characteristics as found during the 
  winter months.'

instance_credits <- 
  'This instance of BotanizeR retrieves images and information from 
  <a href="https://www.floraweb.de/" target=_blank>FloraWeb</a> and the 
  <a href="https://www.brc.ac.uk/plantatlas/" target=_blank>
  UK & Ireland Plant Atlas</a>. 
  Please visit these websites for more informationan about sources and 
  image authors. 
  <br>
  <br>
  In addition, images of plants in summer and winter state have been 
  provided by F. Brambach, C. D&ouml;nges, H. Kreft, J. Kuper and 
  H. Reichelt from 
  <a href="https://www.uni-goettingen.de/en/128741.html" target=_blank>
  Biodiversity, Macroecology and Biogeography</a> 
  which are licensed under Creative Commons Attribution-ShareAlike 4.0 
  International License (
  <a href="http://creativecommons.org/licenses/by-sa/4.0" target=_blank>
  CC BY-SA 4.0</a>
  )'


### Winter:
# species_list_path <- "floraweb_species_winter.csv" # for winter list
# 
# # hints and images
# floraweb:
# hints_floraweb <-  c("German name", "family", "status") # for winter list
# # hints_floraweb <-  NULL # for winter list
# 
# # image_folders <- c("www/pictures_Clemens_400", "www/drawings_Schulz_400")
# image_folders <- c("WWW/pictures_gehoelze_winter_560", 
#                  "WWW/drawings_Schulz_560")
# hints_custom <- NULL
# chorology <- "Chorology"
