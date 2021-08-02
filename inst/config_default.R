################################################################################
### The config_default.R and config.R files are part of the BotanizeR shiny app.
### config_default.R contains the default values to specify which species lists 
### and resources to use and what setup options to make available in the app. 
### Changes to the default configuration should be made in config.R. Here not 
### variables need to be defined but only those deviating from the default.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 1. Setup ----

### Shall setup tab be shown in the app?

# Set to FALSE if setup tab shall be hidden.

setup <- TRUE 


### Shall online resources in setup tab be shown?

# Set to FALSE if content from online resources like www.floraweb.de or  
# https://www.brc.ac.uk/plantatlas/ shall be removed from setup tab.

online_resources <- TRUE 


### Shall subsetting by GBIF occurrences be available?

# Set to FALSE if the option to subset the provided backbone species lists by 
# gbif occurrences for a user-defined radius around user-defined coordinates
# shall not be available in the setup tab.

gbif <- TRUE


### Dynamic species sampling probabilities

# Set to FALSE if species in the quiz shall be drawn with equal probabilities 
# not updated depending on practicing successes and failures.

dynamic_probabilities <- TRUE


### Google analytics

# Put your Google analytics ID as character string (e.g. "UX-123456789-1") if 
# you want to evaluate access to your BotanizeR instance. 

analytics <- FALSE


### Twitter

# Provide the URL of your BotanizeR instance 
# (e.g. "https://gift.uni-goettingen.de/shiny/BotanizeR/") to be included in 
# twitter tweets produced from inside the app

BotanizeR_URL <- ""


### System path

# If your setup requires to add a folder location to all relative paths used in 
# the app (species list location, image locations), add it as a character 
# string here (e.g. "~/ShinyApps/BotanizeR/")

system_path <- ""


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 2. Species list ----

### File

# Provide a csv file (system_path defined above will be added in front) of the 
# backbone species list used in BotanizeR (e.g. "species_list.csv"). If not 
# supplied (species_list_path <- ""), BotanizeR_species.rda from the BotanizeR 
# R package including a comprehensive species list of Germany and Great Britain 
# will be loaded.

species_list_path <- ""


### Species list filters

# Name columns by which the backbone species list can be filtered. These columns
# need to be numeric including 0 and 1 indicating whether or not to include a 
# species in the given subset (e.g. c("Region_A","Orchidaceae")). If set to 
# "All species" the only option shown in the species list drop down of the setup
# tab will be "All species" and no filtering will be possible.

species_list_filter <- "All species"
# species_list_filter <- c("All species","Germany","Germany_BioDiv",
#                          "UK_Ireland","UK_Ireland_Sussex")


### Species list

# Define species list selected at the start (Needs to be one out of 
# species_list_filter), If set to "All species" no filter will be applied.

species_list_selected <- "All species" 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 3. Hints and images ----

### Images from online resources

# Indicate whether images from www.floraweb.de and/or 
# https://www.brc.ac.uk/plantatlas/ shall be shown from the start. If set to 
# FALSE but setup and online_resources are set to TRUE above, the online
# resources can be activated by the user within the setup tab.

image_floraweb <- TRUE
image_ukplantatlas <- TRUE


### Images from folder

# Provide the path to folders including images of the target species to show
# in the BotanizeR app at the start as a character string ( e.g. 
# c("WWW/pictures_Angiosperms","WWW/drawings_Region_A") ). The system_path 
# defined above will be added in front). The images inside the folder need to 
# include the species names in their file names using " " or "_" to delimit 
# genus and epithet.

image_folders <- NULL


### Images from image link columns

# Indicate a column in the species list with URLs of images online to be shown
# in BotanizeR at the start. Column names need to be named like imagelink_1, 
# imagelink_2 etc. and can be enabled and disabled in the setup tab if setup is 
# set to TRUE above.

imagelinks_custom <- NULL
# imagelinks_custom <- c("imagelink_1", "imagelink_2")


### Hints from online resources

# Indicate which of the hints available from www.floraweb.de and/or 
# https://www.brc.ac.uk/plantatlas/ shall be shown from the start. If set to 
# NULL but setup and online_resources are set to TRUE above, the online hints
# can be activated by the user within the setup tab.


hints_floraweb <- NULL
#hints_floraweb <- c("map","description", "status", "habitat", "family",
#                   "German name")

# hints_ukplantatlas <- NULL
hints_ukplantatlas <- c("mapuk","familyuk","ecology","statusuk","trends",
                       "perennation","lifeform","woodiness","clonality")


### Hints from own hint columns

# Indicate columns with descriptive hints that shall be shown from the start. 
# Column names need to be named like ownhint_HintName. If set to NULL but setup
# is to TRUE above, the hints can be activated by the user within the setup tab.

hints_custom <- NULL
# hints_custom <- c("ownhint_English_name")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 3. About ----

# Add text (html) describing your instance of BotanizeR and your sources of
# images, hints and species lists to be shown in addition to our general 
# description of the app in the about tab. For more fundcamental changes,
# modify about.R directly.

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
  to show.<br>
  In addition to the complete floras (UK_Ireland & Germany), you can 
  choose to practice species from a list of about 700 plants known from Sussex, 
  UK (UK_Ireland_Sussex), a list of 174 species common in forests and 
  grasslands around G&ouml;ttingen, Germany (Germany_BioDiv), a list of 128 
  woody species from Germany with additional images available in winter state 
  (Germany_winter), and a list of 214 species of forest indicator species from 
  Germany (Germany_summer).<br>
  In the settings tab, you can enable and disable images and hints from the 
  <a href="https://www.brc.ac.uk/plantatlas/" target=_blank>
  UK & Ireland Plant Atlas</a> or 
  <a href="https://www.floraweb.de/" target=_blank>FloraWeb</a> as well as 
  images from a local folder on the server including images in winter state 
  as an example of images provided by the host of the app. You can also enable 
  or disable the English and French common names as examples of hints 
  provided directly via the species list table by the host of the app or by the 
  user.' 
  

instance_credits <- 
  'This instance of BotanizeR retrieves images and information from 
  <a href="https://www.floraweb.de/" target=_blank>FloraWeb</a> and the 
  <a href="https://www.brc.ac.uk/plantatlas/" target=_blank>
  UK & Ireland Plant Atlas</a>. 
  Please visit these websites for more informationan about sources and 
  image authors. 
  <br>
  <br>
  In addition, images of plants in winter state have been 
  provided by F. Brambach, C. D&ouml;nges, H. Kreft, J. Kuper and 
  H. Reichelt from 
  <a href="https://www.uni-goettingen.de/en/128741.html" target=_blank>
  Biodiversity, Macroecology and Biogeography</a> 
  which are licensed under Creative Commons Attribution-ShareAlike 4.0 
  International License (
  <a href="http://creativecommons.org/licenses/by-sa/4.0" target=_blank>
  CC BY-SA 4.0</a>
  ).'


