# try
devtools::load_all()

# Species list for Germany with IDs from floraweb.de
data(BotanizeR_species)

# subset for nearly 200 species forestry students in Göttingen learn.
BotanizeR_species <- BotanizeR_species[which(BotanizeR_species$Germany_summer==1 | BotanizeR_species$Germany_BioDiv==1),]

# Type in species name, or press enter for next hint or type "skip" and press 
# enter for next species or type "exit" to end quiz and save results
floraweb_species_trained <- BotanizeR_quiz(species_list = BotanizeR_species, image_floraweb=TRUE,
                            hints_floraweb = c("map","description","status","habitat","family","German name"),
                            case_sensitive = FALSE)


# write.csv(BotanizeR_species, "BotanizeR_species_PW.csv", row.names = FALSE)


floraweb_species_trained <- read.csv("floraweb_species_trained.csv")

floraweb_species_trained <- BotanizeR_quiz(species_list = floraweb_species_trained,
                                           hints_floraweb =  c("description","status","habitat","family",
                                                     "German name"), case_sensitive = FALSE)

write.csv(floraweb_species_trained, "floraweb_species_trained.csv", row.names = FALSE)





species_trained <- BotanizeR_quiz(species_list = floraweb_species, image_floraweb=TRUE,
                                         hints_floraweb = NULL,
                                         hints_custom = NULL, 
                                         imagelinks_custom = NULL, 
                                         image_folders = "inst/WWW/pictures_Clemens",
                                         case_sensitive = FALSE)



data(floraweb_species)

### example for three species with custom hints and images
custom_species <- floraweb_species[which(floraweb_species$SPECIES %in% c("Acer campestre","Erica carnea","Melampyrum nemorosum","Veronica chamaedrys")),]

custom_species_trained <- BotanizeR_quiz(species_list = custom_species, image_floraweb=TRUE,
                                         hints_floraweb = NULL,
                                         hints_custom = c("ownhint_1","ownhint_2"), 
                                         imagelinks_custom = c("imagelink_1","imagelink_2"), 
                                         image_folders = NULL, case_sensitive = FALSE)

custom_species_trained <- BotanizeR_quiz(species_list = custom_species, image_floraweb=TRUE,
                                         hints_floraweb = NULL,
                                         hints_custom = c("ownhint_1","ownhint_2"), 
                                         imagelinks_custom = c("imagelink_1","imagelink_2"), 
                                         image_folders = "inst/WWW/pictures_Clemens", case_sensitive = FALSE)


custom_species_trained <- BotanizeR_quiz(species_list = custom_species, image_floraweb=FALSE,
                                         hints_floraweb = NULL,
                                         hints_custom = NULL, 
                                         imagelinks_custom = NULL, 
                                         image_folders = "inst/WWW/pictures_Clemens",
                                         case_sensitive = FALSE)

### Packages do not need to be installed as sudo from the console for Shiny!
# installing packages as root in R
# sudo su - -c "R -e \"install.packages('httr')\""
# sudo su - -c "R -e \"library(devtools);install_github('https://github.com/patrickweigelt/BotanizeR')\""


# interesting:
# https://www.santoshsrinivas.com/shiny-server-on-ubuntu/

# But shiny needs to be restarted
# sudo systemctl restart shiny-server


devtools::load_all()

# Species list for Germany with IDs from floraweb.de
data(floraweb_species)

# Select Acer campestre

custom_species <- floraweb_species[which(floraweb_species$SPECIES %in% c("Acer campestre","Erica carnea","Melampyrum nemorosum","Veronica chamaedrys")),]

species_row = floraweb_species[which(floraweb_species$SPECIES == "Acer campestre"),]

sp_clemens <- BotanizeR_collect(
  species_row = species_row, 
  image_floraweb = FALSE, hints_floraweb = NULL, 
  hints_custom = NULL, imagelinks_custom = NULL,
  image_folders = "inst/www/pictures_Clemens",
  file_location = "temporary", only_links = TRUE)


# only floraweb image + description + map
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("map","description","status","habitat","family","German name"), 
                           hints_custom = NULL, imagelinks_custom = NULL, image_folders = NULL,
                           file_location="temporary", image_width = 500)

par(oma=c(0,0,0,10.5))
plot(hints$map[[1]], pal = hints$map[[2]], key.pos = 4, main="")

par(mar=c(0.5,0.5,0.5,0.5),oma=c(0,0,0,0))
plot(hints$image[[1]], axes=FALSE)

message(hints$habitat)

names(hints)

# only custom image links + floraweb descriptions
hints <- BotanizeR_collect(species_row, image_floraweb = FALSE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = NULL, imagelinks_custom = c("imagelink_1","imagelink_2"), 
                           image_folders = NULL, file_location="temporary", image_width = 500)

plot(hints$image[[1]], axes=FALSE)
message(hints$habitat)

# floraweb + custom images + floraweb descriptions
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = NULL, imagelinks_custom = c("imagelink_1","imagelink_2"), 
                           image_folders = NULL, file_location="temporary")


# floraweb + custom images + floraweb descriptions + custom description
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = c("ownhint_1","ownhint_2"), 
                           imagelinks_custom = c("imagelink_1","imagelink_2"), 
                           image_folders = NULL, file_location="temporary")

# floraweb + custom images + only custom description
hints <- BotanizeR_collect(species_row, image_floraweb = FALSE, hints_floraweb = NULL, 
                           hints_custom = c("ownhint_1","ownhint_2"), 
                           imagelinks_custom = c("imagelink_1","imagelink_2"), 
                           image_folders = NULL, file_location="temporary")



# floraweb + custom image links + custom image folder + floraweb descriptions + custom description
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = c("ownhint_1","ownhint_2"), 
                           imagelinks_custom = c("imagelink_1","imagelink_2"), 
                           image_folders = "inst/WWW/pictures_Clemens", file_location="temporary")

plot(hints$image[[6]], axes=FALSE)

# custom image folder + floraweb descriptions + custom description
hints <- BotanizeR_collect(species_row, image_floraweb = FALSE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = c("ownhint_1","ownhint_2"), 
                           imagelinks_custom = NULL, 
                           image_folders = "inst/WWW/pictures_gehoelze_winter_400", file_location="temporary", only_links = TRUE)




species_row = floraweb_species[which(floraweb_species$SPECIES == "Sequoia sempervirens"),]
hints <- BotanizeR_collect(species_row, image_floraweb = FALSE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = c("ownhint_1","ownhint_2"), 
                           imagelinks_custom = NULL, 
                           image_folders = "inst/www/pictures_Clemens", file_location="temporary",
                           image_width = 500)

plot(hints$image[[1]], axes=FALSE)


species_row = floraweb_species[which(floraweb_species$SPECIES == "Silene latifolia"),]
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = c("ownhint_1","ownhint_2"), 
                           imagelinks_custom = c("imagelink_1","imagelink_2"), 
                           image_folders = c("inst/WWW/pictures_Clemens","inst/WWW/pictures_Clemens"), 
                           file_location="temporary", only_links = TRUE)


hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = c("ownhint_1","ownhint_2"), 
                           imagelinks_custom = c("imagelink_1","imagelink_2"), 
                           image_folders = c("inst/WWW/pictures_Clemens","inst/WWW/pictures_Clemens"), 
                           file_location="temporary", only_links = TRUE, image_required = TRUE)



species_list <- floraweb_species[which(floraweb_species$SUMMER==1 | floraweb_species$BioDiv2005==1),]


species_list <- BotanizeR_species
species_row = species_list[which(species_list$SPECIES == "Fagus sylvatica"),]
species_row = species_list[which(species_list$SPECIES == "Matricaria chamomilla"),]



image_floraweb=TRUE
hints_floraweb = c("map","description","status","habitat","family","German name")
image_ukplantatlas=TRUE
hints_ukplantatlas = c("mapuk","familyuk","ecology","statusuk","trends","perennation","lifeform","woodiness","clonality")
hints_custom = NULL
imagelinks_custom = NULL
image_folders = NULL
case_sensitive = TRUE
file_location="temporary"
only_links = FALSE
image_required = FALSE
image_width = NA




startat = 0


init_count = sum(species_list$COUNT)
init_score = sum(species_list$SCORE)




species_list <- floraweb_species_trained

species_list$prob <- (species_list$COUNT+1)/((species_list$SCORE+1)  / (species_list$ATTEMPTS+1)/(sum(species_list$ATTEMPTS)+1))     *species_list$INCLUDE


(species_list$ATTEMPTS+1)/(species_list$SCORE+1)

(species_list$SCORE+1)*(species_list$ATTEMPTS+1)


species_list$prob2 <-  ((species_list$COUNT - species_list$SCORE + 1)/(species_list$SCORE+1))*species_list$INCLUDE


(((species_list$COUNT) - ((species_list$SCORE)/(species_list$ATTEMPTS+1)) + 1)/(species_list$SCORE+1))*species_list$INCLUDE


# check if probs > 1 work
numbers <- c(1,2,3)

probs <- c(0.5,0.5,0.5)
mean(sample(numbers, 1000, replace=TRUE, prob=probs))

probs <- c(0.1,0.5,0.9)
mean(sample(numbers, 1000, replace=TRUE, prob=probs))

probs <- c(1,5,9)
mean(sample(numbers, 1000, replace=TRUE, prob=probs))
# seems good



sp_infos <- BotanizeR_collect(
  species_row = floraweb_species[1, ], 
  image_floraweb = FALSE,
  hints_floraweb = NULL, 
  hints_custom = NULL, imagelinks_custom = NULL,
  image_folders = "inst/www/pictures_Clemens",
  # image_folders = "~/ShinyApps/BotanizeR/WWW/pictures_Clemens", # This is needed on server; 
  file_location = "temporary", only_links = TRUE)


image1 <- load.image("inst/www/pictures_Clemens/SamenZapfenFruechte/Acer campestre (2).jpg")

image1 <- resize(image1, size_x = 320, size_y = 320/nrow(image1)*ncol(image1))
plot(image1)






devtools::load_all()

# Species list for Germany with IDs from floraweb.de
data(ukplantatlas_species)


species_row = ukplantatlas_species[which(ukplantatlas_species$SPECIES == "Acer campestre"),]
species_row = floraweb_species[which(floraweb_species$SPECIES == "Sanguisorba minor"),]
species_row = floraweb_species[which(floraweb_species$SPECIES == "Viola arvensis"),]
species_row = floraweb_species[which(floraweb_species$SPECIES == "Clematis vitalba"),]


hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = NULL, 
                           image_ukplantatlas=TRUE,
                           hints_ukplantatlas = c("mapuk"),
                           hints_custom = NULL,
                           imagelinks_custom = NULL, 
                           image_folders = NULL, 
                           file_location="temporary", only_links = FALSE)

hints <- BotanizeR_collect(species_row, image_floraweb = FALSE, 
                           hints_floraweb = NULL, 
                           image_ukplantatlas=TRUE,
                           hints_ukplantatlas = c("mapuk"),
                           hints_custom = NULL,
                           imagelinks_custom = NULL, 
                           image_folders = NULL, 
                           file_location="temporary", only_links = FALSE)

hints <- BotanizeR_collect(species_row, image_floraweb = FALSE, 
                           hints_floraweb = NULL, 
                           image_ukplantatlas=TRUE,
                           hints_ukplantatlas = numeric(0),
                           hints_custom = c("ownhint_1"),
                           imagelinks_custom = NULL, 
                           image_folders = c("inst/WWW/pictures_Clemens_400"), 
                           file_location="temporary", only_links = TRUE)


floraweb_species_trained <- BotanizeR_quiz(species_list = ukplantatlas_species, image_floraweb=FALSE,
                                           hints_floraweb = NULL,
                                           image_ukplantatlas=TRUE,
                                           hints_ukplantatlas = c("mapuk","familyuk","ecology","statusuk","trends","perennation","lifeform","woodiness","clonality"),
                                           hints_custom = c("ownhint_1"),
                                           imagelinks_custom = NULL, 
                                           image_folders = c("inst/WWW/pictures_Clemens_400"),
                                           case_sensitive = FALSE)





devtools::load_all()

# resize
BotanizeR_imageresize(image_folders = "inst/WWW/pictures_Clemens", image_width = 400, max_height = 480, quality = 1)
BotanizeR_imageresize(image_folders = "inst/WWW/pictures_gehoelze_winter", image_width = 400, max_height = 480, quality = 1)

BotanizeR_imageresize(image_folders = "inst/WWW/resize", image_width = 560, max_height = 700, quality = 1)
BotanizeR_imageresize(image_folders = "inst/WWW/resize", image_width = 560, max_height = 700, quality = 1, int_type = 5)


BotanizeR_imageresize(image_folders = "inst/WWW/drawings_Schulz", image_width = 500, max_height = 750, quality = 1)
BotanizeR_imageresize(image_folders = "inst/WWW/pictures_gehoelze_winter", image_width = 500, max_height = 750, quality = 1)

BotanizeR_imageresize(image_folders = "inst/WWW/drawings_Schulz", image_width = 560, max_height = 700, quality = 1)
BotanizeR_imageresize(image_folders = "inst/WWW/pictures_gehoelze_winter", image_width = 560, max_height = 700, quality = 1)

BotanizeR_imageresize(image_folders = "inst/WWW/pictures_gehoelze_winter", image_width = 500, max_height = 625, quality = 1)

BotanizeR_imageresize(image_folders = "inst/WWW/pictures_CommonWaysidePlants_Jambi", image_width = 700, max_height = 600, quality = 1)


BotanizeR_imageresize(image_folders = "D:/Morphologie_BotanizeR", image_width = 560, max_height = 700, quality = 1)




# Check availabilty of images Summer
summer <- read.csv("inst/species_summer_ANSI.csv")

image_files <- list.files("inst/WWW/BotanizeR_Summer_560", recursive = TRUE, full.names = FALSE)
summer$images <- 0

for (i in 1:nrow(summer)){
  species <- summer$SPECIES[i]
  
  images <- image_files[which(grepl(species, image_files) |
                                grepl(gsub("\\.","",species), image_files) |
                                grepl(gsub(" ","_",gsub("\\.","",species)), image_files) |
                                grepl(gsub(" ","_",species), image_files))]
  summer$images[i] <- length(images)
}

write.csv(summer, "species_summer_images.csv", row.names = FALSE)



# 1920 * 1080




species_main <- GET("https://jhkjhlkhlkhlkhlkhlkh")
# species_main <- htmlTreeParse(file = file.path(dir,"species.txt"), isURL = F, isHTML=T, useInternalNodes = T)
species_main <- htmlTreeParse(file = species_main, isURL = F, isHTML=T, useInternalNodes = T)





### crashing Germany all

devtools::load_all()

# Species list for Germany with IDs from floraweb.de
data(BotanizeR_species)

length(which(duplicated(BotanizeR_species$SPECIES)))

BotanizeR_species <- BotanizeR_species[order(BotanizeR_species$SPECIES),]
head(BotanizeR_species)

BotanizeR_species$SPECIES[2321]
species_row = BotanizeR_species[which(BotanizeR_species$SPECIES == BotanizeR_species$SPECIES[2321]),]

species_row = BotanizeR_species[2321,]
species_row = BotanizeR_species[342,]
species_row = BotanizeR_species[3001,]


species_row = BotanizeR_species[which(BotanizeR_species$SPECIES == "Acer campestre"),]
species_row = floraweb_species[which(floraweb_species$SPECIES == "Sanguisorba minor"),]
species_row = floraweb_species[which(floraweb_species$SPECIES == "Viola arvensis"),]


species_row = BotanizeR_species[which(BotanizeR_species$SPECIES == "Abies koreana"),]

sp_quiz <- BotanizeR_collect(
  species_row = temp1[i$i, ], 
  image_floraweb = hints_reactive$image_floraweb,
  hints_floraweb = hints_reactive$hints_floraweb[which(
    hints_reactive$hints_floraweb!="map")],
  image_ukplantatlas = hints_reactive$image_ukplantatlas,
  hints_ukplantatlas = hints_reactive$hints_ukplantatlas, # TODO: add != mapuk for UK
  hints_custom = hints_reactive$hints_custom, 
  imagelinks_custom = hints_reactive$imagelinks_custom,
  image_folders = paste0(system_path,hints_reactive$image_folders),
  only_links = TRUE, image_required = TRUE)

hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = NULL, 
                           image_ukplantatlas=TRUE,
                           hints_ukplantatlas = c("mapuk","familyuk","ecology","statusuk","trends",
                                                  "perennation","lifeform","woodiness","clonality"),
                           hints_custom = c("ownhint_English_name"),
                           imagelinks_custom = NULL, 
                           image_folders = c("inst/WWW/pictures_Clemens_500"), 
                           only_links = TRUE)




devtools::load_all()

data(BotanizeR_species)

BotanizeR_species <- BotanizeR_getlocallist(lat = 51.545483, long = 9.905548, backbone_list = BotanizeR_species)

BotanizeR_species <- BotanizeR_getlocallist(lat = -60, long = 100, backbone_list = BotanizeR_species)
BotanizeR_species <- BotanizeR_getlocallist(lat = -10, long = 100, backbone_list = BotanizeR_species)

GBIF_species <- BotanizeR_getlocallist(lat = 51.545483, long = 9.905548)
GBIF_species <- BotanizeR_getlocallist(lat = -60, long = 100)
GBIF_species <- BotanizeR_getlocallist(lat = -10, long = 100)

GBIF_species <- BotanizeR_getlocallist(lat = -90, long = 100)

GBIF_species <- BotanizeR_getlocallist(lat = -85, long = -180, radius = 5)
