\name{BotanizeR_collect}
\alias{BotanizeR_collect}
\title{Collect information to show as hints in BotanizeR quiz}
\description{
Collects information from floraweb.de (images, map and species desciptions) and from user defined image folders and columns in the species_list data.frame to show them as hints in BotanizeR_quiz
}
\usage{
BotanizeR_collect()
}
\arguments{
}
\details{
}
\value{
A list of named elements to show as hints in the quiz (images, map, descriptions).
}
\details{}
\section{Note}{}

\examples{
# Species list for Germany with IDs from floraweb.de
data(floraweb_species)

# Select Acer campestre
species_row = floraweb_species[which(floraweb_species$SPECIES == "Acer campestre"),]

# Some self-made hints
species_row$ownhint_1 <- "very nice species"
species_row$ownhint_2 <- "tree"
species_row$ownhint_3 <- NA

# Image links from Wikipedia
species_row$imagelink_1 <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Acer_campestre_005.jpg/1280px-Acer_campestre_005.jpg"
species_row$imagelink_2 <- "https://upload.wikimedia.org/wikipedia/commons/f/f5/237_Acer_campestre.jpg"
species_row$imagelink_3 <- ""

# only floraweb image + description + map
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("map","description","status","habitat","family","German name"), 
                           hints_custom = NULL, imagelink_custom = NULL, image_folder = NA,
                           file_location="temporary")

par(oma=c(0,0,0,10.5))
plot(hints$map[[1]], pal = hints$map[[2]], key.pos = 4, main="")

par(mar=c(0.5,0.5,0.5,0.5),oma=c(0,0,0,0))
plot(hints$image[[1]], axes=FALSE)

message(hints$description)

# only custom image links + floraweb descriptions
hints <- BotanizeR_collect(species_row, image_floraweb = FALSE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = NULL, imagelink_custom = c("imagelink_1","imagelink_2","imagelink_3"), 
                           image_folder = NA, file_location="temporary")

# floraweb + custom images + floraweb descriptions
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = NULL, imagelink_custom = c("imagelink_1","imagelink_2","imagelink_3"), 
                           image_folder = NA, file_location="temporary")

# floraweb + custom images + floraweb descriptions + custom description
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = c("ownhint_1","ownhint_2","ownhint_3"), 
                           imagelink_custom = c("imagelink_1","imagelink_2","imagelink_3"), 
                           image_folder = NA, file_location="temporary")

# floraweb + custom images + only custom description
hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, hints_floraweb = NULL, 
                           hints_custom = c("ownhint_1","ownhint_2","ownhint_3"), 
                           imagelink_custom = c("imagelink_1","imagelink_2","imagelink_3"), 
                           image_folder = NA, file_location="temporary")
                           
# To load images from your local computer, specify an image folder with pictures included. File names need to include the species names.
}
