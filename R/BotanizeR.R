### BotanizeR

library(imager)
library(XML)

hints <- c("image","description","floristic status","conservation status","family","German name")

species_list <- read.csv("floraweb_allspecies_subset.csv")

species_list$COUNT <- 1
species_list$SCORE <- length(hints)


i <- sample(1:nrow(species_list), 1, prob = species_list$SCORE/species_list$COUNT)

species <- species_list$SPECIES



# One species

# main infos
# I need this step because of an error in RCURL when getting the url
download.file(paste("https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = "main.txt", quiet = T)
html_main <- htmlTreeParse(file = "main.txt", isURL = F, isHTML=T, useInternalNodes = T)
infos_main <- xpathApply(html_main, "//div[@id='content']//p",xmlValue)

# ecology
download.file(paste("https://www.floraweb.de/pflanzenarten/oekologie.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = "ecology.txt", quiet = T)
html_ecology <- htmlTreeParse(file = "ecology.txt", isURL = F, isHTML=T, useInternalNodes = T)
infos_ecology <- xpathApply(html_ecology, "//div[@id='content']//p",xmlValue)

# photo
download.file(paste("https://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=",species_list$NAMNR[i], sep=""), destfile = "photo.txt", quiet = T)
html_photo <- htmlTreeParse(file = "photo.txt", isURL = F, isHTML=T, useInternalNodes = T)
photolink <- xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs)[[1]][3]
image <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",photolink), sep=""))

infos_photo <- xpathApply(html_photo, "//div[@id='content']//p",xmlValue)

plot(image, xaxt="n", yaxt="n")



xpathApply(html, "//div[@id='content']//img",xmlAttrs)[[1]][3]


