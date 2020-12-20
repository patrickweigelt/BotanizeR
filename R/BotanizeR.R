### BotanizeR
https://www.floraweb.de/bilder/arten/0341.jpg

library(imager)
library(XML)

image <- load.image("https://www.floraweb.de/bilder/arten/0341.jpg")
plot(image, xaxt="n", yaxt="n")

link <- "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=1000&"
test <- getURL("https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=1000&")
html <- htmlTreeParse(file = link, isHTML = T, useInternalNodes = F)

# One species

# I need this step because of an error in RCURL when getting the url
download.file("https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=1000&", destfile = "test.txt")
html <- htmlTreeParse(file = "test.txt", isURL = F, isHTML=T, useInternalNodes = T)


download.file("https://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=1000", destfile = "photo.txt")
html_photo <- htmlTreeParse(file = "photo.txt", isURL = F, isHTML=T, useInternalNodes = T)
photolink <- xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs)[[1]][3]
image <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",photolink), sep=""))

xpathApply(html_photo, "//div[@id='content']",xmlValue)

plot(image, xaxt="n", yaxt="n")



xpathApply(html, "//div[@id='content']//img",xmlAttrs)[[1]][3]


