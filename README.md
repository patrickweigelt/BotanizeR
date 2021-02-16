# BotanizeR
A quiz game to memorize plant species names and characteristics

```
library(devtools)
install_github("https://github.com/patrickweigelt/BotanizeR")

library(BotanizeR)

# Species list for Germany with IDs from floraweb.de
data(floraweb_species)

# Subset for about 300 species students in Göttingen learn
floraweb_species <- floraweb_species[which(floraweb_species$SUMMER==1 | floraweb_species$BioDiv2005==1),]

# Type in species name, or press enter for next hint or type "skip" and press 
# enter for next species or type "exit" to end quiz and save results
floraweb_species_trained <- BotanizeR_quiz(species_list = floraweb_species, 
                                           hints_floraweb = c("description","status",
                                                     "habitat","family","German name"),
                                                     case_sensitive = FALSE)

# If you want to include distribution maps as hints add "map" to hints; This increases the download times a bit
floraweb_species_trained <- BotanizeR_quiz(species_list = floraweb_species, 
                                           hints_floraweb = c("map","description","status",
                                                     "habitat","family","German name"),
                                                     case_sensitive = FALSE)

# If you want to keep track of your progress, you can save the species list with updated scores locally and load it in the next session

# Initial saving
write.csv(floraweb_species_trained, "floraweb_species_trained.csv", row.names = FALSE)

# Load species list
floraweb_species_trained <- read.csv("floraweb_species_trained.csv")

# Practice
floraweb_species_trained <- BotanizeR_quiz(species_list = floraweb_species_trained,
                                           hints_floraweb = c("map","description","status","habitat",
                                          "family","German name"), case_sensitive = FALSE)

# Save species list
write.csv(floraweb_species_trained, "floraweb_species_trained.csv", row.names = FALSE)



### example for three species with custom hints and images
custom_species <- floraweb_species[which(floraweb_species$SPECIES %in% c("Acer campestre","Erica carnea","Melampyrum nemorosum")),]

custom_species_trained <- BotanizeR_quiz(species_list = custom_species, image_floraweb=TRUE,
                                         hints_floraweb = NULL,
                                         hints_custom = c("ownhint_1","ownhint_2"), 
                                         imagelink_custom = c("imagelink_1","imagelink_2"), 
                                         image_folders = NULL, case_sensitive = FALSE)
                                    
```

Sources  
Bundesamt für Naturschutz (BfN) [http://www.floraweb.de](http://www.floraweb.de)  
Image authors: [https://www.floraweb.de/ueberfloraweb/bildautoren.html](https://www.floraweb.de/ueberfloraweb/bildautoren.html)

Online Atlas of the British and Irish flora [https://www.brc.ac.uk/plantatlas/](https://www.brc.ac.uk/plantatlas/)
Image authors: [https://www.brc.ac.uk/plantatlas/content/photos](https://www.brc.ac.uk/plantatlas/content/photos)


