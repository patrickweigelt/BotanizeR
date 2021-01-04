# BotanizeR
Little quiz game to memorize the plant species of Germany

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
                                           hints = c("description","status",
                                                     "habitat","family","German name"),
                                                     case_sensitive = FALSE)

# If you want to keep track of your progress, you can save the species list with updated scores locally and load it in the next session

# Initial saving
write.csv(floraweb_species_trained, "floraweb_species_trained.csv", row.names = FALSE)

# Load species list
floraweb_species_trained <- read.csv("floraweb_species_trained.csv")

# Practice
floraweb_species_trained <- BotanizeR_quiz(species_list = floraweb_species_trained,
                                           hints = c("description","status","habitat",
                                          "family","German name"), case_sensitive = FALSE)

# Save species list
write.csv(floraweb_species_trained, "floraweb_species_trained.csv", row.names = FALSE)
```

Sources  
Bundesamt für Naturschutz (BfN) [http://www.floraweb.de](http://www.floraweb.de)  

Image authors: [https://www.floraweb.de/ueberfloraweb/bildautoren.html](https://www.floraweb.de/ueberfloraweb/bildautoren.html)