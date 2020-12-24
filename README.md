# BotanizeR
Little quiz game to memorize the plant species of Germany

```
library(devtools)
install_github("https://github.com/patrickweigelt/BotanizeR")

library(BotanizeR)

# Species list for Germany with IDs from floraweb.de
data(floraweb_species)

# subset for nearly 200 species forestry students in Göttingen learn
floraweb_species <- floraweb_species[which(floraweb_species$SUMMER==1),]

# Type in species name, or press enter for next hint or type "skip" and press 
# enter for next species or type "exit" to end quiz and save results
floraweb_species_trained <- BotanizeR_quiz(species_list = floraweb_species, 
                                           hints = c("description","status",
                                                     "habitat","family","German name"))
```

Sources  
Bundesamt für Naturschutz (BfN) [http://www.floraweb.de](http://www.floraweb.de)  

Image authors: [https://www.floraweb.de/ueberfloraweb/bildautoren.html](https://www.floraweb.de/ueberfloraweb/bildautoren.html)