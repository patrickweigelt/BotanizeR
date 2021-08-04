
# BotanizeR <img src="figures/biodiv_gottingen_logo.png" align="right" alt="" width="200" />
<br>
[![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)
<br>
<br>
BotanizeR is an R-package with Shiny app designed to help botany students and 
other people interested in plants to learn and distinguish plant diagnostic 
characters, memorize plant species, and train their identification skills. 
Students can browse a species list, images, plant characteristics, habitat 
descriptions and distribution information or play a quiz where images of a 
random species are shown and students have to guess the correct species. 
BotanizeR is highly flexible allowing lecturers to define their own species 
lists and to provide images and further useful information, or choose from 
linked online content from selected botanical online resources.
A Quiz and Species overview pages can be made available via the package’s Shiny 
application or played within R directly. 


## 1. Tutorials

Here, we provide tutorials on <br>
- **[how to use the package's functions](https://patrickweigelt.github.io/BotanizeR/articles/BotanizeR_functions.html)**  
- **[how to use the BotanizeR Shiny app](https://patrickweigelt.github.io/BotanizeR/articles/BotanizeR_Shiny.html)**  
- **[how to set up and customize the Shiny app](https://patrickweigelt.github.io/BotanizeR/articles/BotanizeR_config.html)**  


## 2. Functions

An overview over all functions and data is given 
**[here](https://patrickweigelt.github.io/BotanizeR/reference/index.html)**.  


## 3. Installation
``` r
library(devtools)
install_github("https://github.com/patrickweigelt/BotanizeR")

library(BotanizeR)
```

## 5. First steps  

### Shiny app  

You can start the Shiny application with the function `BotanizeR_shiny()`.

``` r
BotanizeR_shiny()
```

### Running the console quiz 

You can run the console version of the quiz using `BotanizeR_quiz()`.

``` r
# Load species list for UK, Ireland and Germany based on floraweb.de and the 
# Online Atlas of the British and Irish flora 
# (https://www.brc.ac.uk/plantatlas/)
data(BotanizeR_species)

# Subset for about 700 species known from Sussex, UK
BotanizeR_Sussex <- 
  BotanizeR_species[which(BotanizeR_species$UK_Ireland_Sussex==1), ]

# Type in species name, or press enter for next hint or type "skip" and
# press enter for next species or type "exit" to end quiz and save results
BotanizeR_example_practiced <- 
  BotanizeR_quiz(species_list = BotanizeR_example,
                 image_floraweb = FALSE, image_ukplantatlas = TRUE, 
                 hints_ukplantatlas =  c('mapuk', 'familyuk', 'ecology', 
                                         'statusuk', 'trends', 'perennation',
                                         'lifeform', 'woodiness', 
                                         'clonality'), 
                 hints_floraweb = NULL, case_sensitive = FALSE)

# If you want to keep track of your progress, you can save the species list
# with updated scores locally and load it in the next session
```

## 5. Examples

To showcase the flexibililty of BotanizeR we present 4 use cases:<br>  

**1) https://gift.uni-goettingen.de/shiny/BotanizeR/**

This particular instance of the BotanizeR Shiny app exemplifies a few 
use cases. You can practice species from the Floras of 
Germany, Britain and Ireland based on information retrieved live from the 
websites of [FloraWeb](http://www.floraweb.de) and the 
[Online Atlas of the British and Irish flora](https://www.brc.ac.uk/plantatlas/). 
You can choose which of the Floras to practice species from, 
which species subset to use and which information from each of the websites to 
show.This instance of BotanizeR is based on the example data and default 
settings of the BotanizeR R package and can be customized and used for plant 
identification classes across central Europe.  

**2) https://gift.uni-goettingen.de/shiny/BotanizeR_winter/**

In this more customized instance of BotanizeR you can practice 128 woody 
species from Germany based on images of various plant characteristics in winter 
state. The Setup page is disabled and the species have to be identified 
by images of bark, twig, and bud characteristics only. Only the German name, 
family and floristic status in Germany are drawn in addition from 
[FloraWeb](http://www.floraweb.de).  
 
**3) https://gift.uni-goettingen.de/shiny/BotanizeR_summer/**

Here, you can practice 214 forest indicator species from Germany. In the 
settings tab, you can enable and disable images and hints from 
[FloraWeb](http://www.floraweb.de) and the 
[Online Atlas of the British and Irish flora](https://www.brc.ac.uk/plantatlas/) 
as well as images from a local folder on the server including 
detailed images of plant characteristics like leaves, flowers, fruits, bark 
etc. You can also enable or disable additional hints drawn from the species 
list like the taxonomic group, family, German name, Ellenberg indicator values 
or flowering time.  

**4) https://gift.uni-goettingen.de/shiny/BotanizeR_Indonesia/**

This instance of BotanizeR exemplifies two use cases from tropical regions. 
Here you can practice 113 common wayside plant species of lowland Jambi 
Province, Sumatra 
[Rembold et al., 2017](https://doi.org/10.1016/j.biocon.2017.07.020) 
or 107 common tree species from Lore Lindu National Park, Sulawesi 
[Brambach et al., 2017](https://doi.org/10.1016/j.ppees.2017.06.003). You can 
hoose which of the Floras to practice species from and which additional hints 
to show.

## 6. References and dependencies  

### Dependencies
`BotanizeR` depends on `dplyr`, `htmltools`, `httr`, `imager`, `magick`,
`rgbif`, `rgeos`, `sf`, `shiny`, `shinyBS`, `shinyFiles`, `shinythemes`,
`shinyjs`, `slickR`, `sp` and `XML`.


### References    
Bundesamt für Naturschutz (BfN): 
[http://www.floraweb.de](http://www.floraweb.de)  
Image authors: 
[https://www.floraweb.de/ueberfloraweb/bildautoren.html](https://www.floraweb.de/ueberfloraweb/bildautoren.html)

Online Atlas of the British and Irish flora: 
[https://www.brc.ac.uk/plantatlas/](https://www.brc.ac.uk/plantatlas/)  
Image authors: 
[https://www.brc.ac.uk/plantatlas/content/photos](https://www.brc.ac.uk/plantatlas/content/photos)  



