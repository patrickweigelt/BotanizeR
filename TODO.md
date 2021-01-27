# BotanizeR

TODO list

1. Functionality
* fix the mismatch bug
* design settings tab from which to choose the desired version from (winter, summer, other)
  - making the app as flexible as possible. Users choose their own list and criteria
* config file for default values: config.R
* ~~allow to upload previous progress table for defining sample probabilities (higher prob for species not well practiced)~~
* record successes (score; account for retrieved answer) and number of times species shown and include them in the progress download
  - fix counting bug
  - fix cheating bug
  - make sure score is counted if answer is retrieved accidentally after getting the name right
* remove checkboxes if content is not available
* make trivial name and family name independent of floraweb
  - add them to species list table?
* allow practicing trivial name
* seems to load map each time a box is unchecked (takes long)
* error finite ylim value needed... if no map available?
* previous and next species button in species list tab
* don't do adist when "" submitted
* chorology?
  - add to BotanizeR_collect?
  - show as one of the images
  - remove from quiz due to species name in map

2. Base package
* check if cases with more than two images in floraweb exist and write loop for BotanizeR_collect!
* add content from other websites
* plot first image before map is loaded to safe time.
* loading bar for map
* add synonyms
* species list Göttingen
* image folder path in local shiny app

3. Layout
* center smaller images in slider
* avoid overlap of contents when changing window size
* Source floraweb is inbetween the hints (above habitat); Make dependent on whether floraweb content is shown or not
* return between the two status elements
* adjust height of panel columns
* move items
  - download button

4. Server
* proper url (e.g. gift.uni-goeetingen.de/BotanizeR)
* secure shiny behind nginx remote proxy
* embed in stud-ip

5. Other
* write documentation (for package and for "About" page)
* contact floraweb for permission to have a public version
* write a little software note?
* publish on CRAN?
* prepare more picture folders and species lists (algae, bark pictures); agree on columns needed

