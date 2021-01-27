# BotanizeR

TODO list

1. Functionality
* design starting site from which to choose the desired version from (winter, summer, other)
* allow to upload previous progress table for defining sample probabilities (higher prob for species not well practiced)
* record successes (score; account for retrieved answer) and number of times species shown and include them in the progress download
* remove checkboxes if content is not available
* make trivial name and family name independent of floraweb
* allow practicing trivial name
* seems to load map each time a box is unchecked (takes long)
* error finite ylim value needed... if no map available?
* previous and next species button in species list tab
* don't do adist when "" submitted

2. Base package
* check if cases with more than two images in floraweb exist and write loop for BotanizeR_collect!
* add content from other websites
* plot first image before map is loaded to safe time.
* loading bar for map
* add synonyms
* species list Göttingen

3. Layout
* center smaller images in slider
* avoid overlap of contents when changing window size
* Source floraweb is inbetween the hints (above habitat); Make dependent on whether floraweb content is shown or not
* return between the two status elements
* adjust height of panel columns

3. Server
* proper url (e.g. gift.uni-goeetingen.de/BotanizeR)
* secure shiny behind nginx remote proxy
* embed in stud-ip

4. Other
* write documentation (for package and for "About" page)
* contact floraweb for permission to have a public version
* write a little software note?
* publish on CRAN?
* prepare more picture folders and species lists (algae, bark pictures); agree on columns needed

