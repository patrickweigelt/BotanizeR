# BotanizeR

TODO list

1. Functionality
* ~~fix the mismatch bug~~
* ~~design settings tab from which to choose the desired version from (winter, summer, other)~~
  - ~~making the app as flexible as possible. Users choose their own list and criteria~~
  - ~~filtering via column in df <- solved y having predefined subsets~~
  - ~~summary stats about species list: number of species~~
  - ~~save initial scores as reactive values when new list loaded~~
  - control for right columns in data.frame
  - ~~make example datasets available via dropdown~~
  - ~~read custom hints from table header~~
  - image folders
* ~~config file for default values: config.R~~
* ~~allow to upload previous progress table for defining sample probabilities (higher prob for species not well practiced)~~
* ~~record successes (score; account for retrieved answer) and number of times species shown and include them in the progress download~~
* ~~make sure score is counted if answer is retrieved accidentally after getting the name right~~
* ~~remove checkboxes if content is not available~~
* make common name and family name independent of floraweb
  - add them to species list table, but not obligatory (too big a hurdle)
* allow practicing common name
* seems to load map each time a box is unchecked (takes long)
  - make individual checkbox object for map
* ~~error finite ylim value needed... if no map available?~~
* ~~don't show hints that are not available?~~
* previous and next species button in species list tab
* ~~don't do adist when "" submitted~~
* chorology?
  - add to BotanizeR_collect
  - Use Halle chorology site directly?
  - show as one of the images?
  - ~~add to setup~~
  - ~~remove from quiz due to species name in map~~
* ~~Add UK flora~~
* ~~Summary statistics~~
  - ~~number of species practiced, number correct as bar plot? (quiz page popup)~~
  - ~~number of species in currently loaded species list (setup page)~~
* Make some of the elements in the about page dependent on the resources used
  - If floraweb used: Floraweb credits
  - If own images used load file with image credits
* Plot UK map and think about where to plot maps in general. Make space available if maps not plotted?
* App becomes unresponsive if you choose no picture source or the wrong one and the quiz keeps trying to find a species with picture
* Why does species list page update immediately after setup changes and quiz doesn't?
* Only 1000 entries in species list dropdown. Selectize does more but doesn't have auto-complete

2. Base package
* check if cases with more than two images in floraweb exist and write loop for BotanizeR_collect!
* add content from other websites
  - ~~UK-flora~~
* plot first image before map is loaded to safe time.
* loading bar for map
* add synonyms
* species list Göttingen
* check image folder path in local shiny app
* make error messages in case floraweb is not available
* define columns needed for species list
* combine the tables and make columns for UK and GER ...
* Subspecies in UK-list
* replace species names in descriptions be "The species"

3. Layout
* ~~center smaller images in slider~~
* ~~avoid overlap of contents when changing window size~~
  - ~~define min width of middle column or make slickR slider shrink~~
* ~~Source floraweb is inbetween the hints (above habitat); Make dependent on whether floraweb content is shown or not~~
* return between the two status elements
* ~~adjust height of panel columns~~
* move items
  - ~~download button~~
* Make correct more prominent and easier to go to next species  

4. Server
* proper url (e.g. gift.uni-goeetingen.de/BotanizeR)
* secure shiny behind nginx or apache remote proxy
* embed in stud-ip (why not working?)

5. Other
* write documentation (for package and for "About" page)
* contact floraweb for permission to have a public version
* write a little software note?
  - check potential journals
* write function documentation
* publish on CRAN?
  - Is the quiz function which needs user input a problem?
* prepare more picture folders and species lists (algae, bark pictures)

