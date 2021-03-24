# BotanizeR

**<span style="color:blue"><font size="4">TO DO list</span></font>**

# Table of Contents
1. [Functionality](#functionality)
2. [Package](#package)
3. [Layout](#layout)
4. [Server](#server)
5. [Other](#other)

## 1. Functionality
* ~~fix the mismatch bug~~
* ~~design settings tab from which to choose the desired version from (winter, summer, other)~~
  - ~~making the app as flexible as possible. Users choose their own list and criteria~~
  - ~~filtering via column in df <- solved y having predefined subsets~~
  - ~~summary stats about species list: number of species~~
  - ~~save initial scores as reactive values when new list loaded~~
  - ~~control for right columns in data.frame~~
  - ~~only accept ownhint columns to make security checks easier~~
  - ~~get rid of omit_columns statement~~
  - ~~make example datasets available via dropdown~~
  - ~~read custom hints from table header~~
  - ~~image folders~~
* ~~config file for default values: config.R~~
* ~~Radio buttons checking/unchecking several times~~
* ~~Sanguisorba minor: species without UK map caused error when enabling uk map in settings while German was shown~~ <- possibly solved
* ~~allow to upload previous progress table for defining sample probabilities (higher prob for species not well practiced)~~
* ~~record successes (score; account for retrieved answer) and number of times species shown and include them in the progress download~~
* ~~make sure score is counted if answer is retrieved accidentally after getting the name right~~
* ~~remove checkboxes if content is not available~~
* make common name and family name independent of floraweb
  - add them to species list table, but not obligatory (too big a hurdle)
* allow practicing common name
* ~~seems to load map each time a box is unchecked (takes long)~~
  - ~~make individual checkbox object for map~~
* ~~error finite ylim value needed... if no map available?~~
* ~~Ownhints are only showing after setup tab has been visited~~
* Only list own hints in setup which have data for species subset
* don't show hints that are not available?
* **remove ownhint_ from checkboxes**
* ~~previous and next species button in species list tab~~
* ~~previous and next species working with keyboard~~
  - this interferes with the next and previous buttons from slickR - maybe remove?
  - make left and right buttons in quiz switch pictures also when outside lider
* ~~don't do adist when "" submitted~~
* chorology?
  - add to BotanizeR_collect
  - Use Halle chorology site directly?
  - show as one of the images?
  - ~~add to setup~~
  - ~~remove from quiz due to species name in map~~
* ~~Add UK flora~~
* **filfeform not working**
* ~~Summary statistics~~
  - ~~number of species practiced, number correct as bar plot? (quiz page popup)~~
  - ~~number of species in currently loaded species list (setup page)~~
* Make some of the elements in the about page dependent on the resources used
  - If floraweb used: Floraweb credits
  - If own images used load file with image credits
* ~~Plot UK map and think about where to plot maps in general. Make space available if maps not plotted?~~
* ~~App becomes unresponsive if you choose no picture source or the wrong one and the quiz keeps trying to find a species with picture~~
* Why does species list page update immediately after setup changes and quiz doesn't?
* ~~Only 1000 entries in species list dropdown. Use Selectize~~
* ~~Some UK pictures don't exist as large pictures and hence don't show up~~
  - ~~now that the images scale properly we could load the larger ones for these cases. Need control for this~~  
* ~~Make images scale automatically in slickR slider~~
* ~~Add image credits below slider~~
  - **modify BotanizeR_collect to keep track of image source** <- started  
* ~~Map legend and credits~~
* ~~separate UK and DE credits in quiz hints~~
* ~~no map text~~
* redo message images (harmonize)
* ~~if map is checked, going to next species is slow because new map loads (uncheck first)~~
* ~~make whether setup tab is shown a variable in config~~
  - ~~include upload and download of species list in quiz if setup tab is disabled~~
  - Give feedback about species number and number of species practiced in upload popup on Quizpage
* ~~Put upload/download buttons into popup on species page~~
* Twitter buttons in navbar and stats
* Privacy issues (Martin)
* zoom into images


## 2. Package
* check if cases with more than two images in floraweb exist and write loop for BotanizeR_collect!
* add flowering time and vegetative/sexual reproduction
* add content from other websites
  - ~~UK-flora~~
  - Telabotanica
* plot first image before map is loaded to safe time.
* loading bar for map
* add synonyms
* species list Göttingen
* ~~check image folder path in local shiny app~~
* make error messages in case floraweb is not available
* ~~define columns needed for species list~~
* ~~combine the tables and make columns for UK and GER~~
* Subspecies in UK-list
* replace species names in descriptions be "The species"
* Map floraweb distribution based on api
* switch entirely to httr to get rid of temp folders
* jpg, JPG, JPEG, jpeg etc. for own folder
* **BotanizeR collect needs to also look for won images after replacing . in species names (sp. etc)**

## 3. Layout
* ~~center smaller images in slider~~
* ~~avoid overlap of contents when changing window size~~
  - ~~define min width of middle column or make slickR slider shrink~~
* ~~Source floraweb is inbetween the hints (above habitat); Make dependent on whether floraweb content is shown or not~~
* ~~adjust height of panel columns~~
* move items
  - ~~download button~~
* ~~Make correct more prominent and easier to go to next species; info messages~~ 
* **Make correct easier to go to next species** 
* **Clean info messages** 
* Make online resources in Setup collapsable  

## 4. Server
* proper url (e.g. gift.uni-goeetingen.de/BotanizeR)
* secure shiny behind nginx or apache remote proxy
* embed in stud-ip (why not working?)

## 5. Other
* **write documentation (for package and for "About" page)**
* ~~contact floraweb and UKplantatlas for permission to have a public version~~
  - waiting for response
* write a little software note?
  - check potential journals  
* write function documentation
* publish on CRAN?
  - Is the quiz function which needs user input a problem?
* prepare more picture folders and species lists (algae, bark pictures)

