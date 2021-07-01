# Quiz test
test_that("Expected data structure", {
  
  library(BotanizeR)
  
  # Species list for Germany with IDs from floraweb.de
  data("BotanizeR_species")
  
  # Select Acer campestre
  species_row <- BotanizeR_species[which(BotanizeR_species$SPECIES ==
                                           "Acer campestre"), ]
  
  # Quiz with no try
  exit <- NA
  test_quiz <- BotanizeR_quiz(
    species_row, image_floraweb = TRUE,
    hints_floraweb = c("description", "status", "habitat", "family",
                       "German name"),
    hints_custom = NULL, imagelinks_custom = NULL, image_folders = NULL,
    file_location = "temporary")
  
  exit
  
  expect_equal(species_row, test_quiz)
  
  # Quiz with one skip
  test_quiz <- BotanizeR_quiz(
    species_row, image_floraweb = TRUE,
    hints_floraweb = c("description", "status", "habitat", "family",
                       "German name"),
    hints_custom = NULL, imagelinks_custom = NULL, image_folders = NULL,
    file_location = "temporary")
  
  skip
  exit
  
  expect_equal(test_quiz$COUNT, 1)
  
})
