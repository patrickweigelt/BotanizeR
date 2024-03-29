# Collect information
test_that("Expected data structure", {
  
  library(BotanizeR)
  
  data("BotanizeR_species")
  
  # Select Acer campestre
  species_row <- BotanizeR_species[which(BotanizeR_species$SPECIES ==
                                           "Acer campestre"),]
  
  # Running the function
  test_collect <- BotanizeR_collect(
    species_row, image_floraweb = TRUE,
    hints_floraweb = c("description", "status", "habitat", "family",
                       "German name"),
    hints_custom = NULL, imagelinks_custom = NULL, image_folders = NULL,
    file_location = "temporary")
  
  expect_equal(class(test_collect), "list")
  
})
