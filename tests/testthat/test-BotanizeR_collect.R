# Collect information
test_that("Expected data structure", {
  
  library(BotanizeR)
  
  # Species list for Germany with IDs from floraweb.de
  data(floraweb_species)
  
  # Select Acer campestre
  species_row = floraweb_species[which(floraweb_species$SPECIES ==
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
