# Local list test
test_that("Expected data structure", {
  
  library(BotanizeR)
  
  # Species list for Germany with IDs from floraweb.de
  data("BotanizeR_species")
  
  gottingen_coords <- data.frame(lon = 9.93558,
                                 lat = 51.53290,
                                 radius = 1)
  
  test_locallist <- BotanizeR_getlocallist(
    long = gottingen_coords$lon,
    lat = gottingen_coords$lat,
    radius = gottingen_coords$radius,
    backbone_list = BotanizeR_species[which(BotanizeR_species$SPECIES %in%
                                              c("Acer platanoides",
                                                "Acer pseudoplatanus")), ])
  
  expect_equal(class(test_locallist), "data.frame")
  
  expect_true("FREQ" %in% colnames(test_locallist))
  
})
