# Image resize test
test_that("Expected data structure", {
  
  library(BotanizeR)

  test_imageresize <- BotanizeR_imageresize(
    image_folders = "", image_width = 500,
    max_height = NA, int_type = 1, quality = 1)
  
  expect_true(is.null(test_imageresize))
  
})
