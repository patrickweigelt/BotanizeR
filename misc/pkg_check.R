
# Build the website
# usethis::use_pkgdown() # Run once to configure your package to use pkgdown
pkgdown::build_site()

# To generate the documentation of functions and NAMESPACE
roxygen2::roxygenise()

# Mispellings
devtools::spell_check()

# Practice improvements
goodpractice::gp()

# Tests # https://r-pkgs.org/tests.html
usethis::use_testthat() # setting up the tests structure
# testthat::test_that() # test for one function