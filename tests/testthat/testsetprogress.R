# Test SetProgressBar outcome #

# Test that a progress bar object has been created #
test_that("Progress bar works", {
  
test <- SetProgressBar(furthest = 2, closest = 1, STAT = "max")

expect_true(attr(test, "class") == "txtProgressBar")

})

