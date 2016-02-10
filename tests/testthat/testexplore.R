# Test the outcomes of the explore function #

# Test that errors are thrown correctly #
test_that("explore produces parameter errors", {
  
  # Test an error is returned when shape is <= 0 for weibull
  expect_error(explore(shape=-1, scale=0.2, loc=0, weightfunc = "W"))
  
  # Test an error is returned when scale is <= 0 for weibull
  expect_error(explore(shape=3, scale=0, loc=0, weightfunc = "W"))
  
  # Test an error is returned when location is >0 for weibull
  expect_error(explore(shape=3, scale=0.2, loc=1, weightfunc = "W"))
  
  # Test an error is returned when scale is < 0 for GEV
  expect_error(explore(shape=3, scale=-1, loc=0, weightfunc = "G"))
  
})