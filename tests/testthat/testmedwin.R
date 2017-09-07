#Test the medwin function#
test_that("medwin returns two values", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  testmedwin <- medwin(dataset = testdata[[1]]$Dataset)
  
  #Test that medwin returns two objects
  expect_false(is.null(testmedwin[2]$'Median Window Close'))
  expect_false(is.null(testmedwin[1]$'Median Window Open'))
  
  #Test that returned objects are numeric
  expect_true(is.numeric(testmedwin[2]$'Median Window Close'))
  expect_true(is.numeric(testmedwin[1]$'Median Window Open'))
  
  #Test that values returned are the same as previous R version
  expect_true(testmedwin[[1]] == 2.5)
  expect_true(testmedwin[[2]] == 2)
  
})