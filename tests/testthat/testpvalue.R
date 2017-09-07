# Test the pvalue function #
test_that("pvalue produces Pc metric", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  testdatarand <- randwin(repeats = 2, xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                          type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                          window = "sliding")
  
  testpvalue <- pvalue(datasetrand = testdatarand[[1]], dataset = testdata[[1]]$Dataset,
                       metric = "C", sample.size = 47)
  
  #Test that pvalue produces a number for Pc
  expect_true(is.numeric(testpvalue))
  
  #Test that value is the same as current R version
  expect_true(round(testpvalue, 2) == 0.96)
  
})

test_that("pvalue produces PdAICc metric", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  testdatarand <- randwin(repeats = 2, xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                          type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                          window = "sliding")
  
  testdata[[1]]$Dataset$deltaAICc[1] = testdatarand[[1]]$deltaAICc[1]
  
  testpvalue <- pvalue(datasetrand = testdatarand[[1]], dataset = testdata[[1]]$Dataset,
                       metric = "AIC", sample.size = 47)
  
  #Test that pvalue produces a number for PdAICc
  expect_true(is.numeric(testpvalue))
  
  testdata[[1]]$Dataset$deltaAICc[1] = -100
  
  testpvalue <- pvalue(datasetrand = testdatarand[[1]], dataset = testdata[[1]]$Dataset,
                       metric = "AIC", sample.size = 47)
  
  #Test that pvalue returns <0.001 when percentile is 0
  expect_true(testpvalue == "<0.001")
  
})