# Test loglik functions G and W #

# Test GEV function #
# Test that deltaAICc output is not NA #
# Test that deltaAICc is less than 0 #
test_that("GEV loglik test", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  funcenv              <- environment()
  Xvar                 <- MassClimate$Temp
  BDate                <- Mass$Date
  CDate                <- MassClimate$Date
  furthest             <- 2
  closest              <- 1
  duration             <- (furthest-closest) + 1
  baseline             <- lm(Mass ~ 1, data = Mass)
  CMatrix              <- matrix(ncol = (duration), nrow = length(BDate))
  nullmodel            <- AICc(baseline)
  funcenv$MODNO        <- 1
  funcenv$DAICc        <- list()
  funcenv$par_shape    <- list()
  funcenv$par_scale    <- list()
  funcenv$par_location <- list()
  
  cont <- DateConverter(BDate = BDate, CDate = CDate, Xvar = Xvar, 
                        CINTERVAL = "D", FIXED = FALSE)   # create new climate dataframe with continuous daynumbers, leap days are not a problem 
  
  for (i in 1:length(BDate)){
    for (j in closest:furthest){
      k <- j - closest + 1
      CMatrix[i, k] <- Xvar[match(cont$BIntNo[i] - j,cont$CIntNo)]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  }
  
  modeldat           <- model.frame(baseline)
  modeldat$temporary <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from = 1, to = nrow(CMatrix), by = 1))
  
  test <- ModelLogLikelihoodG(par = c(3, 0.2, 0), modeloutput = lm(Mass ~ temporary, data = modeldat), 
                                  duration = duration, CMatrix = CMatrix, 
                                  nullmodel = nullmodel, funcenv = funcenv)
  
  expect_false(is.na(test))
  expect_true(test<=2)
  
})

# Test Weibull function #
# Test that deltaAICc output is not NA #
# Test that deltaAICc is less than 0 #
test_that("Weibull loglik test", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  funcenv              <- environment()
  Xvar                 <- MassClimate$Temp
  BDate                <- Mass$Date
  CDate                <- MassClimate$Date
  furthest             <- 2
  closest              <- 1
  duration             <- (furthest-closest) + 1
  baseline             <- lm(Mass ~ 1, data = Mass)
  CMatrix              <- matrix(ncol = (duration), nrow = length(BDate))
  nullmodel            <- AICc(baseline)
  funcenv$MODNO        <- 1
  funcenv$DAICc        <- list()
  funcenv$par_shape    <- list()
  funcenv$par_scale    <- list()
  funcenv$par_location <- list()
  
  cont <- DateConverter(BDate = BDate, CDate = CDate, Xvar = Xvar, 
                        CINTERVAL = "D", FIXED = FALSE)   # create new climate dataframe with continuous daynumbers, leap days are not a problem 
  
  for (i in 1:length(BDate)){
    for (j in closest:furthest){
      k <- j - closest + 1
      CMatrix[i, k] <- Xvar[match(cont$BIntNo[i] - j,cont$CIntNo)]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  }
  
  modeldat           <- model.frame(baseline)
  modeldat$temporary <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from = 1, to = nrow(CMatrix), by = 1))
  
  test <- ModelLogLikelihoodW(par = c(3, 0.2, 0), modeloutput = lm(Mass ~ temporary, data = modeldat), 
                              duration = duration, CMatrix = CMatrix, 
                              nullmodel = nullmodel, funcenv = funcenv)
  
  expect_false(is.na(test))
  expect_true(test<=2)
  
})