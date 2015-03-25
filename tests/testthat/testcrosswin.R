# Test the outcome of crosswin #

# Test that crosswin has created a correct CrossWinOutput object
# Expect that an object CrossWinOutput exists
# Expect that there are no NA values
# Expect that the number of columns is at least 7 (will vary with values of FIXED) 
# Expect that the number of rows is equal to the number of possible windows
test_that("crosswin produces output", {
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 1
  STAT = "max"
  
  test <- crosswin(Xvar = MassClimate$Temp, Xvar2 = MassClimate$Rain, CDate = MassClimate$Date,
           BDate = Mass$Date, furthest = 2, closest = 1, 
           STAT = "max", FIXED = FALSE,
           CMISSING = FALSE, CINTERVAL = "D")
  
  MaxMODNO <- 0
  duration <- (furthest-closest) + 1
  for (m in closest:furthest){
    for (n in 1:duration){
      if ((m-n)>=(closest-1)){  
        if (STAT!="slope" || n>1){
          MaxMODNO <- MaxMODNO + 1
        }
      }
    }
  }
  
  expect_true(is.data.frame(test))
  expect_equal(length(which(is.na(test))), 0)
  expect_true(ncol(test) >= 7)
  expect_equal(MaxMODNO, nrow(test))
  
})