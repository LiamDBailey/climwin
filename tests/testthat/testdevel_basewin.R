test_that("devel produces the same output as original data with lm", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  #Linear terms
  
  y <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                  bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                  range = c(10, 0),
                  stat = "max", func = "lin",
                  type = "absolute", refday = c(20, 5),
                  cmissing = FALSE, cinterval = "day")
  
  
  x <- devel_slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                        bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                        range = c(10, 0),
                        stat = "max", func = "lin",
                        type = "absolute", refday = c(20, 5),
                        cmissing = FALSE, cinterval = "day")
  
  expect_equal(y[[1]]$Dataset$deltaAICc, x[[1]]$Dataset$deltaAICc)
  expect_equal(y[[1]]$Dataset$ModelBeta, x[[1]]$Dataset$ModelBeta)
  
  #Quadratic terms
  
  y <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                  bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                  range = c(10, 0),
                  stat = "max", func = "quad",
                  type = "absolute", refday = c(20, 5),
                  cmissing = FALSE, cinterval = "day")
  

  x <- devel_slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                        bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                        range = c(10, 0),
                        stat = "max", func = "quad",
                        type = "absolute", refday = c(20, 5),
                        cmissing = FALSE, cinterval = "day")
  
  expect_equal(y[[1]]$Dataset$deltaAICc, x[[1]]$Dataset$deltaAICc)
  expect_equal(y[[1]]$Dataset$ModelBeta, x[[1]]$Dataset$ModelBeta)
  expect_equal(y[[1]]$Dataset$ModelBetaQ, x[[1]]$Dataset$ModelBetaQ)
  expect_equal(y[[1]]$Dataset$Std.ErrorQ, x[[1]]$Dataset$Std.ErrorQ)
  
})

test_that("devel produces the same output as original data with lme4", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  #Linear terms
  
  y <- slidingwin(xvar = list(Temp = OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                  bdate = Offspring$Date, baseline = lmer(Offspring ~ 1 + (1|Order), data = Offspring),
                  range = c(10, 0),
                  stat = "max", func = "lin",
                  type = "relative",
                  cmissing = FALSE, cinterval = "day")
  
  
  x <- devel_slidingwin(xvar = list(Temp = OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                        bdate = Offspring$Date, baseline = lmer(Offspring ~ 1 + (1|Order), data = Offspring),
                        range = c(10, 0),
                        stat = "max", func = "lin",
                        type = "relative",
                        cmissing = FALSE, cinterval = "day")
  
  expect_equal(y[[1]]$Dataset$deltaAICc, x[[1]]$Dataset$deltaAICc)
  expect_equal(y[[1]]$Dataset$ModelBeta, x[[1]]$Dataset$ModelBeta)
  
  #Quadratic terms
  
  y <- slidingwin(xvar = list(Temp = OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                  bdate = Offspring$Date, baseline = lmer(Offspring ~ 1 + (1|Order), data = Offspring),
                  range = c(10, 0),
                  stat = "max", func = "quad",
                  type = "relative",
                  cmissing = FALSE, cinterval = "day")
  
  
  x <- devel_slidingwin(xvar = list(Temp = OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                        bdate = Offspring$Date, baseline = lmer(Offspring ~ 1 + (1|Order), data = Offspring),
                        range = c(10, 0),
                        stat = "max", func = "quad",
                        type = "relative",
                        cmissing = FALSE, cinterval = "day")
  
  expect_equal(y[[1]]$Dataset$deltaAICc, x[[1]]$Dataset$deltaAICc)
  expect_equal(y[[1]]$Dataset$ModelBeta, x[[1]]$Dataset$ModelBeta)
  expect_equal(y[[1]]$Dataset$ModelBetaQ, x[[1]]$Dataset$ModelBetaQ)
  expect_equal(y[[1]]$Dataset$Std.ErrorQ, x[[1]]$Dataset$Std.ErrorQ)
  
})
