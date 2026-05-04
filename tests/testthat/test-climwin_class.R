test_that("get* methods from climwin work as expected...", {
  
  ## Create a climwin object from `run_slidingwin`
  data("MassClimate")
  data("Mass")
  results <- run_slidingwin(range = 0:2, 
                         climate_data = MassClimate, 
                         bio_data = Mass,
                         basemodel = lm(Mass ~ climate, data = bio_data))
  
  ## Use methods to extract dataset and bestmodel
  expect_identical(results@dataset, getDataset(results))
  expect_identical(results@bestModel$model, getBestModel(results))
  expect_identical(results@bestModel$data, getBestModelData(results))
  
})

test_that("Generic plot method should return patchwork...", {
  
  ## Create a climwin object from `run_slidingwin`
  data("MassClimate")
  data("Mass")
  results <- run_slidingwin(range = 0:2, 
                            climate_data = MassClimate, 
                            bio_data = Mass,
                            basemodel = lm(Mass ~ climate, data = bio_data))
  
  plot_output <- plot(results)
  expect_true(inherits(plot_output, "patchwork"))
  
})
