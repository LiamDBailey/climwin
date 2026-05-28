#' Blue tit body mass
#'
#' Annual body mass measurements of blue tit (\emph{Cyanistes caeruleus})
#' nestlings, used as the biological response variable in package examples.
#'
#' @format A data frame with 47 rows and 3 columns:
#' \describe{
#'   \item{Date}{Measurement date, character string in DD/MM/YYYY format.}
#'   \item{Mass}{Nestling body mass (g).}
#'   \item{Age}{Nestling age (days) at measurement.}
#' }
#' @seealso [MassClimate] for the corresponding climate data.
"Mass"

#' Daily climate data for blue tit body mass analysis
#'
#' Daily rainfall and temperature records spanning 1965–2012, paired with
#' the [Mass] dataset for sliding and weighted window analyses.
#'
#' @format A data frame with 17532 rows and 3 columns:
#' \describe{
#'   \item{Date}{Date, character string in DD/MM/YYYY format.}
#'   \item{Rain}{Daily rainfall (mm).}
#'   \item{Temp}{Daily mean temperature (°C).}
#' }
#' @seealso [Mass] for the corresponding biological response data.
"MassClimate"

#' Great tit reproductive success
#'
#' Binary reproductive success records for great tit (\emph{Parus major})
#' breeding attempts, used as the biological response variable in
#' multi-population and spatial examples.
#'
#' @format A data frame with 1619 rows and 5 columns:
#' \describe{
#'   \item{Offspring}{Binary reproductive success (1 = successful, 0 = failed).}
#'   \item{Date}{Breeding date, character string in DD/MM/YYYY format.}
#'   \item{Order}{Laying order within the season.}
#'   \item{BirdID}{Individual bird identifier.}
#'   \item{Cohort}{Breeding cohort (year).}
#' }
#' @seealso [OffspringClimate] for the corresponding climate data.
"Offspring"

#' Daily climate data for great tit reproductive success analysis
#'
#' Daily rainfall and temperature records spanning 2007–2015, paired with
#' the [Offspring] dataset.
#'
#' @format A data frame with 2588 rows and 3 columns:
#' \describe{
#'   \item{Date}{Date, character string in DD/MM/YYYY format.}
#'   \item{Rain}{Daily rainfall (mm).}
#'   \item{Temperature}{Daily mean temperature (°C).}
#' }
#' @seealso [Offspring] for the corresponding biological response data.
"OffspringClimate"

#' Benchmark results comparing old and new sliding window implementations
#'
#' Timing and memory benchmarks from the \pkg{bench} package comparing the
#' previous and current \code{run_slidingwin()} implementations across
#' increasing window sizes. Used in package vignettes to illustrate
#' performance improvements.
#'
#' @format A data frame with 12 rows and 10 columns:
#' \describe{
#'   \item{expression}{Implementation label: \code{"old"} or \code{"new"}.}
#'   \item{min}{Minimum execution time.}
#'   \item{median}{Median execution time.}
#'   \item{itr.sec}{Iterations per second.}
#'   \item{mem_alloc}{Memory allocated per iteration.}
#'   \item{gc.sec}{Garbage collections per second.}
#'   \item{n_itr}{Number of successful iterations.}
#'   \item{n_gc}{Number of garbage collections.}
#'   \item{total_time}{Total benchmark duration.}
#'   \item{window_size}{Climate window size tested (days).}
#' }
benchmark_climwin <- function(){
  
  if (!require("bench")){
    stop("This data is generated using 'bench' package")
  }
  
  window_sizes <- c(25, 50, 75, 100, 150, 200)
  simple_mods <- purrr::map_df(.x = window_sizes, .f = \(max_range){
    
    print(paste0("Benchmarking: ", max_range))
    
    output <- bench::mark(old = slidingwin(xvar = list(Temp = MassClimate$Temp),
                                           cdate = MassClimate$Date, 
                                           bdate = Mass$Date, 
                                           baseline = lm(Mass ~ 1, data = Mass),
                                           range = c(max_range, 0), 
                                           type = "absolute", refday = c(20, 5),
                                           stat = "mean", 
                                           func = c("lin"), cmissing = FALSE, cinterval = "day"),
                          new = run_slidingwin(xvar = "Temp",
                                               cdate = "Date",
                                               bdate = "Date", 
                                               bio_data = Mass,
                                               baseline = lm(Mass ~ climate, data = bio_data),
                                               range = 0:max_range,
                                               type = "absolute", refday = c(20, 5),
                                               fn = mean, cinterval = "day",
                                               climate_data = MassClimate), check = FALSE) |> 
      select(expression:total_time) |> 
      mutate(expression = as.character(expression),
             across(.cols = c(min, median, total_time), .fns = as.numeric),
             mem_alloc = as.numeric(mem_alloc),
             max_range = max_range)
    
  })
  
  if (!require("lme4")){
    stop("Need 'lme4' for complex mods")
  }
  
  complex_mods <- purrr::map_df(.x = window_sizes, .f = \(max_range){
    
    output <- bench::mark(old = slidingwin(xvar = list(Temp = OffspringClimate$Temperature),
                                           cdate = OffspringClimate$Date, 
                                           bdate = Offspring$Date, 
                                           baseline = glmer(Offspring ~ 1 + (1|Cohort), data = Offspring),
                                           range = c(max_range, 0), 
                                           type = "relative",
                                           stat = "mean", 
                                           func = c("quad"), cmissing = FALSE, cinterval = "day"),
                          new = run_slidingwin(xvar = "Temperature",
                                               cdate = "Date",
                                               bdate = "Date", 
                                               bio_data = Offspring,
                                               baseline = glmer(Offspring ~ poly(climate, 2) + (1|Cohort), family = "binomial", data = bio_data),
                                               range = 0:max_range,
                                               type = "relative",
                                               fn = mean, cinterval = "day",
                                               climate_data = OffspringClimate),
                          check = FALSE) |> 
      select(expression:total_time) |> 
      mutate(expression = as.character(expression),
             across(.cols = c(min, median, total_time), .fns = as.numeric),
             mem_alloc = as.numeric(mem_alloc),
             max_range = max_range)
    
  })
  
  bench_results <- dplyr::bind_rows(simple_mods |> mutate(mod = "simple"),
                                    complex_mods |> mutate(mod = "complex"))
  usethis::use_data(bench_results, internal = TRUE)
  
}

#' Benchmark randwin with parallel processing
#'
#' Timing and memory benchmarks from the \pkg{bench} package comparing the
#' previous and current \code{run_slidingwin()} implementations across
#' increasing window sizes. Used in package vignettes to illustrate
#' performance improvements.
#'
#' @format A data frame with 12 rows and 10 columns:
#' \describe{
#'   \item{expression}{Implementation label: \code{"old"} or \code{"new"}.}
#'   \item{min}{Minimum execution time.}
#'   \item{median}{Median execution time.}
#'   \item{itr.sec}{Iterations per second.}
#'   \item{mem_alloc}{Memory allocated per iteration.}
#'   \item{gc.sec}{Garbage collections per second.}
#'   \item{n_itr}{Number of successful iterations.}
#'   \item{n_gc}{Number of garbage collections.}
#'   \item{total_time}{Total benchmark duration.}
#'   \item{window_size}{Climate window size tested (days).}
#' }
benchmark_randwin <- function(){
  
  if (!require("bench")){
    stop("This data is generated using 'bench' package")
  }
  
  n <- c(100, 150, 200)
  randwin <- purrr::map_df(.x = n, .f = \(repeats){
    
    print(paste0("Benchmarking: ", max_range))
    
    output <- bench::mark(nonparallel = run_randwin(repeats = repeats, xvar = "Temp",
                                               cdate = "Date",
                                               bdate = "Date", 
                                               bio_data = Mass,
                                               baseline = lm(Mass ~ climate, data = bio_data),
                                               range = 0:100,
                                               type = "absolute", refday = c(20, 5),
                                               fn = mean, cinterval = "day",
                                               climate_data = MassClimate),
                          parallel = run_randwin(repeats = repeats, xvar = "Temp",
                                                    cdate = "Date",
                                                    bdate = "Date", 
                                                    bio_data = Mass,
                                                    baseline = lm(Mass ~ climate, data = bio_data),
                                                    range = 0:100,
                                                    type = "absolute", refday = c(20, 5),
                                                    fn = mean, cinterval = "day",
                                                    climate_data = MassClimate, parallel = TRUE),
                          check = FALSE) |> 
      select(expression:total_time) |> 
      mutate(expression = as.character(expression),
             across(.cols = c(min, median, total_time), .fns = as.numeric),
             mem_alloc = as.numeric(mem_alloc),
             n = repeats)
    
  })
  
  if (!require("lme4")){
    stop("Need 'lme4' for complex mods")
  }
  
  complex_mods <- purrr::map_df(.x = window_sizes, .f = \(max_range){
    
    output <- bench::mark(old = slidingwin(xvar = list(Temp = OffspringClimate$Temperature),
                                           cdate = OffspringClimate$Date, 
                                           bdate = Offspring$Date, 
                                           baseline = glmer(Offspring ~ 1 + (1|Cohort), data = Offspring),
                                           range = c(max_range, 0), 
                                           type = "relative",
                                           stat = "mean", 
                                           func = c("quad"), cmissing = FALSE, cinterval = "day"),
                          new = run_slidingwin(xvar = "Temperature",
                                               cdate = "Date",
                                               bdate = "Date", 
                                               bio_data = Offspring,
                                               baseline = glmer(Offspring ~ poly(climate, 2) + (1|Cohort), family = "binomial", data = bio_data),
                                               range = 0:max_range,
                                               type = "relative",
                                               fn = mean, cinterval = "day",
                                               climate_data = OffspringClimate),
                          check = FALSE) |> 
      select(expression:total_time) |> 
      mutate(expression = as.character(expression),
             across(.cols = c(min, median, total_time), .fns = as.numeric),
             mem_alloc = as.numeric(mem_alloc),
             max_range = max_range)
    
  })
  
  bench_results <- dplyr::bind_rows(simple_mods |> mutate(mod = "simple"),
                                    complex_mods |> mutate(mod = "complex"))
  usethis::use_data(bench_results, internal = TRUE)
  
}
