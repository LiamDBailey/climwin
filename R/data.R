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
"bench_results"
