#' Chick body mass data since 1979.
#' 
#' Artificially generated data representing
#' average body mass of bird chicks since 1979.
#' @format A data frame with 47 rows and 2 variables
#' \describe{
#'   \item{Date}{Date of mass measurements (dd/mm/yyyy).}
#'   \item{Mass}{Annual average body mass in grams.}
#'   \item{Age}{Annual average age of mother in years.}
#'   }
#'@name Mass
NULL

#' Daily climate data since 1979.
#' 
#' Daily temperature and rainfall data since 1979.
#' @format A data frame with 17,532 rows and 3 variables.
#' \describe{
#'   \item{Date}{Date when climate data was recorded (dd/mm/yyyy).}
#'   \item{Rain}{Daily rainfall data in mm.}
#'   \item{Temp}{Daily temperature data in degrees centigrade.}
#'   }
#'@name MassClimate
NULL

#' Example output dataframe from function slidingwin.
#' 
#' Output file from \code{\link{slidingwin}} using temperature and body mass data. 
#' Generated with \code{\link{Mass}} and \code{\link{MassClimate}} dataframes.
#' @format A data frame with 5,151 rows and 19 variables.
#' \describe{
#'   \item{deltaAICc}{Difference between model AICc of fitted climate window and a null model containing no climate.}
#'   \item{WindowOpen}{The start day of each tested climate window. Furthest from the biological record.}
#'   \item{WindowClose}{The end day of each tested climate window. Closest to the biological record.}
#'   \item{ModelBeta}{Beta estimate of the relationship between temperature and mass.}
#'   \item{Std.Error}{Standard error term for linear model betas.}
#'   \item{ModelBetaQ}{Quadratic beta estimate of the relationship between temperature and mass.}
#'   \item{ModelBetaC}{Cubic beta estimate of the relationship between temperature and mass.}
#'   \item{ModelInt}{Model intercept.}     
#'   \item{Function}{The function used to fit climate (e.g. linear ("lin"), quadratic ("quad"))}
#'   \item{Furthest}{Furthest day back considered in slidingwin.}
#'   \item{Closest}{Closest day back considered in slidingwin.}
#'   \item{Statistics}{The aggregate statistic used to analyse climate (e.g. mean, max, slope).}
#'   \item{Type}{Whether "absolute" or "relative" climate windows were tested.}
#'   \item{K}{Number of folds used for k-fold cross validation.}
#'   \item{ModWeight}{Model weight of each fitted climate window.}
#'   \item{sample.size}{Sample size (i.e. number of years or sites) used for climate window analysis.}
#'   \item{Reference.day,Reference.month}{If type is "absolute", the date from which the climate window was tested.}
#'   \item{Randomised}{Whether the data was generated using \code{\link{slidingwin}} or \code{\link{randwin}}.}
#'   }
#'@name MassOutput
NULL


#' Example output dataframe from function randwin.
#' 
#' Output file from function \code{\link{randwin}} using temperature and mass data. 
#' Generated with \code{\link{Mass}} and \code{\link{MassClimate}} dataframes.
#' @format A data frame with 5 rows and 21 variables.
#' \describe{
#'   \item{deltaAICc}{Difference between model AICc of fitted climate window and a null model containing no climate.}
#'   \item{WindowOpen}{The start day of each tested climate window. Furthest from the biological record.}
#'   \item{WindowClose}{The end day of each tested climate window. Closest to the biological record.}
#'   \item{ModelBeta}{Beta estimate of the relationship between temperature and mass.}
#'   \item{Std.Error}{Standard error term for linear model betas.}
#'   \item{ModelBetaQ}{Quadratic beta estimate of the relationship between temperature and mass.}
#'   \item{ModelBetaC}{Cubic beta estimate of the relationship between temperature and mass.}
#'   \item{ModelInt}{Model intercept.}     
#'   \item{Function}{The function used to fit climate (e.g. linear ("lin"), quadratic ("quad"))}
#'   \item{Furthest}{Furthest day back considered in slidingwin.}
#'   \item{Closest}{Closest day back considered in slidingwin.}
#'   \item{Statistics}{The aggregate statistic used to analyse climate (e.g. mean, max, slope).}
#'   \item{Type}{Whether "fixed" or "variable" climate windows were tested.}
#'   \item{K}{Number of folds used for k-fold cross validation.}
#'   \item{ModWeight}{Model weight of each fitted climate window.}
#'   \item{sample.size}{Sample size (i.e. number of years or sites) used for climate window analysis.}
#'   \item{Reference.day,Reference.month}{If type is "absolute", the date from which the climate window was tested.}
#'   \item{Randomised}{Whether the data was generated using \code{\link{slidingwin}} or \code{\link{randwin}}.}
#'   \item{Repeat}{The number of randomisations carried out.}
#'   \item{WeightDist}{Model spread of 95 percent confidence set of models.}
#'   }
#'@name MassRand
NULL
   
#' Reproductive success of birds since 2009.
#' 
#' Artificially generated data representing
#' reproductive success of birds since 2009.
#' @format A data frame with 1,619 rows and 5 variables.
#' \describe{
#'   \item{Offspring}{Total number of offspring produced.}
#'   \item{Date}{Date of hatching (dd/mm/yyyy).}
#'   \item{Order}{Order of nest within each season.}
#'   \item{BirdID}{Individual ID of female.}
#'   \item{Cohort}{Grouping factor designating the breeding season
#'                  of each record.}
#'    }
#'@name Offspring
NULL

#' Daily climate data since 2009.
#' 
#' Daily temperature and rainfall data since 2009.
#' Coincides with biological data from \code{\link{Offspring}}.
#' @format A data frame with 2,588 rows and 3 variables.
#' \describe{
#'   \item{Date}{Date when climate was recorded (dd/mm/yyyy).}
#'   \item{Rain}{Daily rainfall data in mm.}
#'   \item{Temperature}{Daily temperature data in degrees centigrade.}
#'   }
#'@name OffspringClimate
NULL

#' Average size of red winged fairy wren (Malurus elegans) chicks.
#' 
#' Average size (using standardised measures of tarsus length,
#' head-bill length and wing length) in red winged fairy wren
#' (Malurus elegans) chicks. Measured over 7 years.
#' @format A data frame with 1,003 rows and 5 variables.
#' \describe{
#'   \item{NestID}{Unique nest identifier.}
#'   \item{Cohort}{Year of breeding season.}
#'   \item{Helpers}{Total number of non-breeding helpers at the nest.}
#'   \item{Size}{Average offspring size.}
#'   \item{Date}{Date when offspring size was recorded (dd/mm/yyyy).}
#'   }
#'@name Size
NULL

#' Daily climate data from 2006 to 2015.
#' 
#' Average, maximum and minimum daily temperature data, average rainfall data 
#' from 2006 to 2015. 
#' Coincides with biological data from \code{\link{Size}}.
#' @format A data frame with 3,411 rows and 5 variables.
#' \describe{
#'   \item{Date}{Date when climate was recorded (dd/mm/yyyy).}
#'   \item{Rain}{Average daily rainfall data in mm.}
#'   \item{Temperature}{Average daily temperature data in degrees centigrade.}
#'   \item{MaxTemp}{Maximum daily temperature in degrees centigrade.}
#'   \item{MinTemp}{Minimum daily temperature in degrees centigrade.}
#'   }
#'@name SizeClimate
NULL

#' Annual laying date of breeding common chaffinch (Fringilla coelebs).
#' 
#' Average annual laying date of common chaffinch (Fringilla coelebs)
#' measured over 47 years.
#' @format A data frame with 47 rows and 3 variables.
#' \describe{
#'   \item{Year}{Year of laying date measurement.}
#'   \item{Date}{Average date of measurement.}
#'   \item{Laydate}{Average annual laying date in days after January 1st.}
#'   }
#'@name Chaff
NULL

#' Daily climate data from 1965 to 2012.
#' 
#' Maximum daily temperature and average rainfall data 
#' from 1965 to 2012. Coincides with biological data 
#' from \code{\link{Chaff}}.
#' @format A data frame with 17,520 rows and 3 variables.
#' \describe{
#'   \item{Date}{Date when climate was recorded (dd/mm/yyyy).}
#'   \item{Rain}{Average daily rainfall data in mm.}
#'   \item{Temp}{Maximum daily temperature in degrees centigrade.}
#'   }
#'@name ChaffClim
NULL

#' Monthly temperature data
#' 
#' Artificially generated temperature data
#' at a monthly scale. Used for code testing.
#' @format A data frame with 576 rows and 2 variables
#' \describe{
#'   \item{Date}{Date of temperature measurements (dd/mm/yyyy).}
#'   \item{Temp}{Mean monthly temperature}
#'   }
#'@name Monthly_data
NULL