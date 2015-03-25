\name{climwin-package}
\alias{climwin-package}
\alias{climwin}
\docType{package}
\title{
Detect and visualise a climate window 
}
\description{
Contains a number of functions used to detect a period of climate sensitivity (climate window) for a given biological response,
and visualise the detected climate window. The functions provided in climwin allow users to select meaningful
time periods over which to investigate the impacts of climate, reducing the need to choose arbitrary climate windows
(e.g. spring temperature, winter precipitation).
}
\details{
\tabular{ll}{
Package: \tab climwin\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
License: \tab GPL-2\cr
}
climwin contains a number of useful functions. The key functions are outlined below.

\code{\link{climatewin}} is used to detect a climate window for a given biological response. Requires seperate climate and biologial datasets.

\code{\link{randwin}} will randomise provided biological data and conduct a new climate window analysis. By comparing the climate window results of the randomised data and original data, users can determine if their climate window results could have occured by random chance.

\code{\link{plotall}} will generate a number of useful plots to help visualise climate window results. These plots can be invaluable for identifying a 'true' climate window.

For a complete list of functions see library(help = "climwin")
}
\author{
Liam D. Bailey and Martijn van de Pol

Maintainer: Liam D. Bailey \email{liam.bailey@anu.edu.au}
}
\references{
van de Pol & Cockburn 2011 Am Nat 177(5):698-707 (doi:
 10.1086/659101) "Identifying the critical climatic time window that affects
 trait expression"
}

\keyword{
Climate change
Climate windows
}
\examples{
\dontrun{
# Test for a fixed climate window using the datasets 'Mass' and 'MassClimate'

# Load data##

 data(Mass)
 data(MassClimate)

# Test a fixed window, starting 20 May
# Test for climate windows between 100 and 0 days ago (furthest=100, closest=0)
# Fit a linear term for the mean climate (FUNC="L")
# Test at the resolution of days (CINTERVAL="D")

clim <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, 
                   BDate = Mass$Date,
                   baseline = lm(Mass$Mass ~ 1),
                   furthest = 100, closest = 0,
                   STAT = "mean", FUNC = "L",
                   FIXED = TRUE, cutoff.day = 20, cutoff.month = 5,
                   nrandom = 0, CMISSING = FALSE, CINTERVAL = "D")
         
# Randomise biological data and re-run climate window analysis

rand <- randwin(repeats = 2,
                Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                baseline = lm(Mass$Mass ~ 1), furthest = 100, closest = 0,
                STAT = "mean", FUNC = "L", FIXED = TRUE, cutoff.day = 20, cutoff.month = 5,
                CMISSING = FALSE, CINTERVAL = "D")
       
# Visualise climate window results

plotall(Dataset = clim[[3]], DatasetRand = rand,
        BestModel = clim[[1]], BestModelData = clim[[2]],
        CW1 = 0.95, CW2 = 0.5, CW3 = 0.25, HISTQ = 0.99)
}
}
