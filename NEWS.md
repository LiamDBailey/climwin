# climwin 1.0.0

## Major changes

### Cohort variable:
When a group of biological measurements covers two years (e.g. Southern hemisphere species which breed between November - February) use of 'absolute' climate windows will cause these measurements to be split between two different reference days. To overcome this issue, we include a 'cohort' argument to our functions. 

The cohort variable will determine which biological measurements should be grouped together (e.g. measurements from the same breeding season), and ensure that these measurements share the same reference day. The cohort variable should come from the same dataset as the 'bdate' argument (i.e. variables should have equal lengths).

### Spatial variable:
Climate window analysis often requires large amounts of data to effectively determine periods of climate sensitivity. Often this is achieved through temporal replication, collecting many years of data on the same population, but can also be achieved through spatial replication, collecting data on multiple populations, or, ideally, a combination of the two. The new argument 'spatial' allows users to carry out a climate window analysis with data from multiple populations by linking each set of biological measurements to a corresponding set of climate data.

Users can include data from multiple study sites/populations in their climate dataset (i.e. the dataset used for arguments cdate and xvar), with a new site ID variable included to distinguish between different sites. Similarly, the user can add a new site ID variable to the biological dataset that can be used to link biological measurements to the corresponding climate data. When carrying out a climatewin analysis, the user can then include the argument 'spatial'. 'spatial' is a list item, containing the biological site ID variable and climate site ID variable respectively. During model fitting, the climate window analysis will link extract different climate data for each biological record based on the provided site ID.

N.B. Spatial replication in climate window analysis works on the assumption that all populations share the same period of climate sensitivity. If this is NOT the case, populations should be analysed separately.

### Cox proportional hazard models
Proportional hazard models may often be useful for climate window analyses on phenological data. We have included the ability for users to fit proportiona hazard models for the argument 'baseline' using the function coxph().  For more detail on understanding the use of proportional hazard models for phenology analysis see van de Pol & Cockburn ?????.

### New analysis metric
In previous versions of `climwin` climate windows have been compared visually using a number of metrics (e.g. deltaAICc distribution, model weights). In this newest version of `climwin` we have included a new standard metric that can allows for a standard method of distinguishing real periods of climate sensitivity in biological data. INFO ABOUT HOW IT IS DISPLAYED/PLOTTED. INFO ON WHAT IT DOES.

For more information on the effectiveness of the new metric, please see van de Pol et al. 2016 METHODS PAPER.

### 'exclude' parameter
Added 'exclude' parameter to the `climatewin` and `randwin` functions. #But we may end up removing this!!?

### Removal of cross validation
May be possible if metric is more effective than cross validation!!

## Minor changes

Argument changes:
- 'furthest' and 'closest' are now combined into a single argument 'range'
- 'cutoff.day' and 'cutoff.month' are now combined into a single argument 'refday'
- 'cvk' argument is now redundant, replaced with argument 'k'
- 'thresh' argument is now redundant, replaced with argument 'binary'
- 'type' now accepts possible arguments 'absolute' and 'relative' (rather than 'fixed' and        'variable')

# climwin 0.1.2

Fixed bug which caused convergence issues using cross-validation.

# climwin 0.1.1

Fixed serious bug causing an error in 'plotwin' and 'plotall'. Naming mismatch in the 'closest' column in climatewin$Dataset. Column name changes from Closest to closest.

# climwin 0.1.0

Our newest release aims to speed up the functions and provide greater versatility to users. In addition, we have produced a vignette providing a detailed introduction on how to use the climwin package. See `vignette("climwin")` for more.

We have made some changes to the names and levels or parameters that should be checked in the help documentation.

## Major changes

* Changes to parameter names and levels (e.g. Cinterval levels are now in the format "day", "week", "month" rather than "D", "W", "M"). Please check the help documentation of each function to familiarise yourself with the new function wording.

* Function `climatewin` (and corresponding functions) now contains parameter CVK. This provides the ability to run k-fold cross validation during climate window analysis. This provides a further check against finding strong climate signals by chance.

* Function `climatewin` (and corresponding functions) now contains parameters upper, lower and thresh. These allow users to adapt their climate data to consider climatic thresholds. This will be useful for those interested in investigating topics like growing degree or chill days.

* Function `climatewin` (and corresponding functions) now contains parameter centre. This allows users to carry out within-group centreing with their climate data. Note that within-group centreing will only test linear relationships.

* Function `climatewin` now includes functionality to test multiple parameter combinations (e.g. func = c("lin", "quad")) in succession. Please see the vignette `vignette("climwin")` for more detail.

* Function `crosswin` now includes parameter stat2. This allows users to test the correlation between two climate variables using different aggregate statistics (e.g. Mean temperature and minimum rainfall).

## Minor changes

* The parameter Xvar must now be a list item (e.g. Xvar = list(MassClimate$Temp)) 

# climwin 0.0.1

* Initial package release. Forthcoming changes will be noted.