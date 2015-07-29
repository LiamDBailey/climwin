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