# climwin

A package for analyzing climate data and calculating temperature means across different time windows.

## Installation

You can install the development version of climwin from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/climwin")
```

## Example

This is a basic example which shows you how to use the main function:

``` r
library(climwin)

# Calculate mean temperatures for ranges from 0 to 100 days
result <- calculate_temp_means(0:100)

# View the results
print(result)
```

## Function Documentation

The main function `calculate_temp_means()` takes a range parameter that specifies the number of days to look back from the reference date (January 1st, 1979). It returns a data frame containing:

- Start_Date: The beginning date of each range
- End_Date: The reference date (1979-01-01)
- Mean_Temperature: The mean temperature for the specified range

For more detailed documentation, see the function help page:

``` r
?calculate_temp_means
``` 