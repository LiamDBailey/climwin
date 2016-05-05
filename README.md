# climwin

climwin is a package in R designed for those interested in understanding the impacts of climate, particularly on biological systems. When seeking to understand the effects of climate it is neccesary to select a sampling period over which climate is recorded, a climate window. Often this choice is made arbitrarily, with many studies using seasonal values (e.g. spring temperature, winter precipitation). However, these climate windows may not be the most relevant for the biological system in question, and if we fail to find a relationship between climate and the biological response it can be difficult to determine whether this is due to climate insensitivity in the biological response or if the choice of climate window is flawed.

Rather than being required to make a single arbitrary choice of climate window, climwin allows users to test the effectiveness of a wide range of possible climate windows with the aim of identifying the most appropriate climate window for further use. climwin gives users to ability to visualise the results of their climate window analysis, using ggplot2, as a means to best interpret and understand the climate window results.

To install:

* latest released version: `install.packages("climwin")`


How to use climwin:

Examples for both climate window analysis and plotting are provided in the package help documentation. See `library(help = "climwin")` once installed for more detail.

For more detailed insight on how to use climwin to investigate climate data see our introductory vignette using `vignette("climwin")`, or our more advanced vignette with `vignette("advanced_climwin")`.
