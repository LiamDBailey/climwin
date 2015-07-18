---
title: "cran-comments"
author: "Liam D. Bailey"
output: html_document
---

## Test environments
* ubuntu 12.04 (on travis-ci), R 3.2.1
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking R code for possible problems ... NOTE
autowin: no visible binding for global variable 'modweights'
basewin: no visible binding for global variable 'modweights'
singlewin: no visible binding for global variable 'modweights'

Note caused by environment searching patterns in function update().

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
  
Note caused by local .Rprofile settings.

## Downstream dependencies
There are currently no downstream dependencies for this package.
