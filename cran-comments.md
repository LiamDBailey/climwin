---
title: "cran-comments"
author: "Liam D. Bailey"
output: html_document
---

## Test environments
* ubuntu 12.04 (on travis-ci), R 3.2.1
* win-builder (devel and release)

## R CMD check results

We discovered and fixed a serious bug in our code. Sorry to upload a new version so soon after our last.

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
  
Note caused by local .Rprofile settings.

## Downstream dependencies
There are currently no downstream dependencies for this package.
