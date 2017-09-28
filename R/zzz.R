.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.5) return()
  
  intro <- c("To learn how to use climwin see our vignette. \n",
             "climwin v1.2.0 has added new features. See help documentation and release notes for more details."
  )
  packageStartupMessage(intro)
}
