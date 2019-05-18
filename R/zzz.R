.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.5) return()
  
  intro <- c("To learn how to use climwin see our vignette. \n",
             "See help documentation and release notes for details on changes."
  )
  packageStartupMessage(intro)
}
