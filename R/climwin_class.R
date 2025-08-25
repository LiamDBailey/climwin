## Try creating our own climwin object class
## We use S7 as this seems to be the newest option (e.g. in ggplot)
library(S7)
climwin <- S7::new_class("climwin", properties = list(
  dataset = class_data.frame,
  bestModel = class_list,
  range = class_numeric
))
S7::method(print, climwin) <- function(x) {
  print(head(x@dataset))
}
S7::method(plot, climwin) <- function(x, ...) {
  plot_slidingwin(x, ...)
}
getDataset <- new_generic("getDataset", "x")
S7::method(getDataset, climwin) <- function(x) {
  x@dataset
}
getBestModel <- new_generic("getBestModel", "x")
S7::method(getBestModel, climwin) <- function(x) {
  x@bestModel
}
bestModelData <- new_generic("bestModelData", "x")
S7::method(bestModelData, climwin) <- function(x) {
  x@bestModelData
}