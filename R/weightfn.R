weibull3 <- function(x, shape, scale, location){
  shape/scale * ((x - location)/scale)^(shape-1) * exp(-((x - location)/scale)^shape)
}