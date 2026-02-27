#' Forest growth derivative
#' @param Time time since start
#' @param C forest size (kg Carbon)
#' @param parms - as list with four values: r, g, K, thresh
#' @param r exponential growth rate before canopy closure
#' @param g linear growth rate after canopy closure
#' @param K carrying capacity (kg Carbon)
#' @param thresh canopy closure threshold (kg Carbon)
#' @return derivative of forest size with time

dforestgrowth <- function(Time, C, parms) {
  
  if (C < parms$thresh) {
    
    dC <- parms$r * C
    
  } else {
    
    dC <- parms$g * (1 - C / parms$K)
    
  }
  
  return(list(dC))
  
}