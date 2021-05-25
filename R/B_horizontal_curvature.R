#' Horizontal Curve Curvature in Multi-lane Road(B, degree)
#'
#' This function calculates horizontal curve curvature in multi-lane road. It follows <Formula 6-1> in KHCM(2013).
#' @param density *Numeric* The density of the road(pcpkmpl)
#' @keywords LOS Level of Service Density V/C ratio
#' @export LOS Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}
#' @examples
#' DDHV(AADT = 1500, region = 'city', road = 'general', lane = 2)
#' LOS_freeway_basic(density = 30)
#' LOS_freeway_basic(design_speed = 120, v_c_ratio = 0.5)
#' LOS_freeway_basic(design_speed = 80, volume = 1000)
LOS_freeway_basic <- function(density = NULL, design_speed = NULL, volume = NULL, v_c_ratio = NULL){
}
