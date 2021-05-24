#' Maximum Service Flow Rate(MSFi)
#'
#' This function calculates Maximum Service Flow Rate(MSFi) under basic conditions.
#' @param design_speed *Numeric* Choose one from : \code{120}, \code{100}, \code{80}
#' @param v_c_ratio_i *Numeric* V/c ratio under LOS i. It must be more than 0.
#' @export MSFi \code{c_j * v_c_ratio_i} Maximum Service Flow Rate in LOS i(pcphpl)
#' @examples
#' MSFi(c_j = 1400, v_c_ratio_i = 0.2)
MSFi <- function(design_speed = NULL, v_c_ratio_i = NULL){
  if (v_c_ratio_i >=0){
    if (design_speed == 120){c_j <- 2300}
    if (design_speed == 100){c_j <- 2200}
    if (design_speed == 80){c_j <- 2000}
    c_j * v_c_ratio_i
  }
}
