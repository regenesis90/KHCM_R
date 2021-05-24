#' Service Flow Rate(SFi)
#'
#' This function calculates Service Flow Rate(MSFi) under basic conditions.
#' @param design_speed *Numeric* Choose one from : \code{120}, \code{100}, \code{80}
#' @param v_c_ratio_i *Numeric* V/c ratio under LOS i.
#' @param N *Numeric* The number of freeway lane(one-way lane). It must be 2 or more.
#' @param f_w *Numeric* The One Side Lane Width and Lateral Clearance Factor(f_w). It could be calculated by f_w()
#' @param f_hv *Numeric* Heavy Vehicle Factors(f_hv). See f_hv()
#' @export SFi Service Flow Rate under LOS i, given road and traffic conditions(vph)
#' @examples
#' SFi(c_j = 1400, v_c_ratio_i = 0.2, N = 6, f_w = 0.5, f_hv = 0.8)
SFi <- function(design_speed = NULL, v_c_ratio_i = NULL, N = NULL, f_w = NULL, f_hv = NULL){
  if (v_c_ratio_i >=0 & N >= 2 & f_w >=0 & f_hv >=0){
    MSFi <- MSFi(design_speed = design_speed, v_c_ratio_i = v_c_ratio_i)
    MSFi * N * f_w * f_hv
  }
}
