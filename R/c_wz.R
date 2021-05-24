#' Construction section capacity(c_wz, vph)
#'
#' This function calculates the construction section capacity(vph). It follows <Formula 2-10> in KHCM(2013)
#' @param design_speed *Categorical* Choose one from : \code{120}, \code{100}, \code{80} (kph)
#' @param N *Numeric* The number of freeway lane(one-way lane). It means (# of normal one-way lane) - (# of lanes closed due to construction)
#' @param f_w *Numeric* The One Side Lane Width and Lateral Clearance Factor(f_w). It could be calculated by f_w()
#' @param f_hv *Numeric* Heavy Vehicle Factors(f_hv). See f_hv()
#' @export c_wz Construction section capacity(vph)
#' @examples
#' c_wz(design_speed = 100, N = 3, f_w = 0.9, f_hv = 0.6)
#' c_wz(80, 4, 0.9, 0.85)
c_wz <- function(design_speed = NULL, N = NULL, f_w = NULL, f_hv = NULL){
  if (N >= 1 & f_w >= 0 & f_hv >= 0){
    basic_capacity <- basic_capacity_in_construction_section(design_speed)
    basic_capacity * N * f_w * f_hv
  }
}
