#' Traffic Volume Converted to Peak Hour in Ramp-Expressway Junction.
#'
#' This function calculates traffic volume converted to peak hour passenger car traffic volume in Ramp-Expressway Junction.
#'    It follows <Formula 4-1> in KHCM(2013), p.88.
#' @param v Current or future traffic volume(vph). See \code{\link{DDHV}}.
#' @param PHF Peak Hour Factor(PHF). See \code{\link{PHF_expwy_basic}}.
#' @param f_hv Heavy Vehicle Factors. See \code{\link{f_hv_expwy_basic}}
#' @keywords traffic volume passenger car weaving section expressway
#' @details The traffic volume in the intersecting section is converted to the peak hour passenger traffic volume.
#'          The PHF and the composition ratio of heavy vehicles are investigated on site,
#'          and the passenger car conversion coefficient is converted into the peak hour passenger traffic volume by using the value suggested in the basic section of expressway.
#' @seealso \code{\link{PHF_expwy_basic}}, \code{\link{f_hv_expwy_basic}},
#' @export V_P_expwy_rpjt \code{v/(PHF * f_hv)}(pcph)
#' @examples
#' V_P_expwy_wv(v = 1530, PHF = 0.95, f_hv = 0.588)
V_P_expwy_wv <- function(v = NULL, PHF = NULL, f_hv = NULL){
  if (v > 0){
    if (PHF >= 0 & PHF < 1){
      if (f_hv > 0){res <- v/(PHF * f_hv)}
      else {res <- 'Error : [f_hv] must be positive. Please check that. See [f_hv_expwy_basic()].'}
    }
    else {res <- 'Error : [PHF] must be positive, and less than 1. Please check that. See [PHF_expwy_basic()].'}
  }
  else {res <- 'Error : [v] must be positive(vph). Please check that. See [DDHV()].'}
  res
}
