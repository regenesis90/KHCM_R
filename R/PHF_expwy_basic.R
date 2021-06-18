#' Peak Hour Factor in Basic Section of Expressway.
#'
#' This function calculates the peak hour factor(PHF) in basic section of expressway.
#'     It follows the formula in KHCM(2013), p.23.
#' @param V_60 Peak hour traffic volume(vph)
#' @param V_15 The largest 15-minute traffic volume observed during peak hours(veh/15minutes)
#' @export PHF_expwy_basic \code{V_60/(V_15 * 4)}
#' @examples
#' PHF_expwy_basic(V_60 = 2200, V_15 = 1000)
PHF_expwy_basic <- function(V_60 = NULL, V_15 = NULL){
  if (V_60 >= 0){
    if (V_15 >= 0){res <- V_60 / (V_15 * 4)}
    else {res <- 'Error : [V_15] must be positive(vph). Please check that.'}
  }
  else {res <- 'Error : [V_60] must be positive(vph). Please check that.'}
  res
}
