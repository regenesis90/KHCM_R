#' Average control delay per vehicle of diamond-shaped interchange I (sec/unit)
#'
#' It follows <Formula 9-10> in KHCM(2013) p.434
#' @param d_i Average control delay per vehicle of the type point movement flow i(sec/unit)
#' @param V_i Corrected traffic volume (vph) of the moving flow i of the end point
#' @export d_I
#' @examples
d_I <- function(d_i = NULL, V_i = NULL){
  dvsum <- 0
  vsum <- 0
  for (i in 1:length(d_i)){
    dv <- d_i[i] * V_i[i]
    v <- V_i[i]
    vsum <- vsum + v
    dvsum <- dvsum + dv
  }
  dvsum / vsum
}
