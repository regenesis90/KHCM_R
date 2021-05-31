#' Saturated traffic flow rate of i-lane group reflecting special circumstances (construction section, weather). vphg, S_i_backtick
#'
#' It follows <Formula 8-38> in KHCM(2013)
#' @param S_i i Saturated traffic flow rate(vphg)
#' @param f_WZ Construction impact correction factor
#' @param f_IW weather correction factor
#' @export S_i_backtick Saturated traffic flow rate of i-lane group reflecting special circumstances (construction section, weather)
#' @examples
S_i_backtick <- function(S_i = NULL, f_WZ = NULL, f_IW = NULL){
  S_i * f_WZ * f_IW
}
