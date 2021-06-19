#' Average Speed of Non-Weaving Traffic Flow in Expressway Weaving Section
#'
#' This function calculates the average speed of non-weaving traffic flow in expressway weaving section.(kph)
#'     It follows <Formula 3-1>, <Formula 3-2> in KHCM(2013), p.61.
#' @param design_speed Design speed of the main lane(kph)
#' @param V Total traffic flow in expressway weaving section(pcph).
#' @param V_nw Non-weaving traffic flow(pcph).
#' @param N Total number of lanes in the weaving section.
#' @param L Length of the weaving section(m).
#' @keywords average speed weaving section weaving traffic flow
#' @seealso \code{\link{W_expwy_wv_nw}}, \code{\link{S_expwy_wv_w}}, \code{\link{S_expwy_w}}
#' @export S_expwy_wv_nw \code{30 + (((design_speed + 10) - 30)/(1 + W_expwy_wv_nw(V, V_w, N, L)))}
#' @examples
#' S_expwy_wv_nw(design_speed = 120, V = 2183, V_w = 494, N = 3, L = 350)
S_expwy_wv_nw <- function(design_speed = NULL, V = NULL, V_w = NULL, N = NULL, L = NULL){
  W_nw <- W_expwy_wv_nw(V = V, V_w = V_w, N = N, L = L)
  if (is.numeric(W_nw) == TRUE){
    if (design_speed == 80 | design_speed == 100 | design_speed == 120){
      30 + (((design_speed + 10) - 30)/(1 + W_nw))
    }
    else {'Error : [design_speed] must be one of [80], [100], [120]. Please check that.'}
  }
  else {print(W_nw)}
}
