#' Ratio of Left Turns in Direct-Left Shared Lanes at 3-way Signalized Intersection
#'
#' Ratio of left turns in direct-left shared lanes at three-way signalized intersection
#'     This function follows <Formula 8-36> in KHCM(2013), p.246.
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @keywords Turning traffic volume ratio shared left-turning lane group signalized intersection
#' @seealso \code{\link{f_LT_sh_3si}}
#' @export P_LT_sh_3si
#' @examples
#' P_LT_sh_3si(V_L = 300, V_TH = 1000)
P_LT_sh_3si <- function(V_L = NULL, V_TH = NULL){
  if (V_L >= 0 & V_TH > 0){
    p <- V_L / (V_TH + V_L)
  }
  else {p <- 'Error : [V_L], [V_TH], must be positive(vph). Please check that.'}
  p
}
