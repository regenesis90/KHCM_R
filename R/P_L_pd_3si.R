#' Turning Traffic Volume Ratio of the Practically Dedicated Left-turn Lane Group i at 3-way Signalized Intersection
#'
#' Turning traffic volume ratio of the practically dedicated left-turn lane group i at 3-waysignalized intersection.
#'     The ratio of left turns in the group of practically dedicated left-turn lanes.
#'     This function follows <Formula 8-21> in KHCM(2013), p.246.
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_LF Traffic Going Straight Ahead of the First Left Turn on the Public Left Turn Lane at Signalized Intersection. See \code{\link{V_LF_si}}
#' @keywords Turning traffic volume ratio practical dedicated left-turn lane signalized intersection
#' @seealso \code{\link{V_LF_pd_3si}}
#' @export P_L_pd_3si
#' @examples
#' P_L_pd_3si(V_L = 320, V_LF = 300)
P_L_pd_3si <- function(V_L = NULL, V_LF = NULL){
  if (V_L >= 0 & V_LF >= 0 & ((V_L + V_LF) > 0)){p <- V_L / (V_LF + V_L)}
  else {p <- 'Error : [V_L], [V_LF] must be positive(vph). Please check that.'}
  p
}
