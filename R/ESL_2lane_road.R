#' Signal Intersection Length of Sphere of Influence(ESL, m)
#'
#' This function follows <Formula 7-3>
#' @param V_d *Numeric* Traffic in the direction of travel(pcph?)
#' @param LB *Categorical* Whether the left turn-only lane exist. \code{LB = 1} means exist. \code{LB = 0} means not.
#' @param g_c_ratio *Numeric* Effective green time ratio
#' @param L *Numeric* Direction Traffic Volume Left Turn Ratio
#' @param DIS *Numeric* Direction traffic volume straight forward ratio
#' @keywords
#' @export ESL_2lane_road Signal Intersection Length of Sphere of Influence(m)
#' @examples
#' ESL_2lane_road(V_d = 1000, LB = 1, g_c_ratio = 0.23, L = 0.2, DIS = 0.73)
#' ESL_2lane_road(300, 0, 0.11, 0.123, 0.84)
ESL_2lane_road <- function(V_d = NULL, LB = NULL, g_c_ratio = NULL, L = NULL, DIS = NULL){
  if (V_d >= 0 & (LB == 1 | LB == 0) & g_c_ratio >= 0 & g_c_ratio <= 1 & L >= 0 & L <= 1 & DIS >= 0 & DIS <= 1){
    result <- 242 + (74 * (V_d/100)) - (102 * LB) - (70 * ((V_d/100) * (g_c_ratio)) + (152 * ((V_d/100) * L * DIS)))
    result
  }
  else{
    result <- 'hehe'
    result
  }
}
