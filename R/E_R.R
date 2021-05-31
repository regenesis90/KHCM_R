#' Percentage of crosswalk signal time for which a right turn is not available (E_R)
#'
#' This function follows <Table 8-13> in KHCM(2013)
#' @param dowry *Categorical* Choose one from : \code{'yes'}, \code{'no'}
#' @param V_R *Numeric* Corrected Right Turn Traffic (vph)
#' @param V_TH *Numeric* straight-through traffic (vph)
#' @param f_c *Numeric* Proportion of time to obstruct right turns among pedestrian crossing signals
#' @param G_p *Numeric* Pedestrian crossing signal (sec.)
#' @param C *Numeric* Signal Cycle(seconds)
#' @param L_H *Numeric* Roadside friction (seconds) due to entry and exit of the back road, bus stop, and on-street parking
#' @param N *Categorical* Number of lanes. Choose one from : \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, \code{6}
#' @keywords
#' @export E_R
#' @examples
#'E_R(dowry = 'yes', V_R = 320, V_TH = 280, f_c = 0.2, G_p = 40, C = 200, L_H = 30.22, N = 4)
E_R <- function(dowry = NULL, V_R = NULL, V_TH = NULL, f_c = NULL, G_p = NULL, C = NULL, L_H = NULL, N = NULL){
  if (is.null(dowry) == FALSE &  V_R >= 0 & V_TH >= 0 & f_c >= 0 & G_p >= 0 & C >= 0 & L_H >= 0 & is.null(N) == FALSE){
    if (dowry == 'yes'){e <- 1.16 + L_H /(1.63 * V_R)}
    if (dowry == 'no'){
      if (N == 1 | N == 2 | N == 3 | N == 4 | N == 6){N_T <- N}
      if (N == 5){N_T <- N - 1}
      e <- 1.16 + (2200/V_R) * ((f_c * G_p / C) + (L_H / 3600) - ((1.63 * V_TH)/(C * N_T * V_R)))}
    e
  }
}
