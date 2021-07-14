#' Straight-line Conversion Factor of the Left Turn Itself at the Signalized Intersection.
#'
#' The straight-line conversion factor of the left turn itself at the signalized intersection.
#'     This function follows <Formula 8-6> in KHCM(2013)
#' @param type_left Type of left lane. Choose one from: \code{'dedicated'}, \code{'public'}
#' @param N_left Number of left lane. Choose one from: \code{1}, \code{2}
#' @param si_left Method of left signal operation. Choose one from: \code{'two-way-protection'}, \code{'direct-left'}, \code{'unprotected'}
#' @param N Number of access lanes (excluding dedicated left-turn lanes)
#' @param V_O Opposite traffic volume(vph)
#' @param V_L Left turn traffic volume(vph)
#' @param V_TH Straight-through traffic volume(vph)
#' @param C Period(s)
#' @param g_c_ratio Effective green hour rate
#' @keywords straight-line conversion factor left turn signalized intersection
#' @seealso \code{\link{P_si}}, \code{\link{E_L_si}}
#' @details
#'     * Even if the left lane is an exclusive left turn lane, if the right lane is shared, both lanes are considered to be shared.
#'     * When the radius of the left turn is more than 20m and there is no U-turn,
#'     the average straight-forward conversion factor for left turns varies depending on the number of lanes available
#'     for left turns and the method of signal operation. When operated with unprotected left turns,
#'     this average straight-forward conversion factor varies depending on the amount of traffic going straight ahead,
#'     traffic volume turning left, the number of lanes and the green hour ratio.
#' @export E_1_si The straight-line conversion factor of the left turn itself
#' @examples
#' E_1_si(type_left = 'dedicated', N_left = 2, N = 2, si_left = 'direct-left', V_O = 100, V_L = 111, V_TH = 300, C = 120, g_c_ratio = 0.25)
E_1_si <- function(type_left = NULL, N_left = NULL, si_left = NULL, N = NULL, V_O = NULL, V_L = NULL, V_TH = NULL, C = NULL, g_c_ratio = NULL){
  if (V_O > 0 & V_L > 0 & V_TH > 0){
    if (C > 0){
      if (g_c_ratio > 0 & g_c_ratio < 1){
        if (N > 0){
          if (type_left == 'dedicated'){
            if (N_left == 1){
              if (si_left == 'two-way-protection'){e1 <- 1}
              else if (si_left == 'direct-left'){e1 <- 1}
              else if (si_left == 'unprotected'){e1 <- (2200/(V_O * P)) + (2200 * (1 - g_c_ratio) * V_O)/((2200 * N - V_O) * V_L)}
              else {e1 <- 'Error : [si_left] must be one of [two-way-protection], [direct-left], [unprotected]. Please check that.'}
            }
            else if (N_left == 2){
              if (si_left == 'two-way-protection'){e1 <- 1.05}
              else if (si_left == 'direct-left'){e1 <- 1.05}
              else {e1 <- 'Error : [si_left] must be one of [two-way-protection], [direct-left], [unprotected]. Please check that.'}
            }
            else {e1 <- 'Error : [N_left] must be one of 1, 2. Please check that.'}
          }
          else if (type_left == 'public'){
            if (N_left == 1){
              if (si_left == 'direct-left'){e1 <- 1.00}
              else if (si_left == 'unprotected'){e1 <- 1.02}
              else {e1 <- 'Error : [si_left] must be one of [two-way-protection], [direct-left], [unprotected]. Please check that.'}
            }
            else if (N_left == 2){
              if (si_left == 'direct-left'){e1 <- (2200/(V_O * P)) + (1/V_L) * (((2200 * (1 - g_c_ratio) * V_O) / (2200 * N - V_O)) - (3600 * V_TH / (C * N * V_L)))}
              else {e1 <- 'Error : [si_left] must be one of [two-way-protection], [direct-left], [unprotected]. Please check that.'}
            }
            else {e1 <- 'Error : [N_left] must be one of 1, 2. Please check that.'}
          }
          else {e1 <- 'Error : [type_left] must be one of [dedicated], [public]. Please check that.'}
        }
        else {e1 <- 'Error : [N] must be positive integer. Please check that.'}
      }
      else {e1 <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
    }
    else {e1 <- 'Error : [C] must be positive(sec). Please check that.'}
  }
  else {e1 <- 'Error : [V_O], [V_L], [V_TH] must be positive(vph). Please check that.'}
  e1
}
