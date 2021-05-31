#' The straight-line conversion factor of the left turn itself(E_1)
#'
#' This function follows <Formula 8-6> in KHCM(2013)
#' @param left_signal_operation *Categorical* \code{'two_sided_protection'}, \code{'direct_left_sinultaneous'}, \code{'unprotected'}
#' @param left_turn *Categorical* \code{'private'}, \code{'public'}
#' @param P *Numeric* Number of possible unprotected left turns per gap by opposite traffic volume
#' @param N_left *categorical* Number of left-turn lane. \code{1}, \code{2}
#' @param N *Numeric* Number of access lanes (excluding dedicated left-turn lanes)
#' @param V_o *Numeric* Opposite traffic volume(vph)
#' @param V_L *Numeric* Left turn traffic volume(vph)
#' @param V_TH *Numeric* Straight-through traffic volume(vph)
#' @param C *Numeric* Period(s)
#' @param g_c_ratio *Numeric* Effective green hour rate
#' @keywords
#' @export E_1 The straight-line conversion factor of the left turn itself
#' @examples
#' E_1(left_signal_operation = 'two_sided_protection', left_turn = 'private', N_left = 2)
#' E_1(left_signal_operation = 'direct_left_simultaneous', left_turn = 'public', N_left = 1)
#' E_1(left_signal_operation = 'unprotected', left_turn = 'public', N = 2, P = 10, V_o = 100, V_L = 50, V_TH = 280, C = 120, g_c_ratio = 0.2)
E_1 <- function(left_signal_operation = NULL, left_turn = NULL, N_left = NULL, N = NULL, P = NULL, V_o = NULL, V_L = NULL, V_TH = NULL, C = NULL, g_c_ratio = NULL){
  if (left_signal_operation == 'two_sided_protection'){
    if (left_turn == 'private'){
      if (N_left == 1){e <- 1.00}
      if (N_left == 2){e <- 1.05}
    }
  }
  if (left_signal_operation == 'direct_left_simultaneous'){
    if (left_turn == 'public'){
      if (N_left == 1){e <- 1.00}
      if (N_left == 2){e <- 1.02}
    }
    if (left_turn == 'private'){
      if (N_left == 1){e <- 1.00}
      if (N_left == 2){e <- 1.05}
    }
  }
  if (left_signal_operation == 'unprotected'){
    if (left_turn == 'private'){
      e <- (2200/(V_o * P)) + (2200 * (1 - g_c_ratio) * V_o)/((2200 * N - V_o) * V_L)
    }
    if (left_turn == 'public'){
      e <- (2200/(V_o * P)) + (1/V_L) * (((2200 * (1 - g_c_ratio) * V_o) / (2200 * N - V_o)) - (3600 * V_TH / (C * N * V_L)))
    }
  }
  e
}
