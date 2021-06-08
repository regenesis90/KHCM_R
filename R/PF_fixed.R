#' Fixed time signal interlocking factor (PF_fixed)
#'
#' This function follows <Table 8-17>, <12-8> in KHCM(2013) p.541
#' @param TVO
#' @param g_c_ratio
#' @keywords
#' @export PF_fixed Fixed time signal interlocking factor (PF)
#' @examples
#' PF_fixed(TVO = 0.4, g_c_ratio = 0.42)
#' PF_fixed(0.5, 0.2223)
PF_fixed <- function(TVO = NULL, g_c_ratio= NULL){
  if (TVO == 0){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (0.86 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (0.86 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 0.86}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 0.86 + (0.76 - 0.86)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 0.76}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.76 + (0.71 - 0.76)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 0.71}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.71}
    if (g_c_ratio == 0.5){pf <- 0.71}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.71 + (0.73 - 0.71)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 0.73}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.73 + (0.78 - 0.73)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 0.78}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.78 + (0.86 - 0.78)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 0.86}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.86 + (1.06 - 0.86)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 1.06}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.86 + (1.06 - 0.86)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.1){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 0.62 + (0.56 - 0.62)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 0.62}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 0.62 + (0.56 - 0.62)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 0.56}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 0.56 + (0.54 - 0.56)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 0.54}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.54 + (0.55 - 0.54)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 0.55}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.55 + (0.58 - 0.55)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 0.58}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.58 + (0.64 - 0.58)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 0.64}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.64 + (0.72 - 0.64)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 0.72}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.72 + (0.81 - 0.72)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 0.81}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 0.92}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.2){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (0.81 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (0.81 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 0.81}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 0.81 + (0.59 - 0.81)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 0.59}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.59 + (0.55 - 0.59)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 0.55}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.55 + (0.58 - 0.55)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 0.58}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.58 + (0.64 - 0.58)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 0.64}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.64 + (0.72 - 0.64)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 0.72}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.72 + (0.81 - 0.72)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 0.81}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 0.92}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.3){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 1.11}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (0.98 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 0.98}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.98 + (0.77 - 0.98)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 0.77}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.77 + (0.58 - 0.77)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 0.58}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.58 + (0.64 - 0.58)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 0.64}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.64 + (0.72 - 0.64)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 0.72}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.72 + (0.81 - 0.72)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 0.81}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 0.92}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.4){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 1.11}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 1.20}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.14 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 1.14}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.14 + (0.94 - 1.14)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 0.94}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.94 + (0.73 - 0.94)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 0.73}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.73 + (0.72 - 0.73)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 0.72}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.72 + (0.81 - 0.72)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 0.81}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 0.92}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.5){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 1.11}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 1.20}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.31 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 1.31}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.31 + (1.30 - 1.31)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 1.30}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.30 + (1.09 - 1.30)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 1.09}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.09 + (0.83 - 1.09)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 0.83}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.83 + (0.81 - 0.83)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 0.81}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 0.92}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.6){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 1.11}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 1.20}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.31 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 1.31}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.31 + (1.43 - 1.31)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 1.43}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.43 + (1.47 - 1.43)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 1.47}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.47 + (1.22 - 1.47)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 1.22}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 1.22 + (0.81 - 1.22)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 0.81}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 0.92}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.7){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 1.11}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 1.20}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.31 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 1.31}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.31 + (1.43 - 1.31)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 1.43}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.43 + (1.56 - 1.43)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 1.56}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.56 + (1.63 - 1.56)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 1.63}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 1.63 + (1.27 - 1.63)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 1.27}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 1.27 + (0.92 - 1.27)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 0.92}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 1.27 + (0.92 - 1.27)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.8){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 1.11}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 1.20}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.31 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 1.31}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.31 + (1.43 - 1.31)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 1.43}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.43 + (1.47 - 1.43)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 1.47}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.47 + (1.58 - 1.47)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 1.58}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 1.58 + (1.76 - 1.58)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 1.76}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 1.76 + (1.00 - 1.76)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 1.00}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 1.76 + (1.00 - 1.76)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 0.9){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.04}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 1.11}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.25 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 1.15}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.15 + (1.08 - 1.15)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 1.08}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.08 + (1.06 - 1.08)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 1.06}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.06 + (1.09 - 1.06)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 1.09}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.09 + (1.17 - 1.09)/0.1 * (g_c_ratio - 0.6)}
    if (g_c_ratio == 0.7){pf <- 1.17}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 1.17 + (1.32 - 1.17)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 1.32}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 1.32 + (1.59 - 1.32)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 1.59}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 1.32 + (1.59 - 1.32)/0.1 * (g_c_ratio - 0.8)}
  }
  if (TVO == 1.0){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.03 + (1.01 - 1.03)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.1){pf <- 1.03}
    if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.03 + (1.01 - 1.03)/0.1 * (g_c_ratio - 0.1)}
    if (g_c_ratio == 0.2){pf <- 1.01}
    if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.01 + (0.89 - 1.01)/0.1 * (g_c_ratio - 0.2)}
    if (g_c_ratio == 0.3){pf <- 0.89}
    if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.89 + (0.80 - 0.89)/0.1 * (g_c_ratio - 0.3)}
    if (g_c_ratio == 0.4){pf <- 0.80}
    if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.80 + (0.74 - 0.80)/0.1 * (g_c_ratio - 0.4)}
    if (g_c_ratio == 0.5){pf <- 0.74}
    if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.74 + (0.71 - 0.74)/0.1 * (g_c_ratio - 0.5)}
    if (g_c_ratio == 0.6){pf <- 0.71}
    if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.71}
    if (g_c_ratio == 0.7){pf <- 0.71}
    if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.71 + (0.81 - 0.71)/0.1 * (g_c_ratio - 0.7)}
    if (g_c_ratio == 0.8){pf <- 0.81}
    if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (1.08 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    if (g_c_ratio == 0.9){pf <- 1.08}
    if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (1.08 - 0.81)/0.1 * (g_c_ratio - 0.8)}
  }
  pf
}
