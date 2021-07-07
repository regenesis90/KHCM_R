#' Fixed Signal Interlocking Coefficient on Arterial Road
#'
#' Fixed signal interlocking coefficient on arterial roads.
#'     For non-interlocking fixed-signal intersections or sub-road access roads,
#'     dedicated left-turn lane groups, dedicated right-turn lane groups, etc., the correction factor is set to 1.0 regardless of arrival type.
#'     This function follows <12-8> in KHCM(2013) p.541.
#' @param TVO Offset convenience rate on arterial road. See \code{\link{TVO_artl}}
#' @param g_c_ratio Green time/Signal cycle ratio
#' @keywords Fixed Signal Interlocking Coefficient Arterial Road
#' @export PF_fixed Fixed time signal interlocking factor
#' @examples
#' PF_fixed(TVO = 1, g_c_ratio = 0.2)
#' PF_fixed(TVO = 0.32, g_c_ratio = 0.254)
PF_fixed <- function(TVO = NULL, g_c_ratio= NULL){
  if (TVO == 0){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (0.86 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (0.86 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 0.86}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 0.86 + (0.76 - 0.86)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 0.76}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.76 + (0.71 - 0.76)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 0.71}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.71}
    else if (g_c_ratio == 0.5){pf <- 0.71}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.71 + (0.73 - 0.71)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 0.73}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.73 + (0.78 - 0.73)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 0.78}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.78 + (0.86 - 0.78)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 0.86}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.86 + (1.06 - 0.86)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 1.06}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.86 + (1.06 - 0.86)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.1){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 0.62 + (0.56 - 0.62)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 0.62}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 0.62 + (0.56 - 0.62)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 0.56}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 0.56 + (0.54 - 0.56)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 0.54}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.54 + (0.55 - 0.54)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 0.55}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.55 + (0.58 - 0.55)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 0.58}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.58 + (0.64 - 0.58)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 0.64}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.64 + (0.72 - 0.64)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 0.72}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.72 + (0.81 - 0.72)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 0.81}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 0.92}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.2){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (0.81 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (0.81 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 0.81}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 0.81 + (0.59 - 0.81)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 0.59}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.59 + (0.55 - 0.59)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 0.55}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.55 + (0.58 - 0.55)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 0.58}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.58 + (0.64 - 0.58)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 0.64}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.64 + (0.72 - 0.64)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 0.72}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.72 + (0.81 - 0.72)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 0.81}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 0.92}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.3){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 1.11}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (0.98 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 0.98}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.98 + (0.77 - 0.98)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 0.77}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.77 + (0.58 - 0.77)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 0.58}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.58 + (0.64 - 0.58)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 0.64}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.64 + (0.72 - 0.64)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 0.72}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.72 + (0.81 - 0.72)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 0.81}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 0.92}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.4){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 1.11}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 1.20}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.14 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 1.14}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.14 + (0.94 - 1.14)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 0.94}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.94 + (0.73 - 0.94)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 0.73}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.73 + (0.72 - 0.73)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 0.72}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.72 + (0.81 - 0.72)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 0.81}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 0.92}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.5){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 1.11}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 1.20}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.31 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 1.31}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.31 + (1.30 - 1.31)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 1.30}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.30 + (1.09 - 1.30)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 1.09}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.09 + (0.83 - 1.09)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 0.83}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.83 + (0.81 - 0.83)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 0.81}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 0.92}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.6){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 1.11}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 1.20}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.31 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 1.31}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.31 + (1.43 - 1.31)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 1.43}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.43 + (1.47 - 1.43)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 1.47}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.47 + (1.22 - 1.47)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 1.22}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 1.22 + (0.81 - 1.22)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 0.81}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 0.92}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (0.92 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.7){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 1.11}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 1.20}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.31 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 1.31}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.31 + (1.43 - 1.31)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 1.43}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.43 + (1.56 - 1.43)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 1.56}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.56 + (1.63 - 1.56)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 1.63}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 1.63 + (1.27 - 1.63)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 1.27}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 1.27 + (0.92 - 1.27)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 0.92}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 1.27 + (0.92 - 1.27)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.8){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 1.11}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.20 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 1.20}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.20 + (1.31 - 1.20)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 1.31}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.31 + (1.43 - 1.31)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 1.43}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.43 + (1.47 - 1.43)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 1.47}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.47 + (1.58 - 1.47)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 1.58}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 1.58 + (1.76 - 1.58)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 1.76}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 1.76 + (1.00 - 1.76)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 1.00}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 1.76 + (1.00 - 1.76)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 0.9){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.04}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.04 + (1.11 - 1.04)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 1.11}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.11 + (1.25 - 1.11)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 1.15}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 1.15 + (1.08 - 1.15)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 1.08}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 1.08 + (1.06 - 1.08)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 1.06}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 1.06 + (1.09 - 1.06)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 1.09}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 1.09 + (1.17 - 1.09)/0.1 * (g_c_ratio - 0.6)}
    else if (g_c_ratio == 0.7){pf <- 1.17}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 1.17 + (1.32 - 1.17)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 1.32}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 1.32 + (1.59 - 1.32)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 1.59}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 1.32 + (1.59 - 1.32)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO == 1.0){
    if (g_c_ratio > 0 & g_c_ratio < 0.1){pf <- 1.03 + (1.01 - 1.03)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.1){pf <- 1.03}
    else if (g_c_ratio > 0.1 & g_c_ratio < 0.2){pf <- 1.03 + (1.01 - 1.03)/0.1 * (g_c_ratio - 0.1)}
    else if (g_c_ratio == 0.2){pf <- 1.01}
    else if (g_c_ratio > 0.2 & g_c_ratio < 0.3){pf <- 1.01 + (0.89 - 1.01)/0.1 * (g_c_ratio - 0.2)}
    else if (g_c_ratio == 0.3){pf <- 0.89}
    else if (g_c_ratio > 0.3 & g_c_ratio < 0.4){pf <- 0.89 + (0.80 - 0.89)/0.1 * (g_c_ratio - 0.3)}
    else if (g_c_ratio == 0.4){pf <- 0.80}
    else if (g_c_ratio > 0.4 & g_c_ratio < 0.5){pf <- 0.80 + (0.74 - 0.80)/0.1 * (g_c_ratio - 0.4)}
    else if (g_c_ratio == 0.5){pf <- 0.74}
    else if (g_c_ratio > 0.5 & g_c_ratio < 0.6){pf <- 0.74 + (0.71 - 0.74)/0.1 * (g_c_ratio - 0.5)}
    else if (g_c_ratio == 0.6){pf <- 0.71}
    else if (g_c_ratio > 0.6 & g_c_ratio < 0.7){pf <- 0.71}
    else if (g_c_ratio == 0.7){pf <- 0.71}
    else if (g_c_ratio > 0.7 & g_c_ratio < 0.8){pf <- 0.71 + (0.81 - 0.71)/0.1 * (g_c_ratio - 0.7)}
    else if (g_c_ratio == 0.8){pf <- 0.81}
    else if (g_c_ratio > 0.8 & g_c_ratio < 0.9){pf <- 0.81 + (1.08 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else if (g_c_ratio == 0.9){pf <- 1.08}
    else if (g_c_ratio > 0.9 & g_c_ratio < 1){pf <- 0.81 + (1.08 - 0.81)/0.1 * (g_c_ratio - 0.8)}
    else {pf <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else if (TVO > 0 & TVO < 0.1){
    l_gap <- TVO - 0
    r_gap <- 0.1 - TVO
    l_value <- PF_fixed(TVO = 0, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.1, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.1 & TVO < 0.2){
    l_gap <- TVO - 0.1
    r_gap <- 0.2 - TVO
    l_value <- PF_fixed(TVO = 0.1, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.2, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.2 & TVO < 0.3){
    l_gap <- TVO - 0.2
    r_gap <- 0.3 - TVO
    l_value <- PF_fixed(TVO = 0.2, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.3, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.3 & TVO < 0.4){
    l_gap <- TVO - 0.3
    r_gap <- 0.4 - TVO
    l_value <- PF_fixed(TVO = 0.3, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.4, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.4 & TVO < 0.5){
    l_gap <- TVO - 0.4
    r_gap <- 0.5 - TVO
    l_value <- PF_fixed(TVO = 0.4, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.5, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.5 & TVO < 0.6){
    l_gap <- TVO - 0.5
    r_gap <- 0.6 - TVO
    l_value <- PF_fixed(TVO = 0.5, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.6, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.6 & TVO < 0.7){
    l_gap <- TVO - 0.6
    r_gap <- 0.7 - TVO
    l_value <- PF_fixed(TVO = 0.6, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.7, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.7 & TVO < 0.8){
    l_gap <- TVO - 0.7
    r_gap <- 0.8 - TVO
    l_value <- PF_fixed(TVO = 0.7, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.8, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.8 & TVO < 0.9){
    l_gap <- TVO - 0.8
    r_gap <- 0.9 - TVO
    l_value <- PF_fixed(TVO = 0.8, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 0.9, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else if (TVO > 0.9 & TVO < 1.0){
    l_gap <- TVO - 0.9
    r_gap <- 1.0 - TVO
    l_value <- PF_fixed(TVO = 0.9, g_c_ratio = g_c_ratio)
    r_value <- PF_fixed(TVO = 1.0, g_c_ratio = g_c_ratio)
    pf <- l_gap/0.1 * l_value + r_gap/0.1 * r_value
  }
  else {pf <- 'Error : [TVO] must be > 0 and <= 1. Please check that.'}
  pf
}
