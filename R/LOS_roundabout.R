#' Level of Service(LOS) in Roundabout(LOS_roundabout)
#'
#' This function follows <Table 11-1> in KHCM(2013) p.494
#' @param avg_d Average Delay(seconds/unit)
#' @param V_c_ratio Traffic Volume/Capacity
#' @keywords
#' @export LOS_roundabout
#' @examples
#' LOS_roundabout(43.2, 0.3)
LOS_roundabout <- function(avg_d = NULL, V_c_ratio = NULL){
  if (V_c_ratio >= 0 & V_c_ratio <= 1){
    if (avg_d >= 0 & avg_d < 10){los <- 'A'}
    if (avg_d >= 10 & avg_d < 15){los <- 'B'}
    if (avg_d >= 15 & avg_d < 25){los <- 'C'}
    if (avg_d >= 25 & avg_d < 35){los <- 'D'}
    if (avg_d >= 35 & avg_d < 50){los <- 'E'}
  }
  if (V_c_ratio > 1){
    if (avg_d >= 50){los <- 'F'}
  }
  los
}
