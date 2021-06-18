#' Nighttime Correction Factor in Basic Section of Expressway
#'
#' It is a coefficient indicating the effect of nighttime conditions on traffic flow under basic road traffic conditions.
#'     It follows <Table 2-11> in KHCM(2013), p.41.
#' @param design_speed Design speed. Choose one from : \code{120}, \code{100}, \code{80}
#' @param day_night Distinguish between day and night. Choose one from : \code{'day'}, \code{'night'}
#' @keywords Nighttime Factor Expressway Basic day night
#' @export f_dk
#' @examples
#' f_dk(design_speed = 100, 'day')
#' f_dk(design_speed = 120, 'night')
#' f_dk(80, 'night')
f_dk <- function(design_speed = NULL, day_night = NULL){
  if (day_night == "day"){f_dk <- 1.00}
  else if (day_night == "night"){
    if (design_speed == 80){f_dk <- 0.97}
    else if (design_speed == 100){f_dk <- 0.93}
    else if (design_speed == 120){f_dk <- 0.91}
    else {'Error : [design_speed] must be one of [80], [100], [120]. Please check that.'}
  }
  else {'Error : [day_night] must be one of [day], [night]. Please check that.'}
  f_dk
}
