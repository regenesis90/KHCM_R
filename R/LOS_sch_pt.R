#' Service Level by Region According to Bus Service Interval
#'
#' Service level by region according to bus service interval.
#'     It follows <Table 13-5> in KHCM(2013), p.595.
#' @param city_scale City Scale. Choose one from: \code{'large'}, \code{'small'}
#' @param hdw Bus operation interval(minutes)
#' @export LOS_sch_pt \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_sch_pt(city_scale = 'large', hdw = 23)
#' LOS_sch_pt(city_scale = 'small', hdw = 92)
LOS_hdw <- function(city_scale = NULL, hdw = NULL){
  if (city_scale == 'large'){
    if (hdw > 0 & hdw <= 3){los <- 'A'}
    else if (hdw > 3 & hdw <= 6){los <- 'B'}
    else if (hdw > 6 & hdw <= 10){los <- 'C'}
    else if (hdw > 10 & hdw <= 15){los <- 'D'}
    else if (hdw > 15 & hdw <= 25){los <- 'E'}
    else if (hdw > 25){los <- 'F'}
    else {hdw <- 'Error : [hdw] must be positive(min). Please check that.'}
  }
  else if (city_scale == 'small'){
    if (hdw > 0 & hdw <= 10){los <- 'A'}
    else if (hdw > 10 & hdw <= 20){los <- 'B'}
    else if (hdw > 20 & hdw <= 40){los <- 'C'}
    else if (hdw > 40 & hdw <= 60){los <- 'D'}
    else if (hdw > 60 & hdw <= 100){los <- 'E'}
    else if (hdw > 100){los <- 'F'}
    else {hdw <- 'Error : [hdw] must be positive(min). Please check that.'}
  }
  else {los <- 'Error : [city_scale] must be one of [large], [small]. Please check that.'}
  los
}
