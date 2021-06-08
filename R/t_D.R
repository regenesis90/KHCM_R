#' bus stop time. Based on door opening time and passenger boarding and disembarking time(t_D, seconds)
#'
#' It follows <Table 13-7> in KHCM(2013), p.597.
#' @param type Bus type. Choose one from: \code{'city'}, \code{'seat'}
#' @param standing_psg Choose one from: \code{'yes'}, \code{'no'}
#' @param pay Choose one from: \code{'card'}, \code{'cash_change'}, \code{'cash_no_change'}
#' @export t_D bus stop time
#' @examples
#' t_D(type = 'city', standing_psg = 'yes', pay = 'card')
#' t_D('seat', 'no', pay = 'cash_no_change')
t_D <- function(type = NULL, standing_psg = NULL, pay = NULL){
  t_drop_off <- 1.5
  if (type == 'city'){t_door_opening <- 3.0}
  if (type == 'seat'){t_door_opening <- 3.2}
  if (standing_psg == 'no'){
    if (pay == 'card'){t_ride <- 3.2}
    if (pay == 'cash_no_change'){t_ride <- 3.0}
    if (pay == 'cash_change'){t_ride <- 5.0}
  }
  if (standing_psg == 'yes'){
    if (pay == 'card'){t_ride <- 4.2}
    if (pay == 'cash_no_change'){t_ride <- 4.0}
    if (pay == 'cash_change'){t_ride <- 5.0}
  }
  t_door_opening + t_ride + t_drop_off
}
