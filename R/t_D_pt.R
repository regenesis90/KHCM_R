#' Bus Stop Time
#'
#' Bus stop time at bus stop(seconds).
#'     It based on door opening time and passenger boarding and disembarking time.
#'     It follows <Table 13-7> in KHCM(2013), p.597.
#' @param type Bus type. Choose one from: \code{'standing'}, \code{'seat'}
#' @param std_psg Presence of standing passengers. Choose one from: \code{'yes'}, \code{'no'}
#' @param pay Payment method with or without change. Choose one from: \code{'card'}, \code{'cash_change'}, \code{'cash_no_change'}
#' @export t_D_pt bus stop time
#' @seealso \code{\link{t_c_pt}}, \code{\link{t_ocp_pt}}
#' @examples
#' t_D_pt(type = 'standing', std_psg = 'yes', pay = 'card')
#' t_D_pt('seat', 'no', pay = 'cash_no_change')
t_D_pt <- function(type = NULL, std_psg = NULL, pay = NULL){
  t_drop_off <- 1.5
  if (type == 'standing'){
    t_door_opening <- 3.0
    if (std_psg == 'no'){
      if (pay == 'card'){
        t_ride <- 3.2
        t <- t_drop_off + t_door_opening + t_ride
        }
      else if (pay == 'cash_no_change'){
        t_ride <- 3.0
        t <- t_drop_off + t_door_opening + t_ride
        }
      else if (pay == 'cash_change'){
        t_ride <- 5.0
        t <- t_drop_off + t_door_opening + t_ride
        }
      else {t <- 'Error : [pay] must be one of [card], [cash_no_change], [cash_change]. Please check that.'}
    }
    else if (std_psg == 'yes'){
      if (pay == 'card'){
        t_ride <- 4.2
        t <- t_drop_off + t_door_opening + t_ride
        }
      else if (pay == 'cash_no_change'){
        t_ride <- 4.0
        t <- t_drop_off + t_door_opening + t_ride
        }
      else if (pay == 'cash_change'){
        t_ride <- 5.0
        t <- t_drop_off + t_door_opening + t_ride
        }
      else {t <- 'Error : [pay] must be one of [card], [cash_no_change], [cash_change]. Please check that.'}
    }
    else {t <- 'Error : [std_psg] must be one of [yes], [no]. Please check that.'}
  }
  else if (type == 'seat'){
    t_door_opening <- 3.2
    if (std_psg == 'no'){
      if (pay == 'card'){
        t_ride <- 3.2
        t <- t_drop_off + t_door_opening + t_ride
        }
      else if (pay == 'cash_no_change'){
        t_ride <- 3.0
        t <- t_drop_off + t_door_opening + t_ride
        }
      else if (pay == 'cash_change'){
        t_ride <- 5.0
        t <- t_drop_off + t_door_opening + t_ride
        }
      else {t <- 'Error : [pay] must be one of [card], [cash_no_change], [cash_change]. Please check that.'}
    }
    else if (std_psg == 'yes'){
      if (pay == 'card'){
        t_ride <- 4.2
        t <- t_drop_off + t_door_opening + t_ride
        }
      else if (pay == 'cash_no_change'){
        t_ride <- 4.0
        t <- t_drop_off + t_door_opening + t_ride
        }
      else if (pay == 'cash_change'){
        t_ride <- 5.0
        t <- t_drop_off + t_door_opening + t_ride
        }
      else {t <- 'Error : [pay] must be one of [card], [cash_no_change], [cash_change]. Please check that.'}
    }
    else {t <- 'Error : [std_psg] must be one of [yes], [no]. Please check that.'}
  }
  else {t <- 'Error : [type] must be one of [city], [seat]. Please check that.'}
  t
}
