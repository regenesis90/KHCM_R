#' Total Clearance Time of Bus at Bus Stop
#'
#' It is the sum of the clearance times in the deceleration section and the acceleration section of bus stop.
#'     It follows <Table 13-8> in KHCM(2013), p.597.
#' @param type Bus type. Choose one from: \code{'standing'}, \code{'seat'}
#' @param bus_bay Presence of bus bay. Choose one from: \code{'yes'}, \code{'no'}
#' @export t_c_pt bus stop time
#' @seealso \code{\link{t_D_pt}}, \code{\link{t_ocp_pt}}
#' @examples
#' t_c_pt(type = 'standing', bus_bay = 'yes')
#' t_c_pt(type = 'seat', bus_bay = 'no')
t_c_pt <- function(type = NULL, bus_bay = NULL){
  if (type == 'standing'){
    if (bus_bay == 'yes'){
      acc <- 9.5
      dsc <- 7
      t <- acc + dsc
    }
    else if (bus_bay == 'no'){
      acc <- 9
      dsc <- 7
      t <- acc + dsc
    }
    else {t <- 'Error : [bus_bay] must be one of [yes], [no]. Please check that.'}
  }
  else if (type == 'seat'){
    if (bus_bay == 'yes'){
      acc <- 9
      dsc <- 7
      t <- acc + dsc
    }
    else if (bus_bay == 'no'){
      acc <- 8
      dsc <- 7
      t <- acc + dsc
    }
    else {t <- 'Error : [bus_bay] must be one of [yes], [no]. Please check that.'}
  }
  else {t <- 'Error : [type] must be one of [standing], [seat]. Please check that.'}
  t
}
