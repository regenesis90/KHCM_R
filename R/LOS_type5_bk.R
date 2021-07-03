#' Level of Service for Cyclist on Bicycle Paths in City Streets
#'
#' Level of service(LOS) for cyclist on bicycle path in city streets(type5).
#'     * type5 : A bicycle road on a city street that continuously meets a signal intersection.
#'     * It follows <Table 15-7> in KHCM(2013), p.655.
#' @param ATS Average travel speed of bicycle(kph)
#' @keywords LOS Level of service Bicycle path city street
#' @export LOS_type5_bk
#' @examples
#' LOS_type5_bk(ATS = 8.88)
LOS_type5_bk <- function(ATS = NULL){
  if (ATS >= 0 & ATS <= 6){los <- 'F'}
  else if (ATS > 6 & ATS <= 7){los <- 'E'}
  else if (ATS > 7 & ATS <= 8){los <- 'D'}
  else if (ATS > 8 & ATS <= 10){los <- 'C'}
  else if (ATS > 10 & ATS <= 12){los <- 'B'}
  else if (ATS > 12){los <- 'A'}
  else {los <- 'Error : [ATS] must be positive(kph). Please check that.'}
  los
}
