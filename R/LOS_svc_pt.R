#' Service Level According to Bus Service Time
#'
#' Service level according to bus service time.
#'     Investigate the daily service time of the bus operating between the starting and ending points.
#'     (The time from the first vehicle to the last vehicle)
#'     It follows <Table 13-6> in KHCM(2013), p.595.
#' @param t Operation time per day(hour)
#' @export LOS_svc_pt \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_svc_pt(t = 19)
LOS_svc_pt <- function(t = NULL){
  if (t > 20 & t <= 24){los <- 'A'}
  else if (t > 18 & t <= 20){los <- 'B'}
  else if (t > 16 & t <= 18){los <- 'C'}
  else if (t > 14 & t <= 16){los <- 'D'}
  else if (t > 13 & t <= 14){los <- 'E'}
  else if (t <= 13 & t > 0){los <- 'F'}
  else {los <- 'Error : [t] must be positive(hour). Please check that.'}
  los
}
