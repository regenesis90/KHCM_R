#' Level of Service(LOS) in Two-point Intersecting Diamond-shaped Interchange
#'
#' Level of service(LOS) in two-point intersecting diamond-shaped interchange(highway connection road-general road junction).
#'     It follows <Table 9-1> in KHCM(2013), p.425.
#' @param d Control delay per Car(seconds)
#' @keywords LOS Level of Service diamond interchange two-point
#' @seealso \code{\link{d_total_di}}
#' @export LOS_di Level of Service. \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}, \code{'FF'}, \code{'FFF'}
#' @examples
#' LOS_di(d = 33.444)
LOS_di <- function(d = NULL){
  if (d >= 0 & d <= 22){LOS <- 'A'}
  else if (d >= 22 & d <= 45){LOS <- 'B'}
  else if (d >= 45 & d <= 75){LOS <- 'C'}
  else if (d >= 75 & d <= 105){LOS <- 'D'}
  else if (d >= 105 & d <= 150){LOS <- 'E'}
  else if (d >= 150 & d <= 330){LOS <- 'F'}
  else if (d >= 330 & d <= 510){LOS <- 'FF'}
  else if (d > 510){LOS <- 'FFF'}
  else {LOS <- 'Error : [d] must be >= 0(seconds). Please check that.'}
  LOS
}
