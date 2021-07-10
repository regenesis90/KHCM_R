#' Number of Conflicts Per Hour at Uncontrolled Unsignalized Intersections
#'
#' Number of conflicts per hour at uncontrolled intersections (type 1) among unsignalized intersections(times/h).
#'     This function follows <Formula 10-6>, <Table 10-3> in KHCM(2013), p.473.
#' @param r Traffic volume ratio of main road(%)
#' @param v Total intersection traffic volume(vph)
#' @keywords conflicts number uncontrolled unsignalized intersection
#' @seealso \code{\link{LOS_type1_nsi}}
#' @export n_confl_nsi
#' @examples
#' n_confl_nsi(r = 0.632, v = 346)
n_confl_nsi <- function(r = NULL, v = NULL){
  if (r >= 0 & r < 60){
    a <- 0.1508
    if (v > 0){y <- a * v}
    else {y <- 'Error : [v] must be positive(vph). Please check that.'}
    }
  else if (r >= 60 & r < 70){
    a <- 0.1487
    if (v > 0){y <- a * v}
    else {y <- 'Error : [v] must be positive(vph). Please check that.'}
    }
  else if (r >= 70 & r <= 100){
    a <- 0.1426
    if (v > 0){y <- a * v}
    else {y <- 'Error : [v] must be positive(vph). Please check that.'}
    }
  else {y <- 'Error : [r] must be >= 0 and <= 100(%). Please check that.'}
  y
}
