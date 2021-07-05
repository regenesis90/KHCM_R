#' Total Number of Conflicts in Bicycle-only Road (times/h)
#'
#' Total number of conflicts in bicycle-only road(type1).
#'     * times/h.
#'     * It follows <Formula 15-4> in KHCM(2013), p.644.
#' @param F_pass Number of overtaking bicycles in the direction of travel(times/h). See \code{\link{F_pass_type1_bk}}.
#' @param F_meet Number of bicycles in the opposite direction(times/h). See \code{\link{F_meet_type1_bk}}
#' @keywords
#' @seealso \code{\link{F_pass_type1_bk}}, \code{\link{F_meet_type1_bk}}, \code{\link{LOS_type1_bk}}
#' @export F_total_type1_bk Total number of conflicts in bicycle traffic (times/h)
#' @examples
#' F_total_type1_bk(F_pass = 389, F_meet = 653)
F_total_type1_bk <- function(F_pass = NULL, F_meet = NULL){
  if (F_pass >= 0 & F_meet >= 0){f <- 0.5 * F_meet + F_pass}
  else {f <- 'Error : [F_pass], [F_meet] must be >= 0(times/h). Please check that.'}
  f
}
