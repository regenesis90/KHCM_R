#' Corrected Right Turn Traffic Volume for RTOR
#'
#' Corrected right turn traffic volume (vph) for RTOR.
#'     It follows <Formula 8-3> in KHCM(2013), p.225.
#' @param V_RO Total right turn traffic(vph)
#' @param legs Leg of signalized intersection. Choose one from : \code{3}, \code{4}
#' @param dowry \code{'yes'}, \code{'no'}
#' @details
#'     * Since the signal intersection analysis deals with only vehicles that consume green signals, vehicles (RTORs) turning right at red signals in a public right-turn lane that are not docked are excluded from the analysis.
#'     * In addition, even in the public right-turn lane that has been docked, the right-turning vehicle that deviates from this lane at the time of RTOR and straight forward signal is excluded from the analysis.
#'     * In other words, since the right lane is generally wider than the other straight lanes, the amount of traffic that turns right and exits straight ahead (even at a green signal) is excluded from the analysis.
#'     * This is because these vehicles can be regarded as right-turn-only vehicles that turn right regardless of the signal.
#'     * There are more cases of public right-turn lanes that have been docked to a traffic island near the stop line because the lane width is generally wide.
#' @seealso \code{\link{F_R_si}}, \code{\link{E_R1_si}}, \code{\link{P_R_pd_si}}
#' @keywords traffic volume land use signalized intersection
#' @export V_R_si
#' @examples
#' V_R_si(V_RO = 122, legs = 3)
#' V_R_si(V_RO = 244, legs = 4, dowry = 'yes')
V_R_si <- function(V_RO = NULL, legs = NULL, dowry = NULL){
  if (V_RO >= 0){
    F_R <- F_R_si(legs = legs, dowry = dowry)
    if (is.numeric(F_R) == TRUE){vr <- V_RO * F_R}
    else {vr <- F_R}
  }
  else {vr <- 'Error : [V_RO] must be >= 0(vph). Please check that.'}
  vr
}
