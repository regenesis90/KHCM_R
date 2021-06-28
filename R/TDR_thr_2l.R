#' Total Delay Rate Obtained By Theory
#'
#' Total delay rate(TDR, %) obtained by applying the theoretical equation.
#'    It follows <Formula 7-2>, <Table 7-1> in KHCM(2013), p.167.
#' @param v_d Traffic Volume in direction(pc/lane).
#' @keywords TDR Total Delay Rate
#' @export TDR_thr_2l Total delay rate obtained by theory(%)
#' @details The total delay rate is the average delay rate of vehicles
#'     within a vehicle group traveling in a certain section.
#'     The total delay rate is a measure that expresses the degree of delay with respect
#'     to the speed desired by the driver.
#'     When traffic is low, vehicles are rarely delayed,
#'     and the average head distance is also increased,
#'     which increases the likelihood of overtaking.
#'     Although the total delay rate is low under low traffic conditions,
#'     the chance of overtaking decreases as the vehicle approaches capacity,
#'     so that almost all vehicles form a vehicle group and the total delay rate increases.
#' @seealso \code{\link{TDR_2l}}, \code{\link{TDR_1_i_2l}}, \code{\link{TDR_2_i_2l}}
#' @examples
#' TDR_thr_2l(3423)
TDR_thr_2l <- function(v_d = NULL){
  if (v_d > 200 & v_d <= 400){tdr <- (1 - exp(-0.0010 * (v_d**0.8397))) * 100}
  else if (v_d > 400 & v_d <= 600){tdr <- (1 - exp(-0.0016 * (v_d**0.7934))) * 100}
  else if (v_d > 600 & v_d <= 1000){tdr <- (1 - exp(-0.0020 * (v_d**0.7754))) * 100}
  else if (v_d > 1000){tdr <- (1 - exp(-0.0040 * (v_d**0.6856))) * 100}
  else {tdr <- 'Error : [v_d] must be positive. Please check that.'}
  tdr
}
