#' Empirical Utilization Efficiency Coefficient at Bus Stops According to Number of Stop
#'
#' Empirical utilization efficiency coefficient at bus stops according to number of stop.
#'     - As the length of the bus stop increases (the number of stops increases),
#'     the efficiency of using the bus stop decreases.
#'     - Bus stop capacity decreases as the length of the bus stop increases.
#'     Therefore, the utilization efficiency of the stopping area(stop length) of the bus stop decreases.
#'     - Therefore, when designing a bus stop,
#'     rather than lengthening the length of the bus stop according to the increase in bus traffic,
#'     it is necessary to design the bus stop separately.
#' It follows <Table 13-10> KHCM(2013), p.600.
#' @param l length of the bus stop(m)
#' @export ue_bs_pt
#' @examples
#' ue_bs_pt(l = 43.22)
ue_bs_pt <- function(l = NULL){
  if (l > 0 & l < 24){f <- 1.00}
  else if (l >= 24 & l < 36){f <- 1.75}
  else if (l >= 36 & l < 48){f <- 2.25}
  else if (l >= 48 & l < 60){f <- 2.55}
  else if (l >= 60){f <- 2.65}
  else {f <- 'Error : [l] must be positive(m). Please check that.'}
  f
}
