#' Average Control Delay per Vehicle on Approach A at Signalized Intersection I
#'
#' Average control delay per vehicle on approach A at signalized intersection I(sec/veh).
#'     It follows <Formula 8-55> in KHCM(2013), p.258.
#' @param d_i Average control delay per vehicle in approach A, i-lane group (sec/veh)
#' @param v_i Corrected traffic volume (vph) in approach A, i-lane group(vph)
#' @seealso \code{\link{d_total_si}}
#' @export d_A_si
#' @examples
#' d_A_si(d_i = c(10, 2.4, 3.5, 6.2), v_i = c(230, 432, 581, 398))
d_A_si <- function(d_i = NULL, v_i = NULL){
  di_sum <- 0
  vi_sum <- 0
  for (i in 1:length(d_i)){
    di <- d_i[i] * v_i[i]
    vi <- v_i[i]
    di_sum <- di_sum + di
    vi_sum <- vi_sum + vi
  }
  di_sum/vi_sum
}
