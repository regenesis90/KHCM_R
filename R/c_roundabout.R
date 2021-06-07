#' Roundabout ramp capacity(c_roundabout, pcph)
#'
#' It follows <Formula 11-2> in KHCM(2013) p.496, <Table 11-8> in p.507
#' @param lane roundabout type. 'rotation lane - entry lane'. Choose one from : \code{'1-1'}, \code{'2-2'}, \code{'2-1'}
#' @param V_c Conflict traffic volume(pcph)
#' @param f_p Crossing Pedestrian Influence Coefficient. See f_p()
#' @export c_roundabout
#' @examples
#' c_roundabout(lane = '1-1', V_c = 382, f_p = 0.8)
c_roundabout <- function(lane = NULL, V_c = NULL, f_p = NULL){
  if (lane == '1-1'){
    t_c <- 3.21
    t_f <- 3.15
    t_min <- 2.05
    n_e <- 1
  }
  if (lane == '2-2'){
    t_c <- 3.21
    t_f <- 3.15
    t_min <- 0
    n_e <- 1.7
  }
  if (lane == '2-1'){
    t_c <- 3.21
    t_f <- 3.15
    t_min <- 0
    n_e <- 1
  }
  c <- 3600 * (1-(V_c * t_min / 3600)) * (n_e/t_f) * exp(-1 * (V_c/3600) * (t_c = 0.5 * t_f - t_min)) * f_p
  c
}
