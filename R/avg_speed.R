#' Average moving speed divided into 5 sections (acceleration 4km/h)
#'
#' It follows <Table 8-21> in KHCM(2013)
#' @param d_sum d_T + d_WZ + d_IW
#' @param unit Choose one from : \code{'kmph'}, \code{'mps'}
#' @export avg_speed Average moving speed divided into 5 sections
#' @examples
#' avg_speed(130, 'mps')
#' avg_speed(88.33, 'kmph')
avg_speed <- function(d_sum = NULL, unit = NULL){
  if (unit == 'kmph'){
    if (d_sum >= 0 & d_sum <= 20){s <- 10.2}
    if (d_sum > 20 & d_sum <= 50){s <- 16.9}
    if (d_sum > 50 & d_sum <= 100){s <- 23.9}
    if (d_sum > 100 & d_sum <= 150){s <- 30.5}
    if (d_sum > 150){s <- 36.0}
  }
  if (unit == 'mps'){
    if (d_sum >= 0 & d_sum <= 20){s <- 2.84}
    if (d_sum > 20 & d_sum <= 50){s <- 4.69}
    if (d_sum > 50 & d_sum <= 100){s <- 6.64}
    if (d_sum > 100 & d_sum <= 150){s <- 8.49}
    if (d_sum > 150){s <- 10.00}
  }
  s
}
