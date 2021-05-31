#' Average control delay per vehicle on approach A (d_A, sec/unit)
#'
#' It follows <Formula 8-56> in KHCM(2013)
#' @param d_A Average control delay per vehicle in approach A lane group (seconds/vehicle)
#' @param V_A Corrected traffic volume (vph) for the i lane group
#' @export d_I Average control delay per vehicle on approach A
#' @examples
#' d_I(d_A = c(10, 2.4, 3.5, 6.2), V_A = c(230, 432, 581, 398))
d_I <- function(d_A = NULL, V_A = NULL){
  da_sum <- 0
  va_sum <- 0
  for (i in 1:length(d_A)){
    da <- d_A[i] * V_A[i]
    va <- V_A[i]
    da_sum <- da_sum + da
    va_sum <- va_sum + va
  }
  da_sum/va_sum
}
