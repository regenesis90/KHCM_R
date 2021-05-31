#' Upstream stop influence correction factor(f_ub)
#'
#' It follows <Table 8-19> in KHCM(2013)
#' @param sep_l The separation distance of the upstream stop(m)
#' @export f_ub Upstream stop influence correction factor
#' @examples
#' f_ub(112.45)
f_ub <- function(sep_l = NULL){
  if (sep_l >= 0 & sep_l <= 20){f <- 0.51}
  if (sep_l > 20 & sep_l < 70){f <- 0.51 + ((0.68 - 0.51)/50) * (sep_l - 20)}
  if (sep_l == 70){f <- 0.68}
  if (sep_l > 70 & sep_l < 120){f <- 0.68 + ((1.00 - 0.68)/50) * (sep_l - 70)}
  if (sep_l >= 120){f <- 1.00}
  f
}
