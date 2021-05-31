#' Inclination correction factor(f_g)
#'
#' It follows <Table 8-16> in KHCM(2013)
#' @param gradient %
#' @export f_g
#' @examples
#' f_g(gradient = 2.72)
#' f_g(3.945)
f_g <- function(gradient = NULL){
  if (gradient <= 0){f <- 1.00}
  if (gradient > 0 & gradient < 3){f <- 1.00 + ((0.96 - 1.00)/3) * (gradient - 0)}
  if (gradient == 3){f <- 0.96}
  if (gradient > 3 & gradient < 6){f <- 0.96 + ((0.93 - 0.96)/3) * (gradient - 3)}
  if (gradient >= 6){f <- 0.93}
  f
}
