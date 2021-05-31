#' Straight conversion factor by left turn curve radius(E_p)
#'
#' This function follows <Table 8-9> in KHCM(2013)
#' @param radius *Numeric* Left turn curve radius(m)
#' @keywords
#' @export E_p Straight conversion factor by left turn curve radius
#' @examples
#' E_p(8.2)
E_p <- function(radius = NULL){
  if (radius >= 0 & radius <= 9){e <- 1.14}
  if (radius > 9 & radius <= 12){e <- 1.11}
  if (radius > 12 & radius <= 15){e <- 1.09}
  if (radius > 15 & radius <= 18){e <- 1.06}
  if (radius > 18 & radius <= 20){e <- 1.05}
  if (radius > 20){e <- 1.00}
  e
}
