#' Correction Coefficient of Total Delay in 2+1 Lane Road Section(f_pl)
#'
#' This function follows <Table 7-14>
#' @param V *Numeric* Traffic Volume(pcphpl)
#' @keywords
#' @export f_pl Correction Coefficient of Total Delay in 2+1 Lane Road Section(f_pl)
#' @examples
#' f_pl(V = 1391)
#' f_pl(999)
f_pl <- function(V = NULL){
  if (V > 0 & V <= 100){f <- 0.789}
  if (V > 100 & V <= 200){f <- 0.761}
  if (V > 200 & V <= 300){f <- 0.735}
  if (V > 300 & V <= 400){f <- 0.713}
  if (V > 400 & V <= 500){f <- 0.694}
  if (V > 500 & V <= 600){f <- 0.678}
  if (V > 600 & V <= 700){f <- 0.665}
  if (V > 700 & V <= 800){f <- 0.655}
  if (V > 800 & V <= 900){f <- 0.648}
  if (V > 900 & V <= 1000){f <- 0.645}
  if (V > 1000 & V <= 1100){f <- 0.644}
  if (V > 1100 & V <= 1200){f <- 0.646}
  if (V > 1200 & V <= 1300){f <- 0.651}
  if (V > 1300 & V <= 1400){f <- 0.660}
  if (V > 1400 & V <= 1500){f <- 0.671}
  if (V > 1500 & V <= 1600){f <- 0.686}
  if (V > 1600 & V <= 1700){f <- 0.704}
  f
}
