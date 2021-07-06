#' Compensation Factor for Calculating the Stopping Area Capacity According to the Bus Waiting Ratio
#'
#' A correction factor for calculating the stopping area capacity according to the bus waiting ratio at the bus stop.
#'     - In general, in most urban areas, the maximum realistically possible bus waiting ratio t_R value is observed to be about 25%.
#'     - The recommended value of t_R applied during design is around 10% in urban areas and around 5% in outlying areas.
#'     - It follows <Table 13-9> KHCM(2013), p.599.
#' @param t_R Bus waiting rate(%)
#' @export R_pt
#' @examples
#' R_pt(t_R = 13.29)
R_pt <- function(t_R = NULL){
  if (t_R == 1){r <- 0.682}
  else if (t_R > 1 & t_R < 2.5){r <- 0.682 + (0.718 - 0.682)/(2.5 - 1) * (t_R - 1)}
  else if (t_R == 2.5){r <- 0.718}
  else if (t_R > 2.5 & t_R < 5){r <- 0.718 + (0.752 - 0.718)/(5 - 2.5) * (t_R - 2.5)}
  else if (t_R == 5){r <- 0.752}
  else if (t_R > 5 & t_R < 7.5){r <- 0.752 + (0.776 - 0.752)/(7.5 - 5) * (t_R - 5)}
  else if (t_R == 7.5){r <- 0.776}
  else if (t_R > 7.5 & t_R < 10){r <- 0.776 + (0.81 - 0.776)/(10 - 7.5) * (t_R - 7.5)}
  else if (t_R == 10){r <- 0.81}
  else if (t_R > 10 & t_R < 15){r <- 0.81 + (0.84 - 0.81)/(15 - 10) * (t_R - 10)}
  else if (t_R == 15){r <- 0.84}
  else if (t_R > 15 & t_R < 20){r <- 0.84 + (0.87 - 0.84)/(20 - 15) * (t_R - 15)}
  else if (t_R == 20){r <- 0.87}
  else if (t_R > 20 & t_R < 25){r <- 0.87 + (0.89 - 0.87)/(25 - 20) * (t_R - 20)}
  else if (t_R == 25){r <- 0.89}
  else if (t_R > 25 & t_R < 30){r <- 0.89 + (0.91 - 0.89)/(30 - 25) * (t_R - 25)}
  else if (t_R == 30){r <- 0.91}
  else if (t_R > 30 & t_R < 50){r <- 0.91 + (0.95 - 0.91)/(50 - 30) * (t_R - 30)}
  else if (t_R == 50){r <- 0.95}
  else {r <- 'Error : [t_R] must be >= 0 and <= 50. Please check that.'}
  r
}
