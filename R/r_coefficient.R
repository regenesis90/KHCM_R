#' Compensation factor for calculating the stopping surface capacity according to the bus waiting ratio (r_coefficient)
#'
#' It follows <Table 13-9> KHCM(2013), p.599
#' @param t_R Bus waiting rate(%)
#' @export r_coefficient
#' @examples
#' r_coefficient(13.29)
r_coefficient <- function(t_R = NULL){
  if (t_R == 1){r <- 0.682}
  if (t_R > 1 & t_R < 2.5){r <- 0.682 + (0.718 - 0.682)/(2.5 - 1) * (t_R - 1)}
  if (t_R == 2.5){r <- 0.718}
  if (t_R > 2.5 & t_R < 5){r <- 0.718 + (0.752 - 0.718)/(5 - 2.5) * (t_R - 2.5)}
  if (t_R == 5){r <- 0.752}
  if (t_R > 5 & t_R < 7.5){r <- 0.752 + (0.776 - 0.752)/(7.5 - 5) * (t_R - 5)}
  if (t_R == 7.5){r <- 0.776}
  if (t_R > 7.5 & t_R < 10){r <- 0.776 + (0.81 - 0.776)/(10 - 7.5) * (t_R - 7.5)}
  if (t_R == 10){r <- 0.81}
  if (t_R > 10 & t_R < 15){r <- 0.81 + (0.84 - 0.81)/(15 - 10) * (t_R - 10)}
  if (t_R == 15){r <- 0.84}
  if (t_R > 15 & t_R < 20){r <- 0.84 + (0.87 - 0.84)/(20 - 15) * (t_R - 15)}
  if (t_R == 20){r <- 0.87}
  if (t_R > 20 & t_R < 25){r <- 0.87 + (0.89 - 0.87)/(25 - 20) * (t_R - 20)}
  if (t_R == 25){r <- 0.89}
  if (t_R > 25 & t_R < 30){r <- 0.89 + (0.91 - 0.89)/(30 - 25) * (t_R - 25)}
  if (t_R == 30){r <- 0.91}
  if (t_R > 30 & t_R < 50){r <- 0.91 + (0.95 - 0.91)/(50 - 30) * (t_R - 30)}
  if (t_R == 50){r <- 0.95}
  r
}
