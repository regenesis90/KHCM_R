#' Correction Coefficient of Travel Speed in 2+1 Lane Road
#'
#' It correct travel speed in 2+1 lane road.
#'     This function follows <Table 7-15> in KHCM(2013), p.188.
#' @param v Traffic volume in 2+1 lane road(pcphpl).
#' @keywords 2+1 lane road correction coefficient TDR total delay ratio
#' @export fs_pl_2lp1 Correction coefficient of travel speed in 2+1 lane road Section(f_pl)
#' @examples
#' fs_pl_2lp1(v = 1391)
#' fs_pl_2lp1(999)
fs_pl_2lp1 <- function(v = NULL){
  if (v > 0 & v <= 100){f <- 1.025}
  else if (v > 100 & v <= 200){f <- 1.034}
  else if (v > 200 & v <= 300){f <- 1.042}
  else if (v > 300 & v <= 400){f <- 1.050}
  else if (v > 400 & v <= 500){f <- 1.057}
  else if (v > 500 & v <= 600){f <- 1.063}
  else if (v > 600 & v <= 700){f <- 1.069}
  else if (v > 700 & v <= 800){f <- 1.074}
  else if (v > 800 & v <= 900){f <- 1.078}
  else if (v > 900 & v <= 1000){f <- 1.081}
  else if (v > 1000 & v <= 1100){f <- 1.084}
  else if (v > 1100 & v <= 1200){f <- 1.086}
  else if (v > 1200 & v <= 1300){f <- 1.088}
  else if (v > 1300 & v <= 1400){f <- 1.089}
  else if (v > 1400 & v <= 1500){f <- 1.089}
  else if (v > 1500 & v <= 1600){f <- 1.088}
  else if (v > 1600 & v <= 1700){f <- 1.087}
  else {f <- 'Error : [v] must be >= 0 and <= 1700(pcphpl). Please check that.'}
  f
}
