#' Correction Coefficient of Total Delay Ratio in 2+1 Lane Road
#'
#' It correct TDR(Total Delay Rate) in 2+1 lane road.
#'     This function follows <Table 7-14> in KHCM(2013), p.186.
#' @param v Traffic volume in 2+1 lane road(pcphpl).
#' @keywords 2+1 lane road correction coefficient TDR total delay ratio
#' @export f_pl_2lp1 Correction Coefficient of Total Delay in 2+1 Lane Road Section(f_pl)
#' @examples
#' f_pl_2lp1(v = 1391)
#' f_pl_2lp1(999)
f_pl_2lp1 <- function(v = NULL){
  if (v > 0 & v <= 100){f <- 0.789}
  else if (v > 100 & v <= 200){f <- 0.761}
  else if (v > 200 & v <= 300){f <- 0.735}
  else if (v > 300 & v <= 400){f <- 0.713}
  else if (v > 400 & v <= 500){f <- 0.694}
  else if (v > 500 & v <= 600){f <- 0.678}
  else if (v > 600 & v <= 700){f <- 0.665}
  else if (v > 700 & v <= 800){f <- 0.655}
  else if (v > 800 & v <= 900){f <- 0.648}
  else if (v > 900 & v <= 1000){f <- 0.645}
  else if (v > 1000 & v <= 1100){f <- 0.644}
  else if (v > 1100 & v <= 1200){f <- 0.646}
  else if (v > 1200 & v <= 1300){f <- 0.651}
  else if (v > 1300 & v <= 1400){f <- 0.660}
  else if (v > 1400 & v <= 1500){f <- 0.671}
  else if (v > 1500 & v <= 1600){f <- 0.686}
  else if (v > 1600 & v <= 1700){f <- 0.704}
  else {f <- 'Error : [v] must be >= 0 and <= 1700(pcphpl). Please check that.'}
  f
}
