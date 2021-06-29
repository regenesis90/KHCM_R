#' Total Average Travel Speed in 2-lane Road
#'
#' Total average travel speed in 2-lane road(kph).
#'     This function follows <Formula 7-11> in KHCM(2013), p.176.
#' @param L Total length(km).
#' @param L_1 Series of ection i length (km) of section type1 2-lane road. See \code{\link{type_2l}}
#' @param L_2 Series of ection i length (km) of section type2 2-lane road. See \code{\link{type_2l}}
#' @param ATS_1 Series of average travel speed (km/h) in section i of section type1 2-lane road. See \code{\link{ATS_1_i_2l}}
#' @param ATS_2 Series of average travel speed (km/h) in section i of section type2 2-lane road. See \code{\link{ATS_2_i_2l}}
#' @keywords Average Travel Speed ATS 2-lane Road
#' @seealso \code{\link{ATS_1_i_2l}}, \code{\link{ATS_2_i_2l}}
#' @export ATS_total_2l Total average travel speed in type1 2-lane road(kph)
#' @examples
#' ATS_total_2l(L = 5, L_1 = c(1, 1.1, 0.9), L_2 = c(0.3, 1.7), ATS_1 = c(74, 28.2, 49.4), ATS_2 = c(56.5, 75.2))
ATS_total_2l <- function(L = NULL, L_1 = NULL, L_2 = NULL, ATS_1 = NULL, ATS_2 = NULL){
  lats1_sum <- 0
  lats2_sum <- 0
  if (L > 0){
    if (length(L_1) == length(ATS_1)){
      if (length(L_2) == length(ATS_2)){
        for (i in 1:length(L_1)){
          lats1 <- L_1[i]/ATS_1[i]
          lats1_sum <- lats1_sum + lats1
        }
        for (i in 1:length(L_2)){
          lats2 <- L_2[i]/ATS_2[i]
          lats2_sum <- lats2_sum + lats2
        }
        ats <- L / (lats1_sum + lats2_sum)
      }
      else {ats <- 'Error : The length of [L_2] series must be same with the length of [ATS_2] series.'}
    }
    else {ats <- 'Error : The length of [L_1] series must be same with the length of [ATS_1] series.'}
  }
  else {ats <- 'Error : [L] must be positive(km). Please check that.'}
  ats
}
