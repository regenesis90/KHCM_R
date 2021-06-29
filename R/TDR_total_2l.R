#' Total Delay Rate in 2-lane Road
#'
#' Total delay rate in 2-lane road(kph).
#'     This function follows <Formula 7-12> in KHCM(2013), p.176.
#' @param L Total length(km).
#' @param L_1 Series of ection i length (km) of section type1 2-lane road. See \code{\link{type_2l}}
#' @param L_2 Series of ection i length (km) of section type2 2-lane road. See \code{\link{type_2l}}
#' @param TDR_1 Series of TDR(%) in section i of section type1 2-lane road. See \code{\link{TDR_1_i_2l}}
#' @param TDR_2 Series of TDR(%) in section i of section type2 2-lane road. See \code{\link{TDR_2_i_2l}}
#' @keywords Average Travel Speed ATS 2-lane Road
#' @seealso \code{\link{TDR_1_i_2l}}, \code{\link{TDR_2_i_2l}}
#' @export TDR_total_2l TDR in type1 2-lane road(kph)
#' TDR_total_2l(L = 4, L_1 = c(2.1, 0.4), L_2 = c(1.0, 0.5), TDR_1 = c(30, 29.1), TDR_2 = c(4.52, 8.92))
TDR_total_2l <- function(L = NULL, L_1 = NULL, L_2 = NULL, TDR_1 = NULL, TDR_2 = NULL){
  tdr1_sum <- 0
  tdr2_sum <- 0
  if (L > 0){
    if (length(L_1) == length(TDR_1)){
      if (length(L_2) == length(TDR_2)){
        for (i in 1:length(L_1)){
          tdr1 <- TDR_1[i] * L_1[i] / L
          tdr1_sum <- tdr1_sum + tdr1
        }
        for (i in 1:length(L_2)){
          tdr2 <- TDR_2[i] * L_2[i] / L
          tdr2_sum <- tdr2_sum + tdr2
        }
        tdr <- tdr1_sum + tdr2_sum
      }
      else {tdr <- 'Error : The length of [L_2] series must be same with the length of [TDR_2] series.'}
    }
    else {tdr <- 'Error : The length of [L_1] series must be same with the length of [TDR_1] series.'}
  }
  else {tdr <- 'Error : [L] must be positive(km). Please check that.'}
  tdr
}
