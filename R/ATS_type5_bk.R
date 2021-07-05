#' Average Travel Speed in Bicycle Path on City Street
#'
#' Average travel speed in bicycle path on city street(km/h).
#'     It follows <Formula 15-17> in KHCM(2013), p.648.
#' @param L_T Total section length(km)
#' @param L Subsection length (km)
#' @param S Propulsion speed of section i (kph)
#' @param d Average stopping delay for bicycles at j intersections (seconds)
#' @export ATS_type5_bk
#' @examples
#' ATS_type5_bk(L_T = 10, L = c(3, 3.2, 3.8), S = c(15, 13.2, 10.5), d = c(300, 400))
ATS_type5_bk <- function(L_T = NULL, L = NULL, S = NULL, d = NULL){
  if (length(L) == length(S)){
    lssum <- 0
    dsum <- 0
    for (i in 1:length(L)){
      ls <- L[i]/S[i]
      lssum <- lssum + ls
    }
    for (j in 1:length(d)){
      ds <- d[j]/3600
      dsum <- dsum + ds
      print(d[j])
      print(ds)
      print(dsum)
    }
    spd <- L_T / (lssum + dsum)
  }
  else {spd <- 'Error : Length of [L] must be same with length of [S]. Please check that.'}
  spd
}
