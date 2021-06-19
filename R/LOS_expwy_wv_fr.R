#' Level of Service(LOS) in Expressway Weaving Section, Ramp Weave.
#'
#' This function decides Level of Service(LOS) in the expressway eaving section, ramp weave.
#'     It follows <Table 3-1> in KHCM(2013), p.63.
#' @param density The density of the road(pcpkmpl). See \code{\link{D_expwy_wv}}.
#' @param V Total traffic volume(pcph).
#' @param v_c_ratio V/C ratio.
#' @param S Average speed. See \code{\link{S_expwy_wv}}.
#' @keywords level of service expressway weaving section ramp weave
#' @seealso \code{\link{D_expwy_wv}}, \code{\link{S_expwy_wv}}, \code{\link{LOS_expwy_wv_ramp}}
#' @export LOS_expwy_wv_fr \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_expwy_wv_fr(density = 24)
#' LOS_expwy_wv_fr(V = 2930)
#' LOS_expwy_wv_fr(v_c_ratio = 0.38)
#' LOS_expwy_wv_fr(S = 38)
LOS_expwy_wv_fr <- function(density = NULL, V = NULL, v_c_ratio = NULL, S = NULL){
  if (is.null(density) == FALSE){
    if (density >= 0 & density <= 7.5){LOS <- 'A'}
    else if (density > 7.5 & density <= 12.5){LOS <- 'B'}
    else if (density > 12.5 & density <= 17.5){LOS <- 'C'}
    else if (density > 17.5 & density <= 25.0){LOS <- 'D'}
    else if (density > 25.0 & density <= 37.5){LOS <- 'E'}
    else if (density > 37.5){LOS <- 'F'}
    else {LOS <- 'Error : [density] must be positive(pcpkmpl). Please check that. See [D_expwy_wv()].'}
  }
  else {
    if (is.null(V) == FALSE){
      if (V >= 0 & V <= 1000){LOS <- 'A'}
      else if (V > 1000 & V <= 1500){LOS <- 'B'}
      else if (V > 1500 & V <= 2000){LOS <- 'C'}
      else if (V > 2000 & V <= 2500){LOS <- 'D'}
      else if (V > 2500 & V <= 3000){LOS <- 'E'}
      else if (V > 3000){LOS <- 'F'}
      else {LOS <- 'Error : [V] must be positive(pcph). Please check that.'}
    }
    else{
      if (is.null(v_c_ratio) == FALSE){
        if (v_c_ratio >= 0 & v_c_ratio <= 0.16){LOS <- 'A'}
        else if (v_c_ratio > 0.16 & v_c_ratio <= 0.33){LOS <- 'B'}
        else if (v_c_ratio > 0.33 & v_c_ratio <= 0.58){LOS <- 'C'}
        else if (v_c_ratio > 0.58 & v_c_ratio <= 0.83){LOS <- 'D'}
        else if (v_c_ratio > 0.83 & v_c_ratio <= 1.00){LOS <- 'E'}
        else if (v_c_ratio > 1.00){LOS <- 'F'}
        else {LOS <- 'Error : [v_c_ratio] must be positive. Please check that.'}
      }
      else {
        if(is.null(S) == FALSE){
          if (S > 65){LOS <- 'A'}
          else if (S < 65 & S >= 60){LOS <- 'B'}
          else if (S < 60 & S >= 55){LOS <- 'C'}
          else if (S < 55 & S >= 50){LOS <- 'D'}
          else if (S < 50 & S >= 40){LOS <- 'E'}
          else if (S < 40){LOS <- 'F'}
          else {LOS <- 'Error : [S] must be positive. Please check that.'}
        }
        else {LOS <- 'Error : At least one of the value is needed : [density], [V], [v_c_ratio], [S]. The value must be positive.'}
    }
    }
  }
  LOS
}
