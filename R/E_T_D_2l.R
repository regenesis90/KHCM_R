#' Heavy Vehicle Correction Factor according to the Traffic Volume in the Direction(E_T)
#'
#' This function follows <Table 7-4>, <Table 7-5>, <Table 7-6>, <Table 7-7> (p.177)
#' @param landform landform Choose one from : \code{'flatland'}, \code{'hill'}, \code{'specific_slope}
#' @param slope If \code{landform = 'specific_slope'}, must use it. The gradient of slope(%). Choose one from : \code{0.07}, \code{0.06}, \code{0.05}, \code{0.04}, \code{0.03}
#' @param v_d If \code{landform = 'specific_slope'}, must use it. Traffic volume in direction(vph)
#' @param L If \code{landform = 'specific_slope'}, must use it. The length of the slope(m). Choose one from : \code{400}, \code{800}, \code{1600}, \code{2400}, \code{3200}, \code{4800}, \code{6400}
#' @export E_T_D_2l Heavy Vehicle Correction Factor in 2-lane road
#' @examples
#' E_T_2l(landform = 'flatland', v_d = 332)
#' E_T_2l(landform = 'hill', v_d = 459)
#' E_T_2l(landform = 'specific_slope', slope = 0.04, L = 1200, v_d = 838)
E_T_2l <- function(landform = NULL, slope = NULL, L = NULL, v_d = NULL){
  if (landform == 'flatland'){
    if (v_d == 100){E_T <- 2.6}
    else if (v_d > 100 & v_d < 200){E_T <- 2.6 - 0.3 * (v_d - 100) * 0.01}
    else if (v_d == 200){E_T <- 2.3}
    else if (v_d > 200 & v_d < 300){E_T <- 2.3 - 0.2 * (v_d - 200) * 0.01}
    else if (v_d == 300){E_T <- 2.1}
    else if (v_d > 300 & v_d < 400){E_T <- 2.1 - 0.2 * (v_d - 300) * 0.01}
    else if (v_d == 400){E_T <- 1.9}
    else if (v_d > 400 & v_d < 500){E_T <- 1.9 - 0.3 * (v_d - 400) * 0.01}
    else if (v_d == 500){E_T <- 1.6}
    else if (v_d > 500 & v_d < 600){E_T <- 1.6 - 0.2 * (v_d - 500) * 0.01}
    else if (v_d == 600){E_T <- 1.4}
    else if (v_d > 600 & v_d < 700){E_T <- 1.4 - 0.2 * (v_d - 600) * 0.01}
    else if (v_d == 700){E_T <- 1.2}
    else if (v_d > 700 & v_d < 800){E_T <- 1.2 - 0.2 * (v_d - 700) * 0.01}
    else if (v_d == 800){E_T <- 1.0}
    else if (v_d > 800 & v_d < 900){E_T <- 1.0}
    else if (v_d == 900){E_T <- 1.0}
    else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
    else if (v_d == 1000){E_T <- 1.0}
    else if (v_d > 1000){E_T <- 1.0}
    else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
  }
  else if (landform == 'hill'){
    if (v_d == 100){E_T <- 3.7}
    else if (v_d > 100 & v_d < 200){E_T <- 3.7 - 0.4 * (v_d - 100) * 0.01}
    else if (v_d == 200){E_T <- 3.3}
    else if (v_d > 200 & v_d < 300){E_T <- 3.3 - 0.3 * (v_d - 200) * 0.01}
    else if (v_d == 300){E_T <- 3.0}
    else if (v_d > 300 & v_d < 400){E_T <- 3.0 - 0.4 * (v_d - 300) * 0.01}
    else if (v_d == 400){E_T <- 2.6}
    else if (v_d > 400 & v_d < 500){E_T <- 2.6 - 0.3 * (v_d - 400) * 0.01}
    else if (v_d == 500){E_T <- 2.3}
    else if (v_d > 500 & v_d < 600){E_T <- 2.3 - 0.3 * (v_d - 500) * 0.01}
    else if (v_d == 600){E_T <- 2.0}
    else if (v_d > 600 & v_d < 700){E_T <- 2.0 - 0.4 * (v_d - 600) * 0.01}
    else if (v_d == 700){E_T <- 1.6}
    else if (v_d > 700 & v_d < 800){E_T <- 1.6 - 0.3 * (v_d - 700) * 0.01}
    else if (v_d == 800){E_T <- 1.3}
    else if (v_d > 700 & v_d < 800){E_T <- 1.3 - 0.3 * (v_d - 800) * 0.01}
    else if (v_d == 900){E_T <- 1.0}
    else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
    else if (v_d == 1000){E_T <- 1.0}
    else if (v_d > 1000){E_T <- 1.0}
    else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
  }
  else if (landform == 'specific_slope'){
    if (slope == 0.03){
      if (L == 400){
        if (v_d > 0 & v_d < 100){E_T <- 1.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 1.4}
        else if (v_d > 100 & v_d < 200){E_T <- 1.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 1.1}
        else if (v_d > 200 & v_d < 300){E_T <- 1.1 - 0.1 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 1.0}
        else if (v_d > 300 & v_d < 400){E_T <- 1.0}
        else if (v_d == 400){E_T <- 1.0}
        else if (v_d > 400 & v_d < 500){E_T <- 1.0}
        else if (v_d == 500){E_T <- 1.0}
        else if (v_d > 500 & v_d < 600){E_T <- 1.0}
        else if (v_d == 600){E_T <- 1.0}
        else if (v_d > 600 & v_d < 700){E_T <- 1.0}
        else if (v_d == 700){E_T <- 1.0}
        else if (v_d > 700 & v_d < 800){E_T <- 1.0}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 800){
        if (v_d > 0 & v_d < 100){E_T <- 1.7 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 1.7}
        else if (v_d > 100 & v_d < 200){E_T <- 1.7 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 1.3}
        else if (v_d > 200 & v_d < 300){E_T <- 1.3 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 1.0}
        else if (v_d > 300 & v_d < 400){E_T <- 1.0}
        else if (v_d == 400){E_T <- 1.0}
        else if (v_d > 400 & v_d < 500){E_T <- 1.0}
        else if (v_d == 500){E_T <- 1.0}
        else if (v_d > 500 & v_d < 600){E_T <- 1.0}
        else if (v_d == 600){E_T <- 1.0}
        else if (v_d > 600 & v_d < 700){E_T <- 1.0}
        else if (v_d == 700){E_T <- 1.0}
        else if (v_d > 700 & v_d < 800){E_T <- 1.0}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1200){
        if (v_d > 0 & v_d < 100){E_T <- 1.9 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 1.9}
        else if (v_d > 100 & v_d < 200){E_T <- 1.9 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 1.6}
        else if (v_d > 200 & v_d < 300){E_T <- 1.6 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 1.3}
        else if (v_d > 300 & v_d < 400){E_T <- 1.3 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 1.0}
        else if (v_d > 400 & v_d < 500){E_T <- 1.0}
        else if (v_d == 500){E_T <- 1.0}
        else if (v_d > 500 & v_d < 600){E_T <- 1.0}
        else if (v_d == 600){E_T <- 1.0}
        else if (v_d > 600 & v_d < 700){E_T <- 1.0}
        else if (v_d == 700){E_T <- 1.0}
        else if (v_d > 700 & v_d < 800){E_T <- 1.0}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1600){
        if (v_d > 0 & v_d < 100){E_T <- 2.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 2.2}
        else if (v_d > 100 & v_d < 200){E_T <- 2.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 1.9}
        else if (v_d > 200 & v_d < 300){E_T <- 1.9 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 1.6}
        else if (v_d > 300 & v_d < 400){E_T <- 1.6 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 1.2}
        else if (v_d > 400 & v_d < 500){E_T <- 1.2 - 0.2 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 1.0}
        else if (v_d > 500 & v_d < 600){E_T <- 1.0}
        else if (v_d == 600){E_T <- 1.0}
        else if (v_d > 600 & v_d < 700){E_T <- 1.0}
        else if (v_d == 700){E_T <- 1.0}
        else if (v_d > 700 & v_d < 800){E_T <- 1.0}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 2400){
        if (v_d > 0 & v_d < 100){E_T <- 2.8 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 2.8}
        else if (v_d > 100 & v_d < 200){E_T <- 2.8 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 2.4}
        else if (v_d > 200 & v_d < 300){E_T <- 2.4 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 2.1}
        else if (v_d > 300 & v_d < 400){E_T <- 2.1 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 1.8}
        else if (v_d > 400 & v_d < 500){E_T <- 1.8 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 1.4}
        else if (v_d > 500 & v_d < 600){E_T <- 1.4 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 1.1}
        else if (v_d > 600 & v_d < 700){E_T <- 1.1 - 0.1 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 1.0}
        else if (v_d > 700 & v_d < 800){E_T <- 1.0}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 3200){
        if (v_d > 0 & v_d < 100){E_T <- 3.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 3.3}
        else if (v_d > 100 & v_d < 200){E_T <- 3.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 3.0}
        else if (v_d > 200 & v_d < 300){E_T <- 3.0 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 2.7}
        else if (v_d > 300 & v_d < 400){E_T <- 2.7 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 2.3}
        else if (v_d > 400 & v_d < 500){E_T <- 2.3 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 2.0}
        else if (v_d > 500 & v_d < 600){E_T <- 2.0 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 1.7}
        else if (v_d > 600 & v_d < 700){E_T <- 1.7 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 1.3}
        else if (v_d > 700 & v_d < 800){E_T <- 1.3 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 4800){
        if (v_d > 0 & v_d < 100){E_T <- 4.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.4}
        else if (v_d > 100 & v_d < 200){E_T <- 4.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 4.1}
        else if (v_d > 200 & v_d < 300){E_T <- 4.1 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.8}
        else if (v_d > 300 & v_d < 400){E_T <- 3.8 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.4}
        else if (v_d > 400 & v_d < 500){E_T <- 3.4 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 3.1}
        else if (v_d > 500 & v_d < 600){E_T <- 3.1 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.8}
        else if (v_d > 600 & v_d < 700){E_T <- 2.8 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.4}
        else if (v_d > 700 & v_d < 800){E_T <- 2.4 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 2.1}
        else if (v_d > 800 & v_d < 900){E_T <- 2.1 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.8}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.8 - 0.4 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.4}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 6400){
        if (v_d > 0 & v_d < 100){E_T <- 5.5 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 5.5}
        else if (v_d > 100 & v_d < 200){E_T <- 5.5 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 5.2}
        else if (v_d > 200 & v_d < 300){E_T <- 5.2 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 4.9}
        else if (v_d > 300 & v_d < 400){E_T <- 4.9 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 4.5}
        else if (v_d > 400 & v_d < 500){E_T <- 4.5 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 4.2}
        else if (v_d > 500 & v_d < 600){E_T <- 4.2 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 3.9}
        else if (v_d > 600 & v_d < 700){E_T <- 3.9 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 3.5}
        else if (v_d > 700 & v_d < 800){E_T <- 3.5 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 3.2}
        else if (v_d > 800 & v_d < 900){E_T <- 3.2 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 2.9}
        else if (v_d > 900 & v_d < 1000){E_T <- 2.9 - 0.4 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 2.5}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else {E_T <- 'Error : [L] must be one of 400, 800, 1200, 1600, 2400, 3200, 4800, 6400. Please check that.'}
    }
    else if (slope == 0.04){
      if (L == 400){
        if (v_d > 0 & v_d < 100){E_T <- 2.3 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 2.3}
        else if (v_d > 100 & v_d < 200){E_T <- 2.3 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 1.9}
        else if (v_d > 200 & v_d < 300){E_T <- 1.9 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 1.6}
        else if (v_d > 300 & v_d < 400){E_T <- 1.6 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 1.3}
        else if (v_d > 400 & v_d < 500){E_T <- 1.3 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 1.0}
        else if (v_d > 500 & v_d < 600){E_T <- 1.0}
        else if (v_d == 600){E_T <- 1.0}
        else if (v_d > 600 & v_d < 700){E_T <- 1.0}
        else if (v_d == 700){E_T <- 1.0}
        else if (v_d > 700 & v_d < 800){E_T <- 1.0}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 800){
        if (v_d > 0 & v_d < 100){E_T <- 2.6 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 2.6}
        else if (v_d > 100 & v_d < 200){E_T <- 2.6 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 2.2}
        else if (v_d > 200 & v_d < 300){E_T <- 2.2 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 1.9}
        else if (v_d > 300 & v_d < 400){E_T <- 1.9 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 1.5}
        else if (v_d > 400 & v_d < 500){E_T <- 1.5 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 1.2}
        else if (v_d > 500 & v_d < 600){E_T <- 1.2 - 0.2 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 1.0}
        else if (v_d > 600 & v_d < 700){E_T <- 1.0}
        else if (v_d == 700){E_T <- 1.0}
        else if (v_d > 700 & v_d < 800){E_T <- 1.0}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1200){
        if (v_d > 0 & v_d < 100){E_T <- 2.8 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 2.8}
        else if (v_d > 100 & v_d < 200){E_T <- 2.8 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 2.5}
        else if (v_d > 200 & v_d < 300){E_T <- 2.5 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 2.2}
        else if (v_d > 300 & v_d < 400){E_T <- 2.2 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 1.8}
        else if (v_d > 400 & v_d < 500){E_T <- 1.8 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 1.5}
        else if (v_d > 500 & v_d < 600){E_T <- 1.5 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 1.2}
        else if (v_d > 600 & v_d < 700){E_T <- 1.2 - 0.2 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 1.0}
        else if (v_d > 700 & v_d < 800){E_T <- 1.0}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1600){
        if (v_d > 0 & v_d < 100){E_T <- 3.1 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 3.1}
        else if (v_d > 100 & v_d < 200){E_T <- 3.1 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 2.8}
        else if (v_d > 200 & v_d < 300){E_T <- 2.8 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 2.4}
        else if (v_d > 300 & v_d < 400){E_T <- 2.4 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 2.1}
        else if (v_d > 400 & v_d < 500){E_T <- 2.1 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 1.8}
        else if (v_d > 500 & v_d < 600){E_T <- 1.8 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 1.4}
        else if (v_d > 600 & v_d < 700){E_T <- 1.4 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 1.1}
        else if (v_d > 700 & v_d < 800){E_T <- 1.1 - 0.1 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 2400){
        if (v_d > 0 & v_d < 100){E_T <- 3.7 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 3.7}
        else if (v_d > 100 & v_d < 200){E_T <- 3.7 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 3.3}
        else if (v_d > 200 & v_d < 300){E_T <- 3.3 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.0}
        else if (v_d > 300 & v_d < 400){E_T <- 3.0 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 2.7}
        else if (v_d > 400 & v_d < 500){E_T <- 2.7 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 2.3}
        else if (v_d > 500 & v_d < 600){E_T <- 2.3 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.0}
        else if (v_d > 600 & v_d < 700){E_T <- 2.0 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 1.7}
        else if (v_d > 700 & v_d < 800){E_T <- 1.7 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.3}
        else if (v_d > 800 & v_d < 900){E_T <- 1.3 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 3200){
        if (v_d > 0 & v_d < 100){E_T <- 4.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.2}
        else if (v_d > 100 & v_d < 200){E_T <- 4.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 3.9}
        else if (v_d > 200 & v_d < 300){E_T <- 3.9 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.5}
        else if (v_d > 300 & v_d < 400){E_T <- 3.5 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.2}
        else if (v_d > 400 & v_d < 500){E_T <- 3.2 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 2.9}
        else if (v_d > 500 & v_d < 600){E_T <- 2.9 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.5}
        else if (v_d > 600 & v_d < 700){E_T <- 2.5 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.2}
        else if (v_d > 700 & v_d < 800){E_T <- 2.2 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.9}
        else if (v_d > 800 & v_d < 900){E_T <- 1.9 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.5}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.5 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.2}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 4800){
        if (v_d > 0 & v_d < 100){E_T <- 5.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 5.3}
        else if (v_d > 100 & v_d < 200){E_T <- 5.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 5.0}
        else if (v_d > 200 & v_d < 300){E_T <- 5.0 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 4.6}
        else if (v_d > 300 & v_d < 400){E_T <- 4.6 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 4.3}
        else if (v_d > 400 & v_d < 500){E_T <- 4.3 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 4.0}
        else if (v_d > 500 & v_d < 600){E_T <- 4.0 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 3.6}
        else if (v_d > 600 & v_d < 700){E_T <- 3.6 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 3.3}
        else if (v_d > 700 & v_d < 800){E_T <- 3.3 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 3.0}
        else if (v_d > 800 & v_d < 900){E_T <- 3.0 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 2.6}
        else if (v_d > 900 & v_d < 1000){E_T <- 2.6 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 2.3}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 6400){
        if (v_d > 0 & v_d < 100){E_T <- 6.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 6.4}
        else if (v_d > 100 & v_d < 200){E_T <- 6.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 6.1}
        else if (v_d > 200 & v_d < 300){E_T <- 6.1 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 5.8}
        else if (v_d > 300 & v_d < 400){E_T <- 5.8 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 5.4}
        else if (v_d > 400 & v_d < 500){E_T <- 5.4 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 5.1}
        else if (v_d > 500 & v_d < 600){E_T <- 5.1 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 4.8}
        else if (v_d > 600 & v_d < 700){E_T <- 4.8 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 4.4}
        else if (v_d > 700 & v_d < 800){E_T <- 4.4 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 4.1}
        else if (v_d > 800 & v_d < 900){E_T <- 4.1 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 3.7}
        else if (v_d > 900 & v_d < 1000){E_T <- 3.7 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 3.4}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else {E_T <- 'Error : [L] must be one of 400, 800, 1200, 1600, 2400, 3200, 4800, 6400. Please check that.'}
    }
    else if (slope == 0.05){
      if (L == 400){
        if (v_d > 0 & v_d < 100){E_T <- 3.2 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 3.2}
        else if (v_d > 100 & v_d < 200){E_T <- 3.2 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 2.8}
        else if (v_d > 200 & v_d < 300){E_T <- 2.8 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 2.5}
        else if (v_d > 300 & v_d < 400){E_T <- 2.5 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 2.2}
        else if (v_d > 400 & v_d < 500){E_T <- 2.2 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 1.8}
        else if (v_d > 500 & v_d < 600){E_T <- 1.8 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 1.5}
        else if (v_d > 600 & v_d < 700){E_T <- 1.5 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 1.2}
        else if (v_d > 700 & v_d < 800){E_T <- 1.2 - 0.2 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.0}
        else if (v_d > 800 & v_d < 900){E_T <- 1.0}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 800){
        if (v_d > 0 & v_d < 100){E_T <- 3.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 3.4}
        else if (v_d > 100 & v_d < 200){E_T <- 3.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 3.1}
        else if (v_d > 200 & v_d < 300){E_T <- 3.1 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 2.8}
        else if (v_d > 300 & v_d < 400){E_T <- 2.8 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 2.4}
        else if (v_d > 400 & v_d < 500){E_T <- 2.4 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 2.1}
        else if (v_d > 500 & v_d < 600){E_T <- 2.1 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 1.8}
        else if (v_d > 600 & v_d < 700){E_T <- 1.8 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 1.4}
        else if (v_d > 700 & v_d < 800){E_T <- 1.4 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.1}
        else if (v_d > 800 & v_d < 900){E_T <- 1.1 - 0.1 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1200){
        if (v_d > 0 & v_d < 100){E_T <- 3.7 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 3.7}
        else if (v_d > 100 & v_d < 200){E_T <- 3.7 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 3.4}
        else if (v_d > 200 & v_d < 300){E_T <- 3.4 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.0}
        else if (v_d > 300 & v_d < 400){E_T <- 3.0 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 2.7}
        else if (v_d > 400 & v_d < 500){E_T <- 2.7 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 2.4}
        else if (v_d > 500 & v_d < 600){E_T <- 2.4 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.0}
        else if (v_d > 600 & v_d < 700){E_T <- 2.0 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 1.7}
        else if (v_d > 700 & v_d < 800){E_T <- 1.7 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.4}
        else if (v_d > 800 & v_d < 900){E_T <- 1.4 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.0}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.0}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1600){
        if (v_d > 0 & v_d < 100){E_T <- 4.0 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.0}
        else if (v_d > 100 & v_d < 200){E_T <- 4.0 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 3.7}
        else if (v_d > 200 & v_d < 300){E_T <- 3.7 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.3}
        else if (v_d > 300 & v_d < 400){E_T <- 3.3 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.0}
        else if (v_d > 400 & v_d < 500){E_T <- 3.0 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 2.7}
        else if (v_d > 500 & v_d < 600){E_T <- 2.7 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.3}
        else if (v_d > 600 & v_d < 700){E_T <- 2.3 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.0}
        else if (v_d > 700 & v_d < 800){E_T <- 2.0 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.6}
        else if (v_d > 800 & v_d < 900){E_T <- 1.6 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.3}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.3 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 2400){
        if (v_d > 0 & v_d < 100){E_T <- 4.5 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.5}
        else if (v_d > 100 & v_d < 200){E_T <- 4.5 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 4.2}
        else if (v_d > 200 & v_d < 300){E_T <- 4.2 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.9}
        else if (v_d > 300 & v_d < 400){E_T <- 3.9 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.5}
        else if (v_d > 400 & v_d < 500){E_T <- 3.5 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 3.2}
        else if (v_d > 500 & v_d < 600){E_T <- 3.2 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.9}
        else if (v_d > 600 & v_d < 700){E_T <- 2.9 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.5}
        else if (v_d > 700 & v_d < 800){E_T <- 2.5 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 2.2}
        else if (v_d > 800 & v_d < 900){E_T <- 2.2 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.9}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.9 - 0.4 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.5}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 3200){
        if (v_d > 0 & v_d < 100){E_T <- 5.1 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 5.1}
        else if (v_d > 100 & v_d < 200){E_T <- 5.1 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 4.8}
        else if (v_d > 200 & v_d < 300){E_T <- 4.8 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 4.4}
        else if (v_d > 300 & v_d < 400){E_T <- 4.4 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 4.1}
        else if (v_d > 400 & v_d < 500){E_T <- 4.1 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 3.8}
        else if (v_d > 500 & v_d < 600){E_T <- 3.8 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 3.4}
        else if (v_d > 600 & v_d < 700){E_T <- 3.4 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 3.1}
        else if (v_d > 700 & v_d < 800){E_T <- 3.1 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 2.8}
        else if (v_d > 800 & v_d < 900){E_T <- 2.8 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 2.4}
        else if (v_d > 900 & v_d < 1000){E_T <- 2.4 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 2.1}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 4800){
        if (v_d > 0 & v_d < 100){E_T <- 6.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 6.2}
        else if (v_d > 100 & v_d < 200){E_T <- 6.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 5.9}
        else if (v_d > 200 & v_d < 300){E_T <- 5.9 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 5.5}
        else if (v_d > 300 & v_d < 400){E_T <- 5.5 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 5.2}
        else if (v_d > 400 & v_d < 500){E_T <- 5.2 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 4.9}
        else if (v_d > 500 & v_d < 600){E_T <- 4.9 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 4.5}
        else if (v_d > 600 & v_d < 700){E_T <- 4.5 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 4.2}
        else if (v_d > 700 & v_d < 800){E_T <- 4.2 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 3.9}
        else if (v_d > 800 & v_d < 900){E_T <- 3.9 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 3.5}
        else if (v_d > 900 & v_d < 1000){E_T <- 3.5 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 3.2}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 6400){
        if (v_d > 0 & v_d < 100){E_T <- 7.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 7.3}
        else if (v_d > 100 & v_d < 200){E_T <- 7.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 7.0}
        else if (v_d > 200 & v_d < 300){E_T <- 7.0 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 6.6}
        else if (v_d > 300 & v_d < 400){E_T <- 6.6 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 6.3}
        else if (v_d > 400 & v_d < 500){E_T <- 6.3 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 6.0}
        else if (v_d > 500 & v_d < 600){E_T <- 6.0 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 5.6}
        else if (v_d > 600 & v_d < 700){E_T <- 5.6 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 5.3}
        else if (v_d > 700 & v_d < 800){E_T <- 5.3 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 5.0}
        else if (v_d > 800 & v_d < 900){E_T <- 5.0 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 4.6}
        else if (v_d > 900 & v_d < 1000){E_T <- 4.6 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 4.3}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else {E_T <- 'Error : [L] must be one of 400, 800, 1200, 1600, 2400, 3200, 4800, 6400. Please check that.'}
    }
    else if (slope == 0.06){
      if (L == 400){
        if (v_d > 0 & v_d < 100){E_T <- 4.0 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.0}
        else if (v_d > 100 & v_d < 200){E_T <- 4.0 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 3.7}
        else if (v_d > 200 & v_d < 300){E_T <- 3.7 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.4}
        else if (v_d > 300 & v_d < 400){E_T <- 3.4 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.0}
        else if (v_d > 400 & v_d < 500){E_T <- 3.0 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 2.7}
        else if (v_d > 500 & v_d < 600){E_T <- 2.7 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.4}
        else if (v_d > 600 & v_d < 700){E_T <- 2.4 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.0}
        else if (v_d > 700 & v_d < 800){E_T <- 2.0 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 1.7}
        else if (v_d > 800 & v_d < 900){E_T <- 1.7 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.4}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.4 - 0.4 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 800){
        if (v_d > 0 & v_d < 100){E_T <- 4.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.3}
        else if (v_d > 100 & v_d < 200){E_T <- 4.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 4.0}
        else if (v_d > 200 & v_d < 300){E_T <- 4.0 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.6}
        else if (v_d > 300 & v_d < 400){E_T <- 3.6 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.3}
        else if (v_d > 400 & v_d < 500){E_T <- 3.3 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 3.0}
        else if (v_d > 500 & v_d < 600){E_T <- 3.0 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.6}
        else if (v_d > 600 & v_d < 700){E_T <- 2.6 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.3}
        else if (v_d > 700 & v_d < 800){E_T <- 2.3 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 2.0}
        else if (v_d > 800 & v_d < 900){E_T <- 2.0 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.6}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.6 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.3}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1200){
        if (v_d > 0 & v_d < 100){E_T <- 4.6 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.6}
        else if (v_d > 100 & v_d < 200){E_T <- 4.6 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 4.3}
        else if (v_d > 200 & v_d < 300){E_T <- 4.3 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 3.9}
        else if (v_d > 300 & v_d < 400){E_T <- 3.9 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.6}
        else if (v_d > 400 & v_d < 500){E_T <- 3.6 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 3.3}
        else if (v_d > 500 & v_d < 600){E_T <- 3.3 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 2.9}
        else if (v_d > 600 & v_d < 700){E_T <- 2.9 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.6}
        else if (v_d > 700 & v_d < 800){E_T <- 2.6 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 2.3}
        else if (v_d > 800 & v_d < 900){E_T <- 2.3 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 1.9}
        else if (v_d > 900 & v_d < 1000){E_T <- 1.9 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.6}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1600){
        if (v_d > 0 & v_d < 100){E_T <- 4.9 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.9}
        else if (v_d > 100 & v_d < 200){E_T <- 4.9 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 4.5}
        else if (v_d > 200 & v_d < 300){E_T <- 4.5 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 4.2}
        else if (v_d > 300 & v_d < 400){E_T <- 4.2 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.9}
        else if (v_d > 400 & v_d < 500){E_T <- 3.9 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 3.5}
        else if (v_d > 500 & v_d < 600){E_T <- 3.5 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 3.2}
        else if (v_d > 600 & v_d < 700){E_T <- 3.2 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.9}
        else if (v_d > 700 & v_d < 800){E_T <- 2.9 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 2.5}
        else if (v_d > 800 & v_d < 900){E_T <- 2.5 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 2.2}
        else if (v_d > 900 & v_d < 1000){E_T <- 2.2 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.9}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 2400){
        if (v_d > 0 & v_d < 100){E_T <- 5.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 5.4}
        else if (v_d > 100 & v_d < 200){E_T <- 5.4 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 5.1}
        else if (v_d > 200 & v_d < 300){E_T <- 5.1 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 4.8}
        else if (v_d > 300 & v_d < 400){E_T <- 4.8 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 4.4}
        else if (v_d > 400 & v_d < 500){E_T <- 4.4 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 4.1}
        else if (v_d > 500 & v_d < 600){E_T <- 4.1 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 3.8}
        else if (v_d > 600 & v_d < 700){E_T <- 3.8 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 3.4}
        else if (v_d > 700 & v_d < 800){E_T <- 3.4 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 3.1}
        else if (v_d > 800 & v_d < 900){E_T <- 3.1 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 2.7}
        else if (v_d > 900 & v_d < 1000){E_T <- 2.7 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 2.4}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 3200){
        if (v_d > 0 & v_d < 100){E_T <- 6.0 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 6.0}
        else if (v_d > 100 & v_d < 200){E_T <- 6.0 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 5.6}
        else if (v_d > 200 & v_d < 300){E_T <- 5.6 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 5.3}
        else if (v_d > 300 & v_d < 400){E_T <- 5.3 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 5.0}
        else if (v_d > 400 & v_d < 500){E_T <- 5.0 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 4.6}
        else if (v_d > 500 & v_d < 600){E_T <- 4.6 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 4.3}
        else if (v_d > 600 & v_d < 700){E_T <- 4.3 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 4.0}
        else if (v_d > 700 & v_d < 800){E_T <- 4.0 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 3.6}
        else if (v_d > 800 & v_d < 900){E_T <- 3.6 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 3.3}
        else if (v_d > 900 & v_d < 1000){E_T <- 3.3 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 3.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 4800){
        if (v_d > 0 & v_d < 100){E_T <- 7.1 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 7.1}
        else if (v_d > 100 & v_d < 200){E_T <- 7.1 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 6.7}
        else if (v_d > 200 & v_d < 300){E_T <- 6.7 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 6.4}
        else if (v_d > 300 & v_d < 400){E_T <- 6.4 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 6.1}
        else if (v_d > 400 & v_d < 500){E_T <- 6.1 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 5.7}
        else if (v_d > 500 & v_d < 600){E_T <- 5.7 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 5.4}
        else if (v_d > 600 & v_d < 700){E_T <- 5.4 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 5.1}
        else if (v_d > 700 & v_d < 800){E_T <- 5.1 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 4.7}
        else if (v_d > 800 & v_d < 900){E_T <- 4.7 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 4.4}
        else if (v_d > 900 & v_d < 1000){E_T <- 4.4 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 4.1}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 6400){
        if (v_d > 0 & v_d < 100){E_T <- 8.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 8.2}
        else if (v_d > 100 & v_d < 200){E_T <- 8.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 7.9}
        else if (v_d > 200 & v_d < 300){E_T <- 7.9 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 7.5}
        else if (v_d > 300 & v_d < 400){E_T <- 7.5 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 7.2}
        else if (v_d > 400 & v_d < 500){E_T <- 7.2 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 6.8}
        else if (v_d > 500 & v_d < 600){E_T <- 6.8 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 6.5}
        else if (v_d > 600 & v_d < 700){E_T <- 6.5 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 6.2}
        else if (v_d > 700 & v_d < 800){E_T <- 6.2 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 5.8}
        else if (v_d > 800 & v_d < 900){E_T <- 5.8 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 5.5}
        else if (v_d > 900 & v_d < 1000){E_T <- 5.5 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 5.2}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else {E_T <- 'Error : [L] must be one of 400, 800, 1200, 1600, 2400, 3200, 4800, 6400. Please check that.'}
    }
    else if (slope == 0.07){
      if (L == 400){
        if (v_d > 0 & v_d < 100){E_T <- 4.9 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 4.9}
        else if (v_d > 100 & v_d < 200){E_T <- 4.9 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 4.6}
        else if (v_d > 200 & v_d < 300){E_T <- 4.6 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 4.3}
        else if (v_d > 300 & v_d < 400){E_T <- 4.3 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 3.9}
        else if (v_d > 400 & v_d < 500){E_T <- 3.9 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 3.6}
        else if (v_d > 500 & v_d < 600){E_T <- 3.6 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 3.3}
        else if (v_d > 600 & v_d < 700){E_T <- 3.3 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 2.9}
        else if (v_d > 700 & v_d < 800){E_T <- 2.9 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 2.6}
        else if (v_d > 800 & v_d < 900){E_T <- 2.6 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 2.2}
        else if (v_d > 900 & v_d < 1000){E_T <- 2.2 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 1.9}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 800){
        if (v_d > 0 & v_d < 100){E_T <- 5.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 5.2}
        else if (v_d > 100 & v_d < 200){E_T <- 5.2 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 4.9}
        else if (v_d > 200 & v_d < 300){E_T <- 4.9 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 4.5}
        else if (v_d > 300 & v_d < 400){E_T <- 4.5 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 4.2}
        else if (v_d > 400 & v_d < 500){E_T <- 4.2 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 3.9}
        else if (v_d > 500 & v_d < 600){E_T <- 3.9 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 3.5}
        else if (v_d > 600 & v_d < 700){E_T <- 3.5 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 3.2}
        else if (v_d > 700 & v_d < 800){E_T <- 3.2 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 2.9}
        else if (v_d > 800 & v_d < 900){E_T <- 2.9 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 2.5}
        else if (v_d > 900 & v_d < 1000){E_T <- 2.5 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 2.2}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1200){
        if (v_d > 0 & v_d < 100){E_T <- 5.5 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 5.5}
        else if (v_d > 100 & v_d < 200){E_T <- 5.5 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 5.1}
        else if (v_d > 200 & v_d < 300){E_T <- 5.1 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 4.8}
        else if (v_d > 300 & v_d < 400){E_T <- 4.8 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 4.5}
        else if (v_d > 400 & v_d < 500){E_T <- 4.5 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 4.1}
        else if (v_d > 500 & v_d < 600){E_T <- 4.1 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 3.8}
        else if (v_d > 600 & v_d < 700){E_T <- 3.8 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 3.5}
        else if (v_d > 700 & v_d < 800){E_T <- 3.5 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 3.1}
        else if (v_d > 800 & v_d < 900){E_T <- 3.1 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 2.8}
        else if (v_d > 900 & v_d < 1000){E_T <- 2.8 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 2.5}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 1600){
        if (v_d > 0 & v_d < 100){E_T <- 5.8 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 5.8}
        else if (v_d > 100 & v_d < 200){E_T <- 5.8 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 5.4}
        else if (v_d > 200 & v_d < 300){E_T <- 5.4 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 5.1}
        else if (v_d > 300 & v_d < 400){E_T <- 5.1 - 0.4 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 4.7}
        else if (v_d > 400 & v_d < 500){E_T <- 4.7 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 4.4}
        else if (v_d > 500 & v_d < 600){E_T <- 4.4 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 4.1}
        else if (v_d > 600 & v_d < 700){E_T <- 4.1 - 0.4 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 3.7}
        else if (v_d > 700 & v_d < 800){E_T <- 3.7 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 3.4}
        else if (v_d > 800 & v_d < 900){E_T <- 3.4 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 3.1}
        else if (v_d > 900 & v_d < 1000){E_T <- 3.1 - 0.4 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 2.7}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 2400){
        if (v_d > 0 & v_d < 100){E_T <- 6.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 6.3}
        else if (v_d > 100 & v_d < 200){E_T <- 6.3 - 0.3 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 6.0}
        else if (v_d > 200 & v_d < 300){E_T <- 6.0 - 0.4 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 5.6}
        else if (v_d > 300 & v_d < 400){E_T <- 5.6 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 5.3}
        else if (v_d > 400 & v_d < 500){E_T <- 5.3 - 0.3 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 5.0}
        else if (v_d > 500 & v_d < 600){E_T <- 5.0 - 0.4 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 4.6}
        else if (v_d > 600 & v_d < 700){E_T <- 4.6 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 4.3}
        else if (v_d > 700 & v_d < 800){E_T <- 4.3 - 0.3 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 4.0}
        else if (v_d > 800 & v_d < 900){E_T <- 4.0 - 0.4 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 3.6}
        else if (v_d > 900 & v_d < 1000){E_T <- 3.6 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 3.3}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 3200){
        if (v_d > 0 & v_d < 100){E_T <- 6.9 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 6.9}
        else if (v_d > 100 & v_d < 200){E_T <- 6.9 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 6.5}
        else if (v_d > 200 & v_d < 300){E_T <- 6.5 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 6.2}
        else if (v_d > 300 & v_d < 400){E_T <- 6.2 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 5.9}
        else if (v_d > 400 & v_d < 500){E_T <- 5.9 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 5.5}
        else if (v_d > 500 & v_d < 600){E_T <- 5.5 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 5.2}
        else if (v_d > 600 & v_d < 700){E_T <- 5.2 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 4.9}
        else if (v_d > 700 & v_d < 800){E_T <- 4.9 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 4.5}
        else if (v_d > 800 & v_d < 900){E_T <- 4.5 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 4.2}
        else if (v_d > 900 & v_d < 1000){E_T <- 4.2 - 0.4 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 3.8}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 4800){
        if (v_d > 0 & v_d < 100){E_T <- 8.0 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 8.0}
        else if (v_d > 100 & v_d < 200){E_T <- 8.0 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 7.6}
        else if (v_d > 200 & v_d < 300){E_T <- 7.6 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 7.3}
        else if (v_d > 300 & v_d < 400){E_T <- 7.3 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 7.0}
        else if (v_d > 400 & v_d < 500){E_T <- 7.0 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 6.6}
        else if (v_d > 500 & v_d < 600){E_T <- 6.6 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 6.3}
        else if (v_d > 600 & v_d < 700){E_T <- 6.3 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 6.0}
        else if (v_d > 700 & v_d < 800){E_T <- 6.0 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 5.6}
        else if (v_d > 800 & v_d < 900){E_T <- 5.6 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 5.3}
        else if (v_d > 900 & v_d < 1000){E_T <- 5.3 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 5.0}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else if (L == 6400){
        if (v_d > 0 & v_d < 100){E_T <- 9.1 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 100){E_T <- 9.1}
        else if (v_d > 100 & v_d < 200){E_T <- 9.1 - 0.4 * (v_d - 100) * 0.01}
        else if (v_d == 200){E_T <- 8.7}
        else if (v_d > 200 & v_d < 300){E_T <- 8.7 - 0.3 * (v_d - 200) * 0.01}
        else if (v_d == 300){E_T <- 8.4}
        else if (v_d > 300 & v_d < 400){E_T <- 8.4 - 0.3 * (v_d - 300) * 0.01}
        else if (v_d == 400){E_T <- 8.1}
        else if (v_d > 400 & v_d < 500){E_T <- 8.1 - 0.4 * (v_d - 400) * 0.01}
        else if (v_d == 500){E_T <- 7.7}
        else if (v_d > 500 & v_d < 600){E_T <- 7.7 - 0.3 * (v_d - 500) * 0.01}
        else if (v_d == 600){E_T <- 7.4}
        else if (v_d > 600 & v_d < 700){E_T <- 7.4 - 0.3 * (v_d - 600) * 0.01}
        else if (v_d == 700){E_T <- 7.1}
        else if (v_d > 700 & v_d < 800){E_T <- 7.1 - 0.4 * (v_d - 700) * 0.01}
        else if (v_d == 800){E_T <- 6.7}
        else if (v_d > 800 & v_d < 900){E_T <- 6.7 - 0.3 * (v_d - 800) * 0.01}
        else if (v_d == 900){E_T <- 6.4}
        else if (v_d > 900 & v_d < 1000){E_T <- 6.4 - 0.3 * (v_d - 900) * 0.01}
        else if (v_d >= 1000){E_T <- 6.1}
        else {E_T <- 'Error : [v_d] must be positive(vph). Please check that.'}
      }
      else {E_T <- 'Error : [L] must be one of 400, 800, 1200, 1600, 2400, 3200, 4800, 6400. Please check that.'}
    }
    else {E_T <- 'Error : [slope] must be one of 0.03, 0.04, 0.05, 0.06, 0.07. Please check that.'}
  }
  else {E_T <- 'Error : [landform] must be one of [flatland], [hill], [specific_slope]. Please check that.'}
  E_T
}
