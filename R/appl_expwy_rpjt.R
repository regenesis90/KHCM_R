#' Determination of whether to analyze LOS of the Ramp-Expressway Junction
#'
#' This function Determine whether to proceed with the service level analysis procedure for the connection road connection
#'     based on the capacity of the connection road connection part
#'     It follows <Table 4-1>, <Table 4-2> in KHCM(2013), p.84, 85.
#' @param free_speed Free speed of main lane(kph).
#' @param free_speed_ramp Free speed of ramp(link) road(kph).
#' @param N Total number of main lanes(one-way). It must be 2 or more.
#' @param N_ramp Number of ramp road. Choose one from : \code{1}, \code{2}
#' @param V_main Traffic volume of main road(pcph).
#' @param V_inflow Inflow traffic volume of influenced area(pcph).
#' @param V_outflow Outflow traffic volume of influenced area(pcph).
#' @param V_ramp Traffic volume of ramp road(pcph).
#' @keywords expressway ramp junction analyze determination
#' @export appl_expwy_rpjt \code{'Possible'}, \code{'Impossible'}
#' @examples
appl_expwy_rpjt(free_speed = 83, free_speed_ramp = 63, N = 4, N_ramp = 1, V_main = 3892, V_inflow = 4221, V_outflow = 1738, V_ramp = 1822)
appl_expwy_rpjt <- function(free_speed = NULL, free_speed_ramp = NULL, N = NULL, N_ramp = NULL, V_main = NULL, V_inflow = NULL, V_outflow = NULL, V_ramp = NULL){
  if (free_speed >= 0){
    if (free_speed_ramp >= 0){
      if (N >= 1){
        if (N_ramp == 1 | N_ramp == 2){
          if (V_main >= 0){
            if (V_inflow >= 0){
              if (V_outflow >= 0){
                if (V_ramp >= 0){
                  capa_main <- capa_expwy_rpjt(free_speed = free_speed, N = N, output = 'main')
                  capa_inflow <- capa_expwy_rpjt(free_speed = free_speed, N = N, output = 'influence_inflow')
                  capa_outflow <- capa_expwy_rpjt(free_speed = free_speed, N = N, output = 'influence_outflow')
                  capa_ramp <- capa_expwy_rpjt_rp(free_speed_ramp = free_speed_ramp, N_ramp = N_ramp)
                  if (V_main <= capa_main){
                    if (V_inflow <= capa_inflow){
                      if (V_outflow <= capa_outflow){
                        if (V_ramp <= capa_ramp){'Possible'}
                        else {'Impossible'}
                      }
                      else {'Impossible'}
                    }
                    else {'Impossible'}
                  }
                  else {'Impossible'}
                }
                else {'Error : [V_ramp] must be positive(kph). Please check that.'}
              }
              else {'Error : [V_outflow] must be positive(kph). Please check that.'}
            }
            else {'Error : [V_inflow] must be positive(kph). Please check that.'}
          }
          else {'Error : [V_main] must be positive(kph). Please check that.'}
        }
        else {'Error : [N_ramp] must be 1 or 2. Please check that.'}
      }
      else {'Error : [N] must be >= 1. Please check that.'}
    }
    else {'Error : [free_speed_ramp] must be positive(kph). Please check that.'}
  }
  else {'Error : [free_speed] must be positive(kph). Please check that.'}
}
