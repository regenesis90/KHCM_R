#' Analyze the Operating Status of the Current and Future Expressway Basic Sections and Determine When to Expand
#'
#' It analyze the operating status(Level of service, LOS) of the basic section of expressway.
#' And determine when to expand expressway.
#'     - It follows the example in KHCM(2013), p.45-46
#' @param obs Obstacle. Choose one from :\code{'one_side'}, \code{'both_sides'}
#' @param side_clearance The lateral clearance of lane. From the center line of the center line to the pavement edge of the lane. The Average of left lateral clearance and right lateral clearance.
#' @param lane The number of expressway lane(Round-trip). It must be 4 or more.
#' @param N The number of freeway lane(one-way lane). It means (# of normal one-way lane) - (# of lanes closed due to construction)
#' @param lane_width The width of each lane(m). It must be equal to or more than 2.75
#' @param landform Choose one from : \code{'flatland'}, \code{'hill'}, \code{'mountain'}, \code{'specific_slope}
#' @param P_T1 The ratio of small heavy vehicle composition(Trucks less than 2.5 tons, buses less than 16 passengers)
#' @param P_T2 The ratio of middle heavy vehicle composition(Trucks of 2.5 tons or more, buses with 16 passengers or more)
#' @param P_T3 The ratio of large heavy vehicle composition(Semi trailer or full trailer)
#' @param P_hv The ratio of total heavy vehicles.
#' @param slope Slope gradient(%)
#' @param slope_length The length of the slope(km)
#' @param v Current traffic volume(vph). See \code{\link{DDHV}}.
#' @param v_future Future traffic volume(vph).
#' @param PHF Peak Hour Factor(PHF). See \code{\link{PHF_expwy_basic}}.
#' @param design_speed Design speed. Choose one from : \code{120}, \code{100}, \code{80}
#' @param weather Weather condition. Choose one from : \code{'sunny'}, \code{'rainy'}, \code{'snowy'}
#' @param precip Rainfall or snowfall or hail. Precipitation(mm/h).
#' @param day_night Distinguish between day and night. Choose one from : \code{'day'}, \code{'night'}
#' @param service_limit Low limit of service level(LOS). Choose one from: \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}
#' @keywords analysis operating status LOS service level
#' @seealso \code{\link{f_hv_expwy_basic}}, \code{\link{f_w_expwy_basic}}, \code{\link{f_iw_expwy_basic}}, \code{\link{f_dk}}, \code{\link{capa_expwy_basic_j}}, \code{\link{capa_expwy_basic}}, \code{\link{V_P_expwy_basic}}, \code{\link{LOS_expwy_basic}}.
#' @export ANLYS_expwy_basic_when_expand
#' @examples
#' ANLYS_expwy_basic_when_expand(obs = 'one_side',
#'                               side_clearance = 1.75,
#'                               lane = 4,
#'                               N = 2,
#'                               lane_width = 3.5,
#'                               landform = 'hill',
#'                               P_T1 = 0, P_T2 = 0.2, P_T3 = 0,
#'                               v = 2000, PHF = 0.95,
#'                               design_speed = 100,
#'                               weather = 'sunny',
#'                               precip = 0,
#'                               day_night = 'day')
ANLYS_expwy_basic_when_expand <- function(obs = NULL, side_clearance = NULL, lane = NULL, N = NULL, lane_width = NULL,
                                          landform = NULL, P_T1 = NULL, P_T2 = NULL, P_T3 = NULL, P_hv = NULL, slope = NULL, slope_length = NULL,
                                          v = NULL, v_future = NULL, PHF = NULL, design_speed = NULL, weather = NULL, precip = NULL, day_night = NULL,
                                          service_limit = NULL){
  f_hv <- f_hv_expwy_basic(landform = landform, P_T1 = P_T1, P_T2 = P_T2, P_T3 = P_T3, P_hv = P_hv, slope = slope, slope_length = slope_length)
  f_w <- f_w_expwy_basic(obs = obs, side_clearance = side_clearance, lane = lane, lane_width = lane_width)
  f_iw <- f_iw_expwy_basic(design_speed = design_speed, weather = weather, precip = precip)
  f_dk <- f_dk(design_speed = design_speed, day_night = day_night)
  c_j <- capa_expwy_basic_j(design_speed = design_speed)
  capacity <- capa_expwy_basic(c_j = c_j, N = N, f_w = f_w, f_hv = f_hv, f_iw = f_iw, f_dk = f_dk)
  V_P <- V_P_expwy_basic(v = v, PHF = PHF)
  V_P_future <- V_P_expwy_basic(v = v_future, PHF = PHF)
  v_c_ratio <- V_P/capacity
  v_c_ratio_future <- V_P_future/capacity
  LOS <- LOS_expwy_basic(design_speed = design_speed, v_c_ratio = v_c_ratio)
  LOS_future <- LOS_expwy_basic(design_speed = design_speed, v_c_ratio = v_c_ratio_future)

  if (service_limit == 'A'){
    msf <- MSF_i(c_j = c_j, v_c_ratio_i = NULL)
    sf <- (MSF_i = NULL, N = NULL, f_w = NULL, f_hv = NULL)}
  else if (service_limit == 'B'){}
  else if (service_limit == 'C'){}
  else if (service_limit == 'D'){}
  else if (service_limit == 'E'){}
  else {sf <- 'Error : [service_limit] must be one of [A], [B], [C], [D], [E]. Please check that.'}
}
