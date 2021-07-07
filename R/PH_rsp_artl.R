#' Interlocking Coefficient of response Signal in Arterial Road
#'
#' Interlocking coefficient of response signal in arterial road.
#'     In the case of response signals, it is described using the US HCM method.
#'     It follows <Table 12-9> in KHCM(2013) p.541-542.
#' @param sg_type Signal Type. Choose one from: \code{'response'}, \code{'semi-response'}
#' @param road \code{'main'}, \code{'sub'}
#' @param turn \code{'straight'}, \code{'right'}, \code{'simultaneous_left'}, \code{'dedicated_left'}
#' @param arv_form Arrival form. Choose one from: \code{'form1'}, \code{'form2'}, \code{'form3'}, \code{'form4'}, \code{'form5'}
#' @param v_c_ratio Green time / Signal Cycle ratio
#' @seealso
#' @details
#'     The arrival form is based on the following:
#'     - form1 : When a densely packed vehicle group arrives at the intersection when the red signal starts. Worst Vehicle Families
#'     - form2: When a dense vehicle group arrives in the middle of a red signal,
#'     a dispersed vehicle group arrives across the red signal.
#'     Better condition than form1, but still in bad condition
#'     - form3 : Overall random arrival status.
#'     In case the interlocking effect disappears because the distance between the interlocking intersections is too far,
#'     or the red and green signals arrive scattered throughout. average condition
#'     - form4: When a dense vehicle group arrives in the middle of the green signal,
#'     or when a group of vehicles dispersed throughout the green signal arrives.
#'     Overall good vehicle condition
#'     - form5: When a dense vehicle group arrives when the green signal starts.
#'     Best vehicle condition
#' @export PF_rsp_artl
#' @examples
#' PF_rsp_artl(sg_type = 'response', road = 'main', turn = 'right', v_c_ratio = 0.5, arv_form = 'form3')
PF_rsp_artl <- function(sg_type = NULL, road = NULL, turn = NULL, arv_form = NULL, v_c_ratio = NULL){
  if (sg_type == 'response'){
    if (turn == 'straight'){
      if (v_c_ratio <= 0.6){
        if (arv_form == 'form1'){f <- 1.54}
        else if (arv_form == 'form2'){f <- 1.08}
        else if (arv_form == 'form3'){f <- 0.85}
        else if (arv_form == 'form4'){f <- 0.62}
        else if (arv_form == 'form5'){f <- 0.40}
        else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
      }
      else {f <- 'Error : [v_c_ratio] must be <= 0.6. Please check that.'}
    }
    else if (turn == 'right'){
      if (v_c_ratio <= 0.8){
        if (arv_form == 'form1'){f <- 1.25}
        else if (arv_form == 'form2'){f <- 0.98}
        else if (arv_form == 'form3'){f <- 0.85}
        else if (arv_form == 'form4'){f <- 0.71}
        else if (arv_form == 'form5'){f <- 0.50}
        else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
      }
      else {f <- 'Error : [v_c_ratio] must be <= 0.8. Please check that.'}
    }
    else if (turn == 'simultaneous_left'){
      if (v_c_ratio <= 1.0){
        if (arv_form == 'form1'){f <- 1.16}
        else if (arv_form == 'form2'){f <- 0.94}
        else if (arv_form == 'form3'){f <- 0.85}
        else if (arv_form == 'form4'){f <- 0.78}
        else if (arv_form == 'form5'){f <- 0.61}
        else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
      }
      else {f <- 'Error : [v_c_ratio] must be <= 1.0. Please check that.'}
    }
    else if (turn == 'dedicated_left'){f <- 1.0}
    else {f <- 'Error : [turn] must be one of [straight], [right], [simultaneous_left], [dedicated_left]. Please check that.'}
    }
  else if (sg_type == 'semi-response'){
    if (road == 'main'){
      if (turn == 'straight'){
        if (v_c_ratio <= 0.6){
          if (arv_form == 'form1'){f <- 1.85}
          else if (arv_form == 'form2'){f <- 1.35}
          else if (arv_form == 'form3'){f <- 1.00}
          else if (arv_form == 'form4'){f <- 0.72}
          else if (arv_form == 'form5'){f <- 0.42}
          else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
        }
        else {f <- 'Error : [v_c_ratio] must be <= 0.6. Please check that.'}
      }
      else if (turn == 'right'){
        if (v_c_ratio <= 0.8){
          if (arv_form == 'form1'){f <- 1.50}
          else if (arv_form == 'form2'){f <- 1.22}
          else if (arv_form == 'form3'){f <- 1.00}
          else if (arv_form == 'form4'){f <- 0.82}
          else if (arv_form == 'form5'){f <- 0.53}
          else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
        }
        else {f <- 'Error : [v_c_ratio] must be <= 0.8. Please check that.'}
      }
      else if (turn == 'simultaneous_left'){
        if (v_c_ratio <= 1.0){
          if (arv_form == 'form1'){f <- 1.40}
          else if (arv_form == 'form2'){f <- 1.18}
          else if (arv_form == 'form3'){f <- 1.00}
          else if (arv_form == 'form4'){f <- 0.90}
          else if (arv_form == 'form5'){f <- 0.65}
          else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
        }
        else {f <- 'Error : [v_c_ratio] must be <= 1.0. Please check that.'}
      }
      else if (turn == 'dedicated_left'){f <- 1.0}
      else {f <- 'Error : [turn] must be one of [straight], [right], [simultaneous_left], [dedicated_left]. Please check that.'}
    }
    if (road == 'sub'){
      if (turn == 'straight'){
        if (v_c_ratio <= 0.6){
          if (arv_form == 'form1'){f <- 1.48}
          else if (arv_form == 'form2'){f <- 1.18}
          else if (arv_form == 'form3'){f <- 1.00}
          else if (arv_form == 'form4'){f <- 0.86}
          else if (arv_form == 'form5'){f <- 0.70}
          else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
        }
        else {f <- 'Error : [v_c_ratio] must be <= 0.6. Please check that.'}
      }
      else if (turn == 'right'){
        if (v_c_ratio <= 0.8){
          if (arv_form == 1){f <- 1.20}
          else if (arv_form == 2){f <- 1.07}
          else if (arv_form == 3){f <- 1.00}
          else if (arv_form == 4){f <- 0.98}
          else if (arv_form == 5){f <- 0.89}
          else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
        }
        else {f <- 'Error : [v_c_ratio] must be <= 0.8. Please check that.'}
      }
      else if (turn == 'simultaneous_left'){
        if (v_c_ratio <= 1.0){
          if (arv_form == 'form1'){f <- 1.12}
          else if (arv_form == 'form2'){f <- 1.04}
          else if (arv_form == 'form3'){f <- 1.00}
          else if (arv_form == 'form4'){f <- 1.00}
          else if (arv_form == 'form5'){f <- 1.00}
          else {f <- 'Error : [arv_form] must be one of [form1], [form2], [form3], [form4], [form5]. Please check that.'}
        }
        else {f <- 'Error : [v_c_ratio] must be <= 1.0. Please check that.'}
      }
      else if (turn == 'dedicated_left'){f <- 1.0}
      else {f <- 'Error : [turn] must be one of [straight], [right], [simultaneous_left], [dedicated_left]. Please check that.'}
    }
  }
  else {f <- 'Error : [type] must be one of [response], [semi-response]. Please check that.'}
  f
}
