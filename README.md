# KHCMinR : 도로용량편람(2013) R 패키지 개발 프로젝트
## *Introduction*

![Fast car-bro-small](https://user-images.githubusercontent.com/75024126/119323940-312e7800-bcba-11eb-9ae4-f31789f1a463.png)


* Ver. : 0.0.0.9000 (개발 중)
* Starts at : 2021.05.22.
* Participant : [Jimin Chae](https://github.com/regenesis90)


도로용량편람(2013) 자동화 패키지

## *Installation*
> Update Soon

## *Functions*
### 기본
* `DHV`
* `DDHV`
* `PDDHV`
* `f_hv`
* `f_w`
* `c_wz`
* `MSFi`
* `SFi`
* `number_of_lanes_required`
* `Vp`
### 고속도로 기본 구간
* `LOS_freeway_basic`
* `capacity`

### 고속도로 엇갈림 구간
* `W_nw`
* `W_w`
* `S_nw`
* `S_w`
* `D_avg_weaving`
* `S_avg_weaving`
* `LOS_freeway_weaving`

### 고속도로 연결로 접속부
* `LOS_freeway_ramp_junction`
* `influence_capacity_ramp_junction`
* `analysis_constraint_freeway_ramp_junction`

### 고속도로 종합 분석

### 다차로도로
* `F_B`
* `F_H`
* `F_A`
* `F_S`
* `F_V`
* `capacity_multilane_road`
* `LOS_multilane_road`
* `B_multilane_road`
* `H_multilane_road`
### 2차로도로
* `TDR`
* `LOS_2lane_road`
* `ESL_2lane_road`
* `PHF_2lane_road`
* `E_T_2lane_Road`
* `f_w_D`
* `f_np`
* `f_pl`
* `TDR_2lane_plus1`
* `ATS_2lane_plus1`
* `TDR_2lane_plus1_TD`
* `ATS_2lane_plus1_TD`
### 신호교차로
* `LOS_signalized_intersection`
* `F_U`
* `F_R`
* `P_signalized_intersection`
* `E_p`
* `E_u`
* `E_1`
* `L_dw`
* `T_b`
* `L_bb`
* `L_p`
* `L_H`
* `f_c`
* `V_RF`
* `V_LF`
* `V_STL`
* `V_STR`
* `P_RT`
* `P_LT`
* `f_LT`
* `f_RT`
* `f_LT_RT`
* `S_bi`
* `f_BLT`
* `S_bi`
* `f_ub`
* `S_i_backtick`
* `CRF`
* `S_i`
* `f_w3`
* `f_g`
* `f_hv3`
* `X_i`
* `X_c`
* `d`
* `type_d3exist`
* `d_1`
* `d_2`
* `d_3`
* `TVO`
* `PF_`
* `d_A`
* `d_g`
* `g_T`
* `avg_speed`
* `f_WZ`
* `S_i_WZ`
* `f_iw3`
### 연결로-일반도로 결합부
* `LOS_diamond_interchange`
* `L_Q`
* `tau_0`
* `tau_1`
* `mu_B`
* `mu_F`
* `g_i_backtick_upstream`
* `g_i_backtick_downstream`
* `c_i_diamond_interchange`
* `X_i_diamond_interchange`
* `d_I`
### 비신호교차로
* `V_ci`
* `t_c_x`
* `t_f_x`
* `c_p_x`
* `p_i_nonsignalize_intersection`
* `V_i_per_c_pi`
* `c_SH`
* `d_nonsignaled_intersection`
* `LOS_two_way_stop`
* `LOS_uncontrolled_intersection`
### 회전교차로
* `LOS_roundabout`
* `V_c_NB`
* `c_roundabout`
* `f_p`
* `Vp_i_roundabout`
* `E_T_roundabout`
* `f_hv_roundabout`
* `V_i_pce`
* `V_c_i_roundabout`
### 도시 및 교외 간선도로
* `LOS_arterial`
* `free_speed_arterial`
* `roadside_friction_arterial`
* `t_p_km_arterial`
* `avg_speed_arterial`
* `f_cw`
* `capacity_arterial_road`
* `PF_sensitive`
* `t_p_km_arterial_central_bus_lane`
* `avg_speed_segment`
* `T_bus`
* `T_others`
* `avg_speed_total`
### 대중교통
### 보행자 시설
### 자전거도로

## *Contact*
* regenesis90@gmail.com
