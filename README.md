# KHCMinR : 도로용량편람(2013) 자동화 R 패키지
## *Introduction*

* Ver. : 0.0.0.9000 (개발 중)
* Starts at : 2021.05.22.
* Participant : [Jimin Chae](https://github.com/regenesis90)


도로용량편람(2013) 자동화 패키지

## *Installation*
> Update Soon

## *Functions*
### 1. 개론
* `K` : 설계시간계수(Design Hourly Factor)
* `DHV`: 설계시간교통량(Design Hourly Volume)
* `DDHV` : 중방향 설계시간교통량(Directional Design Hourly Volume)
### 2. 고속도로 기본 구간(Basic Section of Expressway)
* `AADT` : 연평균 일교통량(Average Annual Daily Traffic Volume)
* `PDDHV` : 첨두 설계시간 교통량(Peak Hour Directional Design Volume)
* `f_hv_expwy_basic` : 고속도로 기본구간에서의 중차량계수(Heavy Vehicle Factor)
* `E_hv_expwy_basic` : 고속도로 기본구간에서 특정 경사 구간의 승용차 환산계수
* `f_w_expwy_basic` : 고속도로 기본구간에서 차로폭 및 측방여유폭에 대한 보정계수
* `f_iw_expwy_basic` : 고속도로 기본구간에서 날씨 보정계수
* `f_dk` : 주야간 보정계수
* `D_coe` : 중방향계수(Directional Factor)
* `capa_expwy_basic_wz_jw` : j 설계속도의 공사구간 기본 용량(pcphpl)
* `capa_expwy_basic_j` : j 설계속도인 고속도로 기본구간의 용량(pcphpl)
* `capa_expwy_basic_wz` : 공사구간 용량(vph, 공사 시 편도 차로 수, 차로폭 및 측방여유폭, 중차량 고려)
* `capa_expwy_basic` : 특수상황(공사구간, 날씨, 주야간)이 반영된 j설계속도인 고속도로 기본구간의 용량(vph)
* `MSF_i` : 최대 서비스 교통류율(Maximum Service Flow Rate)
* `SF_i` : 서비스 교통류율(Service Flow Rate)
* `N_required` : 결정된 수요 차로수(Number of lanes required)
* `V_P` : 첨두시간 환산 교통량(Converted Peak Hour Volume) 
* `PHF_expwy_basic` : 고속도로 기본구간에서의 첨두시간계수(Peak Hour Factor)
* `LOS_expwy_basic` : 고속도로 기본 구간의 서비스 수준(Level of Service in Expressway Basic Section)
* `v_c_ratio` : 교통량 대 용량비(V/C Ratio)
* `ANLYS_expwy_basic_LOS` : 고속도로 기본구간의 운영상태(서비스수준) 분석
* `ANLYS_expwy_basic_when_expand` :  현재와 3년 이후의 기본구간 운영 상태 분석 및 확장 시기 결정(*** 미완성, 진행필요)

### 3. 고속도로 엇갈림 구간
* `VR_expwy_wv` : 엇갈림 교통량비
* `W_expwy_wv_nw` : 비엇갈림 교통류에 따른 엇갈림 강도 계수
* `W_expwy_wv_w` : 엇갈림 교통류에 따른 엇갈림 강도 계수
* `S_expwy_wv_nw` : 엇갈림 구간에서 비엇갈림 교통류 평균속도(kph)
* `S_expwy_wv_w` : 엇갈림 구간에서 엇갈림 교통류 평균속도(kph)
* `S_expwy_wv` : 엇갈림 구간 내의 평균 속도(kph)
* `D_expwy_wv` : 엇갈림 구간의 평균 밀도(pcpkmpl)
* `LOS_expwy_wv_ramp` : 본선-연결로 엇갈림 구간의 서비스수준
* `LOS_exwpy_wv_fr` : 연결로-연결로 엇갈림 구간의 서비스수준
* `V_P_expwy_wv` : 첨두시간 승용차 교통량으로 환산된 엇갈림구간 교통량(pcph)
* `appl_expwy_wv` : 엇갈림 구간 속도 산출 시 적용가능성 판정 

### 4. 고속도로 연결로 접속부
* `appl_expwy_rpjt` : 고속도로 연결로 접속부 서비스수준 분석 절차 진행여부 판정
* `capa_expwy_rpjt` : 고속도로 연결로 접속부 용량(pcph)
* `capa_expwy_rpjt_rp` : 고속도로 연결로 용량(pcph)
* `LOS_expwy_rpjt` : 고속도로 연결로 접속부의 서비스수준
* `V_P_expwy_rpjt` : 고속도로 연결로 접속부 승용차 환산 첨두시간 교통량(pcph)
* `P_FM_expwy_rpjt` : 합류부 영향권 비
* `P_FD_expwy_rpjt` : 분류부 영향권 비
* `V_12_expwy_rpjt` : 고속도로 합류부 또는 분류부의 1~2차선 교통량(pcph)
* `D_MR_expwy_rpjt` : 고속도로 연결로 접속부 합류부의 영향권 밀도(pcpkmpl) 
* `D_DR_expwy_rpjt` : 고속도로 연결로 접속부, 분류부의 영향권 밀도(pcpkmpl) 

### 5. 고속도로 종합 분석

### 6. 다차로도로
* `F_B`
* `F_H`
* `F_A`
* `F_S`
* `F_V`
* `capacity_multilane_road`
* `LOS_multilane_road`
* `B_multilane_road`
* `H_multilane_road`
### 7. 2차로도로
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
### 8. 신호교차로
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
### 9. 연결로-일반도로 결합부
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
### 10. 비신호교차로
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
### 11. 회전교차로
* `LOS_roundabout`
* `V_c_NB`
* `c_roundabout`
* `f_p`
* `Vp_i_roundabout`
* `E_T_roundabout`
* `f_hv_roundabout`
* `V_i_pce`
* `V_c_i_roundabout`
### 12. 도시 및 교외 간선도로
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
### 13. 대중교통
* `LOS_bus_seat`
* `LOS_bus_standing`
* `LOS_bus_interval`
* `LOS_bus_time`
* `t_D`
* `n_bus_stopping_area`
* `c_b`
* `r_coefficient`
* `N_bus_using_efficiency`
* `c_p_b`
### 14. 보행자 시설
* `V_pedestrian`
* `LOS_pedestrian`
* `LOS_pedestrian_stair`
* `LOS_pedestrian_waiting_space`
* `LOS_pedestrian_signal_crosswalk`
* `W_O`
* `W_E`
* `V_pedestrian_traffic_flow`
* `d_P`
* `t_ped_cross`
* `TS`
* `M_ped_cross`
### 15. 자전거도로
* `F_pass`
* `F_meet`
* `F_total`
* `F_pass_b_p `
* `F_meet_b_p`
* `F_meet_p_b`
* `F_total_b`
* `F_total_p`
* `d_bike`
* `c_bike`
* `f_w_bike`
* `S_bike`
* `avg_speed_bike`
* `F_bike`
* `LOS_bike_signal_intersection`
* `LOS_bike_city_street`
* `LOS_bike_default`

## *Datasets*

## *Contact*
* regenesis90@gmail.com
