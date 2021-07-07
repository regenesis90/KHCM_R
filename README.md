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
### 2. 고속도로 기본 구간(expwy_basic)
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
* `V_P_expwy_basic` : 첨두시간 환산 교통량(Converted Peak Hour Volume) 
* `PHF_expwy_basic` : 고속도로 기본구간에서의 첨두시간계수(Peak Hour Factor)
* `LOS_expwy_basic` : 고속도로 기본 구간의 서비스 수준(Level of Service in Expressway Basic Section)
* `v_c_ratio` : 교통량 대 용량비(V/C Ratio)
* `ANLYS_expwy_basic_LOS` : 고속도로 기본구간의 운영상태(서비스수준) 분석
* `ANLYS_expwy_basic_when_expand` :  현재와 3년 이후의 기본구간 운영 상태 분석 및 확장 시기 결정(*** 미완성, 진행필요)

### 3. 고속도로 엇갈림 구간(expwy_wv)
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

### 4. 고속도로 연결로 접속부(expwy_rpjt)
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

### 6. 다차로도로(ml)
* `type_ml` : 다차로도로 유형 구분
* `F_wc_ml` : 차로폭 및 측방여유폭 속도 보정계수(kph)
* `F_B_ml` : 평면선형 속도 보정계수(kph)
* `F_H_ml` : 종단선형 속도 보정계수(kph)
* `F_A_ml` : 유출입 지점수 속도 보정계수(kph)
* `F_S_ml` : 신호등의 속도 보정계수(구간평균 교통량 500vphpl 이하시, kph)
* `F_V_ml` : 교통량에 따른 속도 보정계수(구간평균 교통량 500vphpl 초과시, kph)
* `capa_ml` : 다차로도로에서의 용량(pcph)
* `LOS_ml` : 다차로도로의 서비스수준
* `B_ml` : 평면선형 굴곡도
* `H_ml` : 종단선형 경사도
* `S_i_ml` : 전체 구간에 대한 서비스 수준 i의 경계값(평균통행속도, kph)
* `S_t_ml` : 전체 구간, 전체 차량에 대한 평균 통행속도(kph)`
* `BS_P_ml` : 기본 조건의 최대 통행속도(kph)
* `S_P1_ml` : 다차로도로에서 승용차가 낼 수 있는 최대 통행속도 평균(kph)
* `S_P2_ml` : 다차로도로에서 승용차의 평균 통행속도(kph)
* `S_T2_ml` : 다차로도로에서 중차량의 최대 통행속도(kph)
* `S_TP_ml` : 전체 차량(차종)에 대한 평균 통행속도(kph)
### 7. 2차로도로(2l, 2lp1)
* `type_2l` : 2차로도로의 유형 구분
* `TDR_thr_2l` : 2차로도로의 총지체율(%) 추정
* `LOS_2l` : 2차로도로의 서비스 수준
* `capa_2l` : 기본 조건에서 2차로도로의 용량
* `appl_2l` : 2차로도로 분석절차 진행여부 결정
* `ESL_2l` : 2차로도로의 신호교차로 상류부 영향권 길이(m)
* `V_P_2l` : 2차로도로에서의 교통류율(승용차/시)
* `PHF_2l` : 2차로도로의 일반적인 첨두시간계수
* `E_T_ATS_2l` : 통행속도 중차량 보정계수
* `E_T_D_2l` : 총지체율 중차량 보정계수
* `d_2l` : 2차로도로에서 차량당 평균제어지체(초/대)
* `ATS_1_i_2l` : 구간형태 1의 i구간 통행속도(km/h)
* `ATS_2_i_2l` : 구간형태 2의 i구간 통행속도(km/h)
* `TDR_1_i_2l` : 구간형태 1의 i구간 총지체율(%)
* `TDR_2_i_2l` : 구간형태 2의 i구간 총지체율(%) 
* `ATS_total_2l` : 2차로도로 전체구간의 통행속도(km/h)
* `TDR_total_2l` : 2차로도로 전체구간의 총지체율(%)
* `f_w_D_2l` : 차로폭 및 측방여유폭 보정계수(총지체율, %)
* `f_w_ATS_2l` : 차로폭 및 측방여유폭 보정계수(통행속도, km/h)
* `f_np_D_2l` : 방향별 분포 및 앞지르기 금지구간 비율에 따른 보정계수(총지체율, %)
* `f_np_ATS_2l` : 방향별 분포 및 앞지르기 금지구간 비율에 따른 보정계수(통행속도, km/h)
* `f_pl_2lp1` : 2+1차로도로 구간의 총지체율 보정계수
* `TDR_21_i_2lp1` : 2+1차로도로 일방향 i개 구간의 총지체율(%)
* `TDR_TD_2lp1` : 2+1차로도로의 양방향 총지체율(%)
* `ATS_21_i_2lp1` : 2+1차로도로 일방향 i개 구간의 통행속도(kph)
* `ATS_TD_2lp1` : 2+1차로도로 구간의 양방향 통행속도(kph)
* `f_s_2lp1` : 2+1차로도로 구간의 통행속도 보정계수
### 8. 신호교차로(si)
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
### 9. 연결로-일반도로 결합부(di)
* `LOS_di`
* `L_Q_di`
* `tau_0_di`
* `tau_1_di`
* `mu_B_di`
* `mu_F_di`
* `g_i_up_di`
* `g_i_dn_di`
* `c_i_di`
* `X_i_di`
* `d_I_di`
### 10. 비신호교차로(nsi)
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
### 11. 회전교차로(rab)
* `LOS_rab`
* `V_c_NB`
* `c_rab`
* `f_p`
* `V_P_i_rab`
* `E_T_rab`
* `f_hv_rab`
* `V_i_pce_rab`
* `V_c_i_rab`
### 12. 도시 및 교외 간선도로(artl)
* `type_artl` : 도로구분과 도로여건에 따른 간선도로 유형
* `LOS_artl` : 간선도로의 평균 통행속도(kph)별 서비스수준
* `ATS_artl` : 간선도로의 전체 또는 일부 구간의 평균통행속도(kph)
* `friction_artl` : 간선도로에서 노변마찰의 정도 설정 기준
* `t_trv_km_artl` : 간선도로의 전체 또는 일부 구간의 평균통행속도(kph)
* `d_artl` : 간선도로에서 차량당 평균제어지체(초/대)
* `d_1_artl` : 간선도로에서 연동보정된 균일제어지체(초/대)
* `d_2_artl` : 간선도로에서 임의 도착과 과포화를 나타내는 증분지체
* `d_3_artl` : 간선도로에서의 추가지체(초/대) 
* `f_cw_artl` : 간선도로에서 신호교차로 간 보행자 횡단신호 보정계수
* `V_P_artl` : 간선도로에서 첨두시간으로 보정된 첨두시간 교통류율(vph)
* `capa_plan_artl` : 간선도로에서 개략적인 값이 필요하거나, 간선도로 계획 시에 한정하여 구하는 용량 값(vph)
* `TVO_artl` : 간선도로에서의 옵셋 편의율
* `PF_fix_artl` : 간선도로에서의 고정신호 연동계수
* `PF_rsp_artl` : 간선도로에서 감응신호의 연동계수
* `t_p_km_arterial_central_bus_lane`
* `t_trv_km_bus_artl` : 중앙버스전용차로가 설치된 간선도로에서, 중앙전용차로를 주행하는 버스의 km당 구간 순행시간(초/km)
* `ATS_seg_bus_artl` : 중앙버스전용차로가 설치된 간선도로의 개별 분석구간별 평균통행속도(kph)
* `T_trv_artl` : 중앙버스전용차로 또는 일반차로 분석구간의 통행시간(초)
* `ATS_total_artl` : 중앙버스전용차로가 설치된 간선도로 분석구간 전체의 평균통행속도(kph)
* `LOS_bus_artl` : 중앙버스전용차로가 설치된 간선도로의 서비스수준
### 13. 대중교통(pt)
* `LOS_bus_pt` : 좌석형 또는 입석형 버스의 차내 서비스 수준
* `LOS_sch_pt` : 버스 운행간격에 따른 지역별 서비스 수준
* `LOS_svc_pt` : 버스 운영시간에 따른 서비스 수준
* `t_D_pt` : 버스의 정차 시간(초)
* `t_c_pt` : 버스의 총 소거 시간(초)
* `t_ocp_pt` : 버스의 총 점유시간(초)
* `R_pt` : 버스정류장 버스대기비율에 따른 정차면 용량 산출 보정계수
* `ue_bs_pt` : 대한민국에서 버스정류장의 경험적 정차대수에 따른 이용효율계수
* `capa_bs_bus_pt` : 연속류상 혹은 단속류상 버스정류장의 시간당 차량용량(대/시)
* `capa_bs_png_pt` : 연속류상 혹은 단속류상 버스정류장의 시간당 사람용량(인/시)
### 14. 보행자 시설(ped)
* `V_ped` : 보행자시설에서의 보행교통류율(인/분/m)
* `LOS_road_ped` : 보행자도로에서 보행자 서비스 수준
* `LOS_stair_ped` : 계단에서의 보행자 서비스 수준
* `LOS_wait_ped` : 대기공간에서의 보행자 서비스 수준
* `LOS_cross_ped` : 신호횡단보도에서 보행자 서비스 수준
* `W_O_ped` : 보행자도로에서 시설에 의해 방해를 받는 보도 폭(m)
* `W_E_ped` : 보행자도로에서의 유효도로폭(m)
* `V_P_ped` : 조사된 첨두 15분 보행교통량을 보행교통류율로 환산한 것. 첨두 환산 보행교통류율(인/분/m)
* `d_p_ped` : 신호횡단보도에서 평균 보행자지체(초)
* `t_ped` : 신호횡단보도에서 보행자의 총 횡단시간(초)
* `TS_cross_ped` : 신호횡단보도에서 보행자의 시-공간 면적(m^2-인)
* `T_cross_ped` : 신호횡단보도에서 보행자의 총 횡단보도 점유시간(인-초)
* `M_cross_ped` : 신호횡단보도에서 보행자당 횡단보도 점유공간(m^2/인)
* `W_E_plan_ped` : 보행자도로 계획 및 설계를 위한 유효도로폭(m) 
### 15. 자전거도로(bk)
* `F_pass_type1_bk` : 자전거 전용도로(type1)에서 같은 방향으로 이동하는 자전거의 앞지르기 횟수(times/h)
* `F_meet_type1_bk` : 양방향 자전거 전용도로(type1)에서 주행방향과 대향하여 주행하는 자전거 교통량에 의한 단위시간당 대면횟수(times/h)
* `F_total_type1_bk` : 자전거 전용도로(type1)에서의 전체 상충횟수(times/h)
* `F_pass_type2_bb_bk` : 자전거-보행자 겸용도로(type2)에서 자전거끼리의 앞지르기 횟수(times/h)
* `F_pass_type2_bp_bk` : 자전거-보행자 겸용도로(type2)에서 자전거가 동일방향으로 보행하는 보행자를 앞지르기하는 횟수(times/h)
* `F_pass_type2_pb_bk` : 자전거-보행자 겸용도로(type2)에서 보행자가 동일방향으로 주행하는 자전거에 앞지르기 당하는 횟수(times/h)
* `F_meet_type2_bb_bk` : 자전거-보행자 겸용도로(type2)에서 자전거와 자전거 진행방향과 대향방향으로 진행하는 자전거와의 대면횟수(times/h)
* `F_meet_type2_bp_bk` : 자전거-보행자 겸용도로(type2)에서 자전거와 자전거 진행방향과 대향방향으로 진행하는 보행자와의 대면횟수(times/h)
* `F_meet_type2_pb_bk` : 자전거-보행자 겸용도로(type2)에서 자전거와 자전거 진행방향과 대향방향으로 진행하는 보행자와의 대면횟수를 보행자의 입장에서 본 것(times/h)
* `F_total_type2_b_bk` : 자전거-보행자 겸용도로(type2)에서 자전거 시점의 전체 상충횟수(times/h)
* `F_total_type2_p_bk` : 자전거-보행자 겸용도로(type2)에서 보행자 시점의 전체 상충횟수(times/h)
* `F_pass_type3_bk` : 노상 자전거 기본구간(type3)에서 같은 방향으로 이동하는 자전거의 앞지르기 횟수(times/h)
* `d_type4_bk` : 신호교차로(노상 자전거도로가 신호교차로에 의해 제어되는 곳, type4)의 자전거도로에서의 제어지체
* `f_w_bk` : 자전거도로의 폭에 따른 포화교통류율 보정계수
* `S_bk` : 자전거도로 포화교통류율(vph)
* `capa_bk` : 자전거도로의 용량(vph)
* `ATS_type5_bk` : 도시가로상의 자전거도로(간선도로상에 설치된 노상자전거도로, type5)의 평균통행속도
* `LOS_type1_bk` : 자전거 전용도로(일방통행 또는 양방통행, type1)의  서비스 수준
* `LOS_type2_bk` : 자전거-보행자 겸용도로(type2)의 서비스 수준
* `LOS_type3_bk` : 노상 자전거 도로기본구간(type3)의 서비스 수준
* `LOS_type4_bk` : 노상 신호교차로(type4)에서 자전거 이용자의 서비스 수준
* `LOS_type5_bk` : 도시가로상의 자전거 도로(type5)의 자전거 이용자의 서비스 수준

## *Datasets*

## *Contact*
* regenesis90@gmail.com
