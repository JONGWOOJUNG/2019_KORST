print_passage_summary(main_df) # 통행 요약
print_sum_top_zones(main_df, 5) # 최상위 이용량 대여소

###주중/주말에 따라 그려봐도 좋을듯
zone_heatmap(zones_df, k_size = 9) # 열지도 
total_use_map(main_df, circle_size = 1) # 총통행량 지도
total_use_map(weekday_df, circle_size = 1) # 총통행량 지도
total_use_map(weekend_df, circle_size = 1) # 총통행량 지도
total_use_map(main_df, circle_size = 1, "black") %>% 
  addHeatmap(data = zones_df, lat = ~Y, lng = ~X, radius = 9) #열지도+총통행지도도


plot_timecode_use(main_df) # 총대여건 시간대별 그래프
print_top_course(main_df, 20) # 이동경로 상위 20개


plot_timecode_use(subset_dep(main_df, '183')) # 183번 대여소 시간대별 그래프
plot_timecode_use(subset_dep(main_df, '207')) # 207번 대여소

print_max_nobike_times(rest_bikes, 5) # 7일간 가장 오래 빈 대여소

plot_1ndist_subway(184, 684) # 지하철역과의 거리가 상, 하위 4분위인 대여소의 그래프 겹쳐 보기

summary(print_lm_subway(weekday_df)) # 지하철 역과의 거리 회귀


#################################################################3

head(use_course(main_df))

