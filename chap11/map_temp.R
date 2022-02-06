spdf_kor_geojson <- geojson_read('C:/R/git/datavisualization/map/TL_SCCO_CTPRVN.json',  what = "sp")

## st_as_sf()를 사용해 sp객체를 sf객체로 변환
sf_spdf_kor <- st_as_sf(spdf_kor_geojson)



sf_spdf_kor |> ggplot() +
  geom_sf()


if(!require(geogrid)) {
  install.packages('geogrid')
  library(geogrid)
}

## calculate_grid()를 사용하여 sf_spdf_seoul_joined를 육각형 그리드형태로 변환
new_cells_hex <- calculate_grid(shape = sf_spdf_kor, grid_type = "hexagonal")

## assign_polygons()을 사용하여 sf_spdf_seoul_joined위에 new_cells_hex을 할당
resulthex <- assign_polygons(sf_spdf_kor, new_cells_hex)


## assign_polygons()의 결과를 ggplot 객체 데이터로 사용
resulthex |>
  ggplot() +
  ## fill을 대학수로 매핑한 geom_sf 레이어 생성
  geom_sf() + 
  ## X축을 V1, Y축을 V2, label을 SIG_KOR_NM을 매핑한 geom_text 레이어 추가
  geom_text(aes(x = V1, y = V2, label = CTP_KOR_NM), size = 2) + 
  ## fill 스케일을 흰색부터 dodgerblue까지 가지는 그래디언트로 설정
  scale_fill_gradient(low = 'white', high = 'dodgerblue')


## calculate_grid()를 사용하여 sf_spdf_seoul_joined를 사각형 그리드형태로 변환
new_cells_reg <- calculate_grid(shape = sf_spdf_kor, grid_type = "regular")

## assign_polygons()을 사용하여 sf_spdf_seoul_joined위에 new_cells_reg을 할당
resultreg <- assign_polygons(sf_spdf_kor, new_cells_reg)  


## assign_polygons()의 결과를 ggplot 객체 데이터로 사용
resultreg |>
  ggplot() +
  ## fill을 대학수로 매핑한 geom_sf 레이어 생성
  geom_sf() + 
  ## X축을 V1, Y축을 V2, label을 SIG_KOR_NM을 매핑한 geom_text 레이어 추가
  geom_text(aes(x = V1, y = V2, label = CTP_KOR_NM), size = 2) + 
  ## fill 스케일을 흰색부터 dodgerblue까지 가지는 그래디언트로 설정
  scale_fill_gradient(low = 'white', high = 'dodgerblue') + 
  theme_void() + 
  labs(title = '서울의 구별 대학수')


gridlines()