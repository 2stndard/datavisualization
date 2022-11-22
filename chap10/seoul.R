library(readxl)

df_행정구역 <- read_excel('c:/R/git/datavisualization/chap10/2021_행정구역별 학과수 및 학년별 재적학생수.xlsx', 
                      ## 'sheet0' 시트의 데이터를 불러오는데,
                      sheet = 'Sheet0',
                      ## 앞의 3행을 제외하고
                      skip = 3, 
                      ## 첫번째 행은 열 이름이 아님을 설정
                      col_names = FALSE, 
                      ## 열의 타입을 설정, 처음 4개는 문자형으로 다음 39개는 수치형으로 설정
                      col_types = c(rep('text', 4), rep('numeric', 39)))

## 읽어온 데이터에서 '소계', '전체'에 해당하는 데이터는 제거하고 '서울'데이터만 필터링해서 필요한 열만 남김
df_행정구역 <- df_행정구역 |> filter(...1 == '서울',  ...2 != '소계', ...3 == '전체', ...4 == '소계') |>
  select(2, 5)

## 열 이름을 적절히 설정
names(df_행정구역) <- c('구이름', '대학수')

## 구이음 열에서 '서울 ' 문자열을 ''으로 치환
df_행정구역$구이름 <- gsub('서울 ', '', df_행정구역$구이름)

## geojson_read()을 사용하여 TL_SCCO_CTPRVN.json 파일을 읽어옴
spdf_geojson <- geojson_read('D:/R/git/datavisualization/chap10/TL_SCCO_SIG.json',  what = "sp")

## st_as_sf()를 사용해 sp객체를 sf객체로 변환
sf_spdf_geojson <- st_as_sf(spdf_geojson)

## 변환된 sf객체 중 서울 데이터(SIG_CD < 12000)만 필터링
sf_spdf_seoul <- sf_spdf_geojson |> filter(SIG_CD < 12000)


sf_spdf_seoul |> ggplot() +
  geom_sf()


sf_spdf_seoul_joined <- inner_join(sf_spdf_seoul, df_행정구역, by = c('SIG_KOR_NM' = '구이름'))



sf_spdf_seoul_joined |> ggplot() +
  ## fill을 대학수로 매핑한 geom_sf 레이어 생성
  geom_sf(aes(fill = 대학수))



if(!require(geogrid)) {
  install.packages('geogrid')
  library(geogrid)
}

## calculate_grid()를 사용하여 sf_spdf_seoul_joined를 육각형 그리드형태로 변환
new_cells_hex <- calculate_grid(shape = sf_spdf_seoul_joined, grid_type = "hexagonal")

## assign_polygons()을 사용하여 sf_spdf_seoul_joined위에 new_cells_hex을 할당
resulthex <- assign_polygons(sf_spdf_seoul_joined, new_cells_hex)


## assign_polygons()의 결과를 ggplot 객체 데이터로 사용
resulthex |>
  ggplot() +
  ## fill을 대학수로 매핑한 geom_sf 레이어 생성
  geom_sf(aes(fill = 대학수)) + 
  ## X축을 V1, Y축을 V2, label을 SIG_KOR_NM을 매핑한 geom_text 레이어 추가
  geom_text(aes(x = V1, y = V2, label = SIG_KOR_NM)) + 
  ## fill 스케일을 흰색부터 dodgerblue까지 가지는 그래디언트로 설정
  scale_fill_gradient(low = 'white', high = 'dodgerblue')


## calculate_grid()를 사용하여 sf_spdf_seoul_joined를 사각형 그리드형태로 변환
new_cells_reg <- calculate_grid(shape = sf_spdf_seoul_joined, grid_type = "regular")

## assign_polygons()을 사용하여 sf_spdf_seoul_joined위에 new_cells_reg을 할당
resultreg <- assign_polygons(sf_spdf_seoul_joined, new_cells_reg)  


## assign_polygons()의 결과를 ggplot 객체 데이터로 사용
hexmap <- resultreg |>
  ggplot() +
  ## fill을 대학수로 매핑한 geom_sf 레이어 생성
  geom_sf(aes(fill = 대학수)) + 
  ## X축을 V1, Y축을 V2, label을 SIG_KOR_NM을 매핑한 geom_text 레이어 추가
  geom_text(aes(x = V1, y = V2, label = paste0(SIG_KOR_NM, '\n(', 대학수, '개)'))) + 
  ## fill 스케일을 흰색부터 dodgerblue까지 가지는 그래디언트로 설정
  scale_fill_gradient(low = 'white', high = 'dodgerblue') + 
  theme_void() + 
  labs(title = '서울의 구별 대학수')

ggplotly(hexmap)
