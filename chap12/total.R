library(readxl)
library(geojsonio)
library(sf)
library(tidyverse)
library(ggpp)
showtext_auto()

font_add('NanumBarunGothic', 'c:/windows/fonts/NanumBarunGothic.ttf')
font_add('NanumBarunGothicBold', 'c:/windows/fonts/NanumBarunGothicBold.ttf')

df_seoul_grid <- read_excel('c:/R/git/datavisualization/chap12/seoul_grid.xlsx', 
                      ## 'sheet0' 시트의 데이터를 불러오는데,
                      sheet = 'Sheet1', col_names = TRUE, col_types = c('text', rep('numeric', 2)))
                      

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
df_행정구역 <- df_행정구역 |> filter(...1 == '서울',  ...2 != '소계', ...3 != '전체', ...4 == '소계') |>
  select(1, 2, 3, 4, 5, 10)

## 열 이름을 적절히 설정
names(df_행정구역) <- c('시도', 'code', '과정구분', '학제구분', '대학수', '재적학생수')

## 구이음 열에서 '서울 ' 문자열을 ''으로 치환
df_행정구역$code <- gsub('서울 ', '', df_행정구역$code)


## geojson_read()을 사용하여 TL_SCCO_CTPRVN.json 파일을 읽어옴
spdf_geojson <- geojson_read('C:/R/git/map/TL_SCCO_SIG.json',  what = "sp")

## st_as_sf()를 사용해 sp객체를 sf객체로 변환
sf_spdf_geojson <- st_as_sf(spdf_geojson)

## 변환된 sf객체 중 서울 데이터(SIG_CD < 12000)만 필터링
sf_spdf_seoul <- sf_spdf_geojson |> filter(SIG_CD < 12000)


sf_spdf_seoul |> ggplot() +
  geom_sf()


if(!require(geogrid)) {
  install.packages('geogrid')
  library(geogrid)
}


## calculate_grid()를 사용하여 sf_spdf_seoul_joined를 사각형 그리드형태로 변환
new_cells_reg <- calculate_grid(shape = sf_spdf_seoul, grid_type = "regular")
View(new_cells_reg)
## assign_polygons()을 사용하여 sf_spdf_seoul_joined위에 new_cells_reg을 할당
resultreg <- assign_polygons(sf_spdf_seoul, new_cells_reg)  

## assign_polygons()의 결과를 ggplot 객체 데이터로 사용
resultreg |>
  ggplot() +
  ## fill을 대학수로 매핑한 geom_sf 레이어 생성
  geom_sf() + 
  ## X축을 V1, Y축을 V2, label을 SIG_KOR_NM을 매핑한 geom_text 레이어 추가
  geom_text(aes(x = V1, y = V2, label = paste0(SIG_KOR_NM)))

+ 
  ## fill 스케일을 흰색부터 dodgerblue까지 가지는 그래디언트로 설정
  scale_fill_gradient(low = 'white', high = 'dodgerblue') + 
  theme_void() + 
  labs(title = '서울의 구별 대학수')

seoul_grid <- resultreg |> as.data.frame() |> select(row, col, code = SIG_KOR_NM, name = SIG_KOR_NM)

sf_spdf_seoul_joined <- inner_join(resultreg, df_행정구역, by = c('SIG_KOR_NM' = '행정구역'))


str(seoul_grid)

sf_spdf_seoul_joined |>
  ggplot() +
  geom_col(aes(x = 과정구분, y = 재적학생수)) + 
  facet_grid(rows = vars(row), cols = vars(col))


facet_geo(~행정구역, grid = "kr_seoul_district_grid1")


library(geofacet)

sf_spdf_seoul_joined |> ggplot(aes(variable, rank, fill = variable)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  facet_grid(~ state, grid = "kr_seoul_district_grid1")

str(kr_seoul_district_grid1)
head(state_unemp)

grid_design(data = df_seoul_grid)

df_seoul_grid$code = df_seoul_grid$name

grid_preview(df_seoul_grid)

grid_design(data = df_seoul_grid)

mygrid <- data.frame(
  name = c("강북구", "도봉구", "은평구", "종로구", "성북구", "노원구", "중랑구", "강서구", "양천구", "마포구", "서대문구", "중구", "동대문구", "광진구", "강동구", "구로구", "영등포구", "동작구", "용산구", "성동구", "송파구", "금천구", "관악구", "서초구", "강남구"),
  row = c(1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5),
  col = c(5, 6, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 8, 2, 3, 4, 5, 6, 7, 3, 4, 5, 6),
  code = c("강북구", "도봉구", "은평구", "종로구", "성북구", "노원구", "중랑구", "강서구", "양천구", "마포구", "서대문구", "중구", "동대문구", "광진구", "강동구", "구로구", "영등포구", "동작구", "용산구", "성동구", "송파구", "금천구", "관악구", "서초구", "강남구"),
  stringsAsFactors = FALSE
)

grid_submit(mygrid, name = "mygrid", desc = "An awesome grid...")






df_행정구역 |>
  ggplot() +
  geom_col(aes(x = 과정구분, y = 재적학생수, fill = 과정구분)) +
  facet_geo(~code, grid = mygrid) + 
  theme(
    strip.background = element_blank(), 
    axis.line = element_blank(), 
    axis.text = element_blank(),
    strip.text = element_textbox_simple(family = 'NanumBarunGothicBold', 
                                        size = 10,
                                        color = "white", fill = "dodgerblue4", box.color = "#4A618C",
                                        halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
                                        padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)), 
    panel.border = element_rect(color = 'red', fill = NA)
  ) +
  labs(x = NULL, y = NULL)
  
  
  
  
  
  geom_text_npc(aes(npcx = 0.5, npcy = 0.9, label = name)) +
  facet_grid(rows = vars(row), cols = vars(col),drop=T) + 
  theme_void() +
  theme(
    strip.text.x = element_blank(),
    strip.text.y = element_blank(), 
    strip.background = element_blank(),
    panel.border = element_rect(color = 'red', fill = NA)
    )

g <- ggplotGrob(myplot)



library(cowplot)
library(grid)

grob <- ggplotGrob(myplot);

View(grob)

grob$layout$name

idx <- which(grob$layout$name %in% c("panel-2-1", "panel-3-1", "panel-3-2"));
for (i in idx) grob$grobs[[i]] <- nullGrob();









install.packages('ggpp')
library(ggpp)  
  

seoul_grid1 <- kr_seoul_district_grid1
install.packages("statebins", repos = c("https://cinc.rud.is", "https://cloud.r-project.org/"))
install.packages('statebin')
install.packages('cdcfluview')

library(statebins)
library(cdcfluview)
library(hrbrthemes)
library(tidyverse)


setwd("D:/R/git/datavisualization/starwars")
adat <- read_csv("D:/R/git/datavisualization/starwars/wapo.csv")

mutate(
  adat, 
  share = cut(avgshare94_00, breaks = 4, labels = c("0-1", "1-2", "2-3", "3-4"))
) %>% 
  statebins(
    value_col = "share", 
    ggplot2_scale_function = scale_fill_brewer,
    name = "Share of workforce with jobs lost or threatened by trade"
  ) +
  labs(title = "1994-2000") +
  theme_statebins()

geom_statebins()

