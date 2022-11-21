# library(tidyverse)
# library(geojsonio)
# library(RColorBrewer)
# library(rgdal)
# library(sp)
# library(sf)
# library(tmap)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
counties$features[[1]]

url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- read.csv(url2, colClasses=c(fips="character"))

df_입학자 <- read_excel('d:/R/git/datavisualization/chap3/2021_연도별 입학자수.xlsx', 
                     ## 'data' 시트의 데이터를 불러오는데,
                     sheet = 'Sheet0',
                     ## 앞의 10행을 제외하고
                     skip = 3, 
                     ## 첫번째 행은 열 이름을 설정
                     col_names = FALSE, 
                     ## 열의 타입을 설정, 처음 8개는 문자형으로 다음 56개는 수치형으로 설정
                     col_types = c(rep('text', 2), rep('numeric', 30)))
df_입학자 <- df_입학자 |> select(1, 2, 5, 7, 9, 11, 13, 19, 29, 31)

## df_입학자의 열이름을 적절한 이름으로 설정
colnames(df_입학자) <- c('연도', '지역', '전문대학', '교육대학', '일반대학', '방송통신대학', '산업대학', '원격및사이버대학', '석사', '박사')

df_입학자 <- df_입학자 |> filter(!is.na(지역))
library(plotly)
library(rjson)

plotly_spdf <- fromJSON(file = "D:/R/git/datavisualization/chap10/TL_SCCO_SIG.json", simplify=TRUE)

sf_spdf <- st_as_sf(plotly_spdf)

df_입학자_join <- df_입학자 |> filter(연도 == '2021', 지역 != '전체') |> 
  mutate(id = case_when(
    지역 == '강원' ~ '42', 
    지역 == '경기' ~ '41',
    지역 == '경남' ~ '48',
    지역 == '경북' ~ '47',
    지역 == '광주' ~ '29',
    지역 == '대구' ~ '27',
    지역 == '대전' ~ '30',
    지역 == '부산' ~ '26',
    지역 == '서울' ~ '11',
    지역 == '세종' ~ '36',
    지역 == '울산' ~ '31',
    지역 == '인천' ~ '28',
    지역 == '전남' ~ '46',
    지역 == '전북' ~ '45',
    지역 == '제주' ~ '50',
    지역 == '충남' ~ '44',
    지역 == '충북' ~ '43'
  ))

inner_join(spdf_shp, df_입학자_join, by = c('CTPRVN_CD' = 'id')) |>
  ggplot() + 
  ## fill을 일반대학으로 매핑하고 color를 설정한 geom_sf 레이어 생성
  geom_sf(aes(fill = 일반대학), color = 'dodgerblue') + 
  ## fill 스케일을 흰색부터 dodgerblue까지의 그래디언트 색으로 설정
  scale_fill_gradient(low = 'white', high = 'dodgerblue')


plotly_spdf$features[[1]]

fig <- plot_ly()

fig <- fig %>% add_trace(
  type="scattergeo",
  geojson=plotly_spdf, 
  mode = 'markers'
  )

fig


fig <- fig %>% colorbar(title = "Unemployment Rate (%)")
fig <- fig %>% layout(
  title = "2016 US Unemployment by County"
)

fig <- fig %>% layout(
  geo = g
)

fig








spdf <- geojson_read("C:/R/git/datavisualization/chap10/TL_SCCO_SIG.json",  what = "sp")

class(spdf)

# Show it
plot(spdf)

sp_spdf <- st_as_sf(spdf)

class(sp_spdf)

plot_ly(sp_spdf)

plot_mapbox(sp_spdf)

ggplot() + 
  geom_sf(data = sp_spdf) + 
  scale_y_continuous(breaks = 34:36)

sp_spdf1 <- st_make_valid(sp_spdf)

df_sf <- st_transform(sp_spdf1)

tmap::tm_shape(sp_spdf1) + 
  tm_polygons("SIG_CD", palette = "viridis", legend.show = FALSE) +
  tm_text("SIG_KOR_NM")

new_cells_hex <- calculate_grid(shape = sp_spdf1, grid_type = "hexagonal", seed = 3)
resulthex <- assign_polygons(sp_spdf1, new_cells_hex)

new_cells_reg <- calculate_grid(shape = sp_spdf1, grid_type = "regular", seed = 3)
resultreg <- assign_polygons(sp_spdf1, new_cells_reg)  
write.csv(resultreg, 'resultreg.csv')
getwd()
hexplot <- tm_shape(resulthex) +
  tm_polygons("CTPRVN_CD", palette = "viridis", legend.show = FALSE) +
  tm_text("CTP_KOR_NM", size = 0.5)

regplot <- tm_shape(resultreg) +
  tm_polygons("SIG_CD", palette = "viridis", legend.show = FALSE) +
  tm_text("SIG_KOR_NM", size = 0.2)

View(regplot)

tmap_arrange(hexplot, regplot, nrow = 2)


