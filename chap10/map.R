library(tidyverse)
library(readxl)
library(geojsonio)
# library(RColorBrewer)
# library(rgdal)
# library(sp)
# library(sf)
# library(tmap)


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
# library(rjson)

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

plotly_spdf <- fromJSON(file = "D:/R/git/datavisualization/chap10/TL_SCCO_CTPRVN.json")

#plotly_spdf@data <- merge(plotly_spdf, df_입학자_join, by.x = 'CTPRVN_CD', by.y = 'id')

fig <- plot_ly() %>% add_trace(
  type="choropleth",
  geojson=plotly_spdf, 
  featureidkey = 'properties.CTPRVN_CD', 
  locations = df_입학자_join$id, 
  z = df_입학자_join$일반대학, 
  colorscale="Viridis", 
  text = df_입학자_join$지역,
  marker=list(line=list(
    width=0)
  )
  )
fig <- fig %>% layout(
  geo=list(
    fitbounds = "locations",
#    showlakes = TRUE,
    visible = FALSE,
#    zoom = 1,
    center=list(lon=127, lat=36) )
#    showland = T,
#    landcolor = toRGB("grey90"))
)
fig

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')


map_data("world", "south korea") %>%
  group_by(group) %>%
  plot_geo(x = ~long, y = ~lat) %>%
  add_lines(size = I(1))


map_data("world", "south korea") |> View()
