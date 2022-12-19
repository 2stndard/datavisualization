library(raster)
library(sf)

# Get the map data in sf format
map_data <- getData("GADM", country = "KOR", level = 1, type = "sf")
st_crs(map_data) <- st_crs(map_data)

province <- map_data$HASC_1

####  충원율 데이터
df_충원율 <- read_excel('D:/R/git/datavisualization/plotly/RnPy/chap6/고등 주요 01-시도별 신입생 충원율(2010-2022)_220825y.xlsx', 
                     ## 'data' 시트의 데이터를 불러오는데,
                     sheet = 'Sheet1',
                     ## 앞의 10행을 제외하고
                     skip = 7, 
                     ## 첫번째 행은 열 이름을 설정
                     col_names = FALSE, 
                     ## 열의 타입을 설정, 처음 8개는 문자형으로 다음 56개는 수치형으로 설정
                     col_types = c(rep('text', 2), rep('numeric', 12)))

df_충원율 <- df_충원율 |> dplyr::select(1, 2, 3, 4, 5)

## df_입학자의 열이름을 적절한 이름으로 설정
colnames(df_충원율) <- c('연도', '지역', '정원내모집인원', '정원내입학생수', '신입생충원율')


df_충원율 <- df_충원율 |> filter(연도 == '2022', 지역 != '전국') |> 
  mutate(id = case_when(
    지역 == '강원' ~ 'KR.KW', 
    지역 == '경기' ~ 'KR.KG',
    지역 == '경남' ~ 'KR.KN',
    지역 == '경북' ~ 'KR.KB',
    지역 == '광주' ~ 'KR.KJ',
    지역 == '대구' ~ 'KR.TG',
    지역 == '대전' ~ 'KR.TJ',
    지역 == '부산' ~ 'KR.PU',
    지역 == '서울' ~ 'KR.SO',
    지역 == '세종' ~ 'KR.SJ',
    지역 == '울산' ~ 'KR.UL',
    지역 == '인천' ~ 'KR.IN',
    지역 == '전남' ~ 'KR.CN',
    지역 == '전북' ~ 'KR.CB',
    지역 == '제주' ~ 'KR.CJ',
    지역 == '충남' ~ 'KR.GN',
    지역 == '충북' ~ 'KR.GB'
  ))

plot_dat <- left_join(map_data, df_충원율, by = c("HASC_1" = "id")) %>%
  st_as_sf()


plot_ly(plot_dat) %>%
  add_sf(type = "scatter", 
         split = ~지역,
         color = ~신입생충원율,
         showlegend = F,       # don't show a legend for each region
         colors = "Blues",
         text = ~paste0(지역, "\n", round(신입생충원율, 2), '%'),
         hoveron = "fills",
         hoverinfo = "text") %>%
  layout(title = '22년 전국 대학 신입생 충원율', 
         margin = margins_R) 
