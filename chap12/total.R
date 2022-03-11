
library(showtext)
showtext_auto()
library(tidyverse)
library(readxl)
library(ggtext)
library(geofacet)

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


df_seoul_grid <- read_excel('c:/R/git/datavisualization/chap12/seoul_grid.xlsx', 
                            ## 'sheet0' 시트의 데이터를 불러오는데,
                            sheet = 'Sheet1', col_names = TRUE, col_types = c('text', rep('numeric', 2)))

df_seoul_grid$code <- df_seoul_grid$name


# seoul_grid <- data.frame(
#   name = c("강북구", "도봉구", "은평구", "종로구", "성북구", "노원구", "중랑구", "강서구", "양천구", "마포구", "서대문구", "중구", "동대문구", "광진구", "강동구", "구로구", "영등포구", "동작구", "용산구", "성동구", "송파구", "금천구", "관악구", "서초구", "강남구"),
#   row = c(1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5),
#   col = c(5, 6, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 8, 2, 3, 4, 5, 6, 7, 3, 4, 5, 6),
#   code = c("강북구", "도봉구", "은평구", "종로구", "성북구", "노원구", "중랑구", "강서구", "양천구", "마포구", "서대문구", "중구", "동대문구", "광진구", "강동구", "구로구", "영등포구", "동작구", "용산구", "성동구", "송파구", "금천구", "관악구", "서초구", "강남구"),
#   stringsAsFactors = FALSE
# )

df_행정구역 |>
  ggplot() +
  geom_col(aes(x = 과정구분, y = 재적학생수, fill = 과정구분)) +
  facet_geo(~code, grid = df_seoul_grid) + 
  theme(
    strip.background = element_blank(), 
    axis.line = element_blank(), 
    axis.text = element_blank(),
    strip.text = element_textbox_simple(family = 'NanumBarunGothicBold', 
                                        size = 10,
                                        color = "white", fill = "dodgerblue4", box.color = "#4A618C",
                                        halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
                                        padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)), 
    panel.border = element_rect(color = 'red', fill = NA), 
    axis.ticks = element_blank()
  ) +
  labs(x = NULL, y = NULL)




