##  R code
## 데이터 전처리를 위한 패키지 설치 및 로딩
if(!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse) }

if(!require(readxl)) {
  install.packages('readxl')
  library(readxl) }

if(!require(readr)) {
  install.packages('readr')
  library(readr) }

if(!require(lubridate)) {
  install.packages('lubridate')
  library(lubridate) }


## 1. covid19 원본 데이터 셋 로딩
## covid19 데이터 로딩(파일을 다운로드 받은 경우)
df_covid19 <- read_csv(file = "D:/R/data/Rnpy/owid-covid-data.csv",
                       col_types = cols(date = col_date(format = "%Y-%m-%d")
                       )
)
## covid19 데이터 로딩(온라인에서 바로 로딩할 경우)
# df_covid19 <- read_csv(file = "https://covid.ourworldindata.org/data/owid-covid-data.csv",
#                             col_types = cols(Date = col_date(format = "%Y-%m-%d")
#                                              )
#                             )
## 2. 전체 데이터셋 중 최근 100일간의 데이터를 필터링한 df_covid19_100 생성
df_covid19_100 <- df_covid19 |> 
  ## 한국 데이터와 각 대륙별 데이터만을 필터링
  filter(iso_code %in% c('KOR', 'OWID_ASI', 'OWID_EUR', 'OWID_OCE', 'OWID_NAM', 'OWID_SAM', 'OWID_AFR')) |>
  ## 읽은 데이터의 마지막 데이터에서 100일전 데이터까지 필터링
  filter(date >= max(date) - 100) |>
  ## 국가명을 한글로 변환
  mutate(location = case_when(
    location == 'South Korea' ~ '한국', 
    location == 'Asia' ~ '아시아', 
    location == 'Europe' ~ '유럽', 
    location == 'Oceania' ~ '오세아니아', 
    location == 'North America' ~ '북미', 
    location == 'South America' ~ '남미', 
    location == 'Africa' ~ '아프리카')) |>
  ## 국가 이름의 순서를 설정 
  mutate(location = fct_relevel(location, '한국', '아시아', '유럽', '북미', '남미', '아프리카', '오세아니아')) |>
  ## 날짜로 정렬
  arrange(date)


## 3. df_covid19_100을 한국과 각 대륙별열로 배치한 넓은 형태의 데이터프레임으로 변환
df_covid19_100_wide <- df_covid19_100 |>
  ## 날짜, 국가명, 확진자와, 백신접종완료자 데이터만 선택
  select(date, location, new_cases, people_fully_vaccinated_per_hundred) |>
  ## 열 이름을 적절히 변경
  rename('date' = 'date', '확진자' = 'new_cases', '백신접종완료자' = 'people_fully_vaccinated_per_hundred') |>
  ## 넓은 형태의 데이터로 변환
  pivot_wider(id_cols = date, names_from = location, 
              values_from = c('확진자', '백신접종완료자')) |>
  ## 날짜로 정렬
  arrange(date)

## 4. covid19 데이터를 국가별로 요약한 df_covid19_stat 생성
df_covid19_stat <- df_covid19 |> 
  group_by(iso_code, continent, location) |>
  summarise(인구수 = max(population, na.rm = T), 
            전체사망자수 = sum(new_deaths, na.rm = T), 
            백신접종자완료자수 = max(people_fully_vaccinated, na.rm = T),
            인구백명당백신접종완료율 = max(people_fully_vaccinated_per_hundred, na.rm = T),
            인구백명당부스터접종자수 = max(total_boosters_per_hundred, na.rm = T)) |> 
  ungroup() |>
  mutate(십만명당사망자수 = round(전체사망자수 / 인구수 *100000, 5),
         백신접종완료율 = 백신접종자완료자수 / 인구수)

## 여백 설정을 위한 변수 설정
margins_R <- list(t = 50, b = 25, l = 25, r = 25)


##################################################################################
##  대학 학과 취업률 데이터 로딩
df_취업률 <- read_excel('D:/R/data/Rnpy/2021년 학과별 고등교육기관 취업통계.xlsx', 
                     ## '학과별' 시트의 데이터를 불러오는데,
                     sheet = '학과별',
                     ## 앞의 13행을 제외하고
                     skip = 13, 
                     ## 첫번째 행은 열 이름으로 설정
                     col_names = TRUE, 
                     ## 열의 타입을 설정, 처음 9개는 문자형으로 다음 79개는 수치형으로 설정
                     col_types = c(rep('text', 9), rep('numeric', 79)))

## df_취업률에서 첫번째부터 9번째까지의 열과 '계'로 끝나는 열을 선택하여 다시 df_취업률에 저장
df_취업률 <- df_취업률 |> 
  select(1:9, ends_with('계'), '입대자')

## df_취업률에서 졸업자가 500명 이하인 학과 중 25% 샘플링
df_취업률_500 <- df_취업률 |> 
  filter(졸업자_계 < 500) |>
  mutate(id = row_number()) |>
  filter(row_number() %in% seq(from = 1, to = nrow(df_취업률), by = 4))

## 열 이름을 적절히 설정
names(df_취업률_500)[10:12] <- c('졸업자수', '취업률', '취업자수')



##################################################################################
if(!require(plotly)) {        ## plotly 로딩이 안되면
  install.packages('plotly')  ## plotly  패키지 설치
  library(plotly)             ## plotly 패키지 로딩
}


##################################################################################
R_layout_scatter <- df_취업률_500 |> 
  filter(졸업자수 < 500) |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수) |> 
  ## title 속성의 설정
  layout(title = list(text = '<b>졸업자 대비 취업자수</b>', 
                      x = 0.5, xanchor = 'center', yanchor = 'top'))

R_layout_scatter



##################################################################################
R_layout_scatter |> 
  ## title의 HTML inline 설정
  layout(title = list(text = "<span style = 'font-size:15pt'><span style = 'color:red;font-weight:bold;'> 졸업자</span><span style = 'font-size:10pt'> 대비</span> <span style = 'color:blue;font-weight:bold;'>취업자</span></span>", 
                      x = 0.5, xanchor = 'center', yanchor = 'top'))


##################################################################################
R_layout_scatter <- R_layout_scatter |> 
  ## 페이퍼 배경색과 플롯 배경색의 설정
  layout(paper_bgcolor = 'lightgray', plot_bgcolor = 'lightgray')

R_layout_scatter


##################################################################################
R_layout_scatter <- R_layout_scatter |>
  layout(xaxis = list(
    title = list(text = '<b>학과 졸업자수</b><sub>(명)</sub>'), ## 정상적 방법
    color =  'black', zerolinecolor = 'black', zerolinewidth = 3,
    gridcolor = 'gray', gridwidth = 1), 
    yaxis = list(
      title = '<b>학과 취업자수</b><sub>(명)</sub>', 
      color = 'black', zerolinecolor = 'black', zerolinewidth = 3,
      gridcolor = 'gray', gridwidth = 1) ## 약식 방법
  )

R_layout_scatter


##################################################################################
R_layout_scatter <- R_layout_scatter |> 
  layout(xaxis = list(
    tickmode = 'array',   ## tickmode를 "array"로 설정
    ticktext = c('소규모', '중규모', '대규모'),  ## ticktext 설정
    tickvals = c(100, 300, 400)),  ## tickvals 설정
    yaxis = list(
      tickmode = 'linear',  ## tickmode를 "linear"로 설정
      tick0 = 100,   ## tick0 설정
      dtick = 100))   ## dtick 설정

R_layout_scatter


##################################################################################
R_layout_scatter |>
  layout(xaxis = list(range = c(0, 350),  ## X축의 range 설정
                      rangemode = 'nonnegative'),  ## X축의 rangemode 설정
         yaxis = list(range = c(0, 300),  ## Y축의 range 설정
                      rangemode = 'tozero'), ## Y축의 rangemode 설정
         margin = list(pad = 5))


##################################################################################
## 초기화 및 한국 확진자 선 scatter 트레이스 생성
R_layout_line <- df_covid19_100_wide |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_한국, name = '한국')

## 아시아 확진자 선 scatter 트레이스 추가
R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_아시아, name = '아시아', showlegend = FALSE)

## 유럽 확진자 선 scatter 트레이스 추가
R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_유럽, name = '유럽')

## 북미 확진자 선 scatter 트레이스 추가
R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_북미, name = '북미')

## 범례 layout 설정
R_layout_line <- R_layout_line |> 
  layout(title = list(text = '<b>대륙별 신규 확진자수 추이</b>',
                      x = 0.5, xanchor = 'center', yanchor = 'top'),
         legend = list(orientation = 'v',  bordercolor = 'gray', borderwidth = 2,
                       x = 0.95, y = 0.95, xanchor = 'right')
  )

R_layout_line


##################################################################################
R_layout_line <- R_layout_line |> 
  ## 여백 설정
  layout(margin = list(t = 50, b = 25, l = 25, r = 25))

R_layout_line


##################################################################################
R_layout_scatter |> 
  ## 플롯 사이즈 설정
  layout(width = 450, height = 700)


##################################################################################
R_layout_line <- R_layout_line |> 
  ## 폰트 설정
  layout(font = list(family = "나눔고딕", color = 'MidnightBlue', size = 12))

R_layout_line

