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
## 서브플롯 생성을 위한 기본 plotly 객체 생성
p_line_wide <- df_covid19_100_wide |> plot_ly()

## 첫 번째 서브플롯 생성
p1 <- p_line_wide |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_한국, name = '한국') |> 
  layout(title = '한국', 
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 두 번째 서브플롯 생성
p2 <- p_line_wide |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_아시아, name = '아시아') |> 
  layout(title = '아시아', 
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 세 번째 서브플롯 생성
p3 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_유럽, name = '유럽') |> 
  layout(title = '유럽', 
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 네 번째 서브플롯 생성
p4 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_북미, name = '북미') |> 
  layout(title = '북미', 
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 댜섯 번째 서브플롯 생성
p5 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_남미, name = '남미') |> 
  layout(title = '남미', 
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 여섯 번째 서브플롯 생성
p6 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date,
            y = ~확진자_아프리카, name = '아프리카') |> 
  layout(title = '아프리카', 
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 일곱 번째 서브플롯 생성
p7 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_오세아니아, name = '오세아니아') |> 
  layout(title = '오세아니아', 
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

subplots <- subplot(p1, p2, p3, p4, p5, p6, p7, nrows = 3) |>
  layout(## 전체 제목 설정
    title = '최근 100일간 코로나19 확진자수',
    ## 전체 여백 설정
    margin = margins_R)

subplots



##################################################################################
df_covid19_100 |>
  ## 국가명으로 그룹화
  group_by(location) |>
  ## 그룹화한 각각의 데이터 그룹들에 적용할 코드 설정
  do(
    ## 각 그룹화한 데이터를 사용해 plotly 객체 생성    
    p = plot_ly(.) |> 
      ## line 모드의 스캐터 trace 추가
      add_trace(type = 'scatter', mode = 'lines',
                ## X, Y축에 변수 매핑, color를 설정
                x = ~date, y = ~new_cases, name = ~location) |>
      add_annotations(x = 0.5 , y = 1.02, text = ~location, 
                      showarrow = F, xref='paper', 
                      yref='paper', xanchor = 'center') |>
      ## layout으로 X, Y축을 설정
      layout(xaxis = list(tickfont = list(size = 10)),  
             yaxis = list(title = list(text = '확진자수')))
  ) |>
  ## 생성된 plotly 객체들을 subplot 생성
  subplot(nrows = 3, margin = 0.04) |>
  ## 생성된 subplot의 layout 설정
  layout(showlegend = TRUE, 
         title = '최근 100일간 코로나19 확진자수',
         margin = margins_R)



##################################################################################
subplots |> 
  ## 범례는 제거
  layout(showlegend = FALSE)



##################################################################################
subplot(
  p1, p2, p3, plotly_empty(), p4, p5, plotly_empty(), p6, p7,
  ## 서브플롯은 3개의 열로 설정
  nrows = 3,
  ## 서브플롯간의 여백 설정
  heights = c(0.5, 0.25, 0.25), 
  widths = c(0.5, 0.25, 0.25), 
  margin = 0.04) |> 
  ## 범례는 제거
  layout(showlegend = TRUE,
         ## 전체 제목 설정
         title = '최근 100일간 코로나19 확진자수',
         ## 전체 여백 설정
         margin = margins_R)



##################################################################################
subplot(
  ## 아래의 nrows가 2이기 때문에 맨 위 열에 p1 하나를 위치시킴
  p1, 
  ## subplot()으로 p2부터 p7까지를 묶어 하나의 플롯으로 만듬
  subplot(p2, p3, p4, p5, p6, p7,
          ## 서브플롯은 2개의 열로 설정함으로써 2행 3열 서브플롯 생성
          nrows = 2), 
  ## 전체 서브플롯은 2열로 구성
  nrows = 2
) |> 
  ## 범례는 제거
  layout(showlegend = TRUE,
         ## 전체 제목 설정
         title = '최근 100일간 코로나19 확진자수',
         ## 전체 여백 설정
         margin = margins_R)



##################################################################################
subplot(
  p1, 
  subplot(p2, p3, p4, p5, p6, p7,
          nrows = 2, 
          ## shareX, shareY를 TRUE로 설정하여 축 공유
          shareX = TRUE, shareY = TRUE), 
  nrows = 2, margin = 0.04
) |> 
  layout(showlegend = TRUE, 
         title = '최근 100일간 코로나19 확진자수',
         margin = margins_R)





