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

if(!require(plotly)) {        ## plotly 로딩이 안되면
  install.packages('plotly')  ## plotly  패키지 설치
  library(plotly)             ## plotly 패키지 로딩
}

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
if (!require('remotes')) {
  install.packages("remotes")
  library(remotes)
}

remotes::install_github("plotly/dashR")

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

total_deaths_5_nations_by_day <- df_covid19 |> 
  filter((iso_code %in% c('KOR', 'USA', 'JPN', 'GBR', 'FRA'))) |>
  filter(!is.na(total_deaths_per_million))

last_day = max(distinct(total_deaths_5_nations_by_day, date) |> pull())

fig <- total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location) |>
  add_annotations( 
    x =~ (total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(date) |> pull()), 
    y = ~(total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(total_deaths_per_million) |> pull()),
    text = ~(total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(location) |> pull()), 
    textposition = 'middle right', xanchor = 'left', showarrow = FALSE
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', range = c('2020-02-15', format(last_day, format="%Y-%m-%d"))), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R,
         showlegend = FALSE)

max_deaths_per_million_by_day <- total_deaths_5_nations_by_day |> group_by(location) |>
  summarise(최대사망자 = max(new_deaths_per_million, na.rm = TRUE))

deaths_per_million_in_lateast <- total_deaths_5_nations_by_day |> group_by(location) |>
  filter(is.na(new_deaths_per_million) == FALSE) |>
  filter(date == max(date)) |>
  select(iso_code, date, new_deaths_per_million)

df_gauge <- left_join(max_deaths_per_million_by_day, deaths_per_million_in_lateast, by = 'location') |> arrange(location)

## 한국 게이지 인디케이터 생성
fig_gauge_kor <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[3, 1]),
            value = pull(df_gauge[3, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[3, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[3, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[3, 2])*1.2*0.5, pull(df_gauge[3, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[3, 2])*1.2*0.75, pull(df_gauge[3, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[3, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 프랑스 게이지 인디케이터 생성
fig_gauge_fra <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[1, 1]),
            value = pull(df_gauge[1, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[1, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[1, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[1, 2])*1.2*0.5, pull(df_gauge[1, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[1, 2])*1.2*0.75, pull(df_gauge[1, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[1, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 일본 게이지 인디케이터 생성
fig_gauge_jpn <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[2, 1]),
            value = pull(df_gauge[2, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[2, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[2, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[2, 2])*1.2*0.5, pull(df_gauge[2, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[2, 2])*1.2*0.75, pull(df_gauge[2, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[2, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 영국 게이지 인디케이터 생성
fig_gauge_gbr <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[4, 1]),
            value = pull(df_gauge[4, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[4, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[4, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[4, 2])*1.2*0.5, pull(df_gauge[4, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[4, 2])*1.2*0.75, pull(df_gauge[4, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[4, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 미국 게이지 인디케이터 생성
fig_gauge_usa <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[5, 1]),
            value = pull(df_gauge[5, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[5, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[5, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[5, 2])*1.2*0.5, pull(df_gauge[5, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[5, 2])*1.2*0.75, pull(df_gauge[5, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[5, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## Dash 앱의 초기화
app <- dash_app()

##  Dash 앱 레이아웃 설정
app |> set_layout(
  ##  첫 번째 div 설정
  htmlDiv(id = 'header', children = list(
    ## 첫 레벨 제목 설정
    htmlH1('코로나 19 사망자수 추세',
           ## 제목의 스타일 설정
           style = list(textAlign = 'center', color = 'darkblue'))
  )
  ),
  ##  두 번째 div 설정
  htmlDiv(id = 'second block', children = list(
    ##  두 번째 좌측 div 설정
    htmlDiv(id = 'picker', children = list(
      htmlLabel('날짜 선택', style = list(position = 'absolute', top = '10%', left = '5%')),
      dccDatePickerSingle(date = last_day, placeholder='Select a date', clearable=TRUE,
                          style = list(position = 'absolute', top = '15%', left = '3%', width = '10px'))),
      style = list(width = '20%', display = 'inline-block')),
    ##  두 번째 우측 div 설정
    htmlDiv(
      dccGraph(figure=fig), 
      style = list(width = '80%', display = 'inline-block'))
  )
  ),
  ##  세 번째 div  설정
  htmlDiv(id = 'third block', 
          children = list(
            dccGraph(id = 'indicator_kor',figure = fig_gauge_kor,
                     style = list(width = '20%', display = 'inline-block')),
            dccGraph(id = 'indicator_fra',figure = fig_gauge_fra,
                     style = list(width = '20%', display = 'inline-block')),
            dccGraph(id = 'indicator_jpn',figure = fig_gauge_jpn,
                     style = list(width = '20%', display = 'inline-block')),
            dccGraph(id = 'indicator_gbr',figure = fig_gauge_gbr,
                     style = list(width = '20%', display = 'inline-block')),
            dccGraph(id = 'indicator_usa',figure = fig_gauge_usa,
                     style = list(width = '20%', display = 'inline-block'))
          )
  )
)

app |> add_callback(
  list(
    output(id = 'indicator_kor', property = 'figure'), 
    output(id = 'indicator_fra', property = 'figure'), 
    output(id = 'indicator_jpn', property = 'figure'), 
    output(id = 'indicator_gbr', property = 'figure'), 
    output(id = 'indicator_usa', property = 'figure'), 
    output(id = 'line_5_nations', property = 'figure')),
  input(id = 'datepicker', property = 'date'),
  function(date_value) {
    
    deaths_per_million_update <- total_deaths_5_nations_by_day |> 
      filter(is.na(new_deaths_per_million) == FALSE) |>
      filter(date == date_value) |>
      select(location, new_deaths_per_million) |> arrange(location)
    ########
    ## 한국 게이지 인디케이터 생성
    fig_gauge_kor <- df_gauge |> plot_ly() |>
      add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[3, 1]),
                value = pull(deaths_per_million_update[3, 2]), 
                gauge = list(axis = list(
                  range = list(NULL, pull(df_gauge[3, 2])*1.2)),
                  steps = list(
                    list(range = c(0, pull(df_gauge[3, 2])*1.2*0.5), color = "lightgray"),
                    list(range = c(pull(df_gauge[3, 2])*1.2*0.5, pull(df_gauge[3, 2])*1.2*0.75), color = "darkgray"),
                    list(range = c(pull(df_gauge[3, 2])*1.2*0.75, pull(df_gauge[3, 2])*1.2), color = "gray")),
                  threshold = list(line = list(color = 'white'),
                                   value = pull(df_gauge[3, 2])), 
                  bar = list(color = "darkblue")), 
                number = list(suffix = '명')) 
    
    ## 프랑스 게이지 인디케이터 생성
    fig_gauge_fra <- df_gauge |> plot_ly() |>
      add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[1, 1]),
                value = pull(deaths_per_million_update[1, 2]), 
                gauge = list(axis = list(
                  range = list(NULL, pull(df_gauge[1, 2])*1.2)),
                  steps = list(
                    list(range = c(0, pull(df_gauge[1, 2])*1.2*0.5), color = "lightgray"),
                    list(range = c(pull(df_gauge[1, 2])*1.2*0.5, pull(df_gauge[1, 2])*1.2*0.75), color = "darkgray"),
                    list(range = c(pull(df_gauge[1, 2])*1.2*0.75, pull(df_gauge[1, 2])*1.2), color = "gray")),
                  threshold = list(line = list(color = 'white'),
                                   value = pull(df_gauge[1, 2])), 
                  bar = list(color = "darkblue")), 
                number = list(suffix = '명'))
    
    ## 일본 게이지 인디케이터 생성
    fig_gauge_jpn <- df_gauge |> plot_ly() |>
      add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[2, 1]),
                value = pull(deaths_per_million_update[2, 2]), 
                gauge = list(axis = list(
                  range = list(NULL, pull(df_gauge[2, 2])*1.2)),
                  steps = list(
                    list(range = c(0, pull(df_gauge[2, 2])*1.2*0.5), color = "lightgray"),
                    list(range = c(pull(df_gauge[2, 2])*1.2*0.5, pull(df_gauge[2, 2])*1.2*0.75), color = "darkgray"),
                    list(range = c(pull(df_gauge[2, 2])*1.2*0.75, pull(df_gauge[2, 2])*1.2), color = "gray")),
                  threshold = list(line = list(color = 'white'),
                                   value = pull(df_gauge[2, 2])), 
                  bar = list(color = "darkblue")), 
                number = list(suffix = '명'))
    
    ## 영국 게이지 인디케이터 생성
    fig_gauge_gbr <- df_gauge |> plot_ly() |>
      add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[4, 1]),
                value = pull(deaths_per_million_update[4, 2]), 
                gauge = list(axis = list(
                  range = list(NULL, pull(df_gauge[4, 2])*1.2)),
                  steps = list(
                    list(range = c(0, pull(df_gauge[4, 2])*1.2*0.5), color = "lightgray"),
                    list(range = c(pull(df_gauge[4, 2])*1.2*0.5, pull(df_gauge[4, 2])*1.2*0.75), color = "darkgray"),
                    list(range = c(pull(df_gauge[4, 2])*1.2*0.75, pull(df_gauge[4, 2])*1.2), color = "gray")),
                  threshold = list(line = list(color = 'white'),
                                   value = pull(df_gauge[4, 2])), 
                  bar = list(color = "darkblue")), 
                number = list(suffix = '명'))
    
    ## 미국 게이지 인디케이터 생성
    fig_gauge_usa <- df_gauge |> plot_ly() |>
      add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[5, 1]),
                value = pull(deaths_per_million_update[5, 2]), 
                gauge = list(axis = list(
                  range = list(NULL, pull(df_gauge[5, 2])*1.2)),
                  steps = list(
                    list(range = c(0, pull(df_gauge[5, 2])*1.2*0.5), color = "lightgray"),
                    list(range = c(pull(df_gauge[5, 2])*1.2*0.5, pull(df_gauge[5, 2])*1.2*0.75), color = "darkgray"),
                    list(range = c(pull(df_gauge[5, 2])*1.2*0.75, pull(df_gauge[5, 2])*1.2), color = "gray")),
                  threshold = list(line = list(color = 'white'),
                                   value = pull(df_gauge[5, 2])), 
                  bar = list(color = "darkblue")), 
                number = list(suffix = '명'))
    
    fig_temp <- fig |> layout(shapes = list(type = 'line',
                                            y0 = 0, y1 = max(total_deaths_5_nations_by_day$total_deaths_per_million), yref = "y", 
                                            x0 = date_value, x1 = date_value,
                                            line = list(color = 'black', dash="dot")))
    
    list(fig_gauge_kor, fig_gauge_fra, fig_gauge_jpn, fig_gauge_gbr, fig_gauge_usa, fig_temp)
    ########        
  }
)



##  Dash 앱 실행
app |> run_app()
