library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(plotly)

df_spss_monthly <- read_excel('d:/R/data/spss.xlsx', 
                     ## '학과별' 시트의 데이터를 불러오는데,
                     sheet = '월별로그분석',
                     ## 첫번째 행은 열 이름으로 설정
                     col_names = TRUE, 
                     ## 열의 타입을 설정, 처음 9개는 문자형으로 다음 79개는 수치형으로 설정
                     col_types = c('text', rep('numeric', 5), 'text'))

df_spss_daily <- read_excel('d:/R/data/spss.xlsx', 
                              ## '학과별' 시트의 데이터를 불러오는데,
                              sheet = '일별로그분석',
                              ## 첫번째 행은 열 이름으로 설정
                              col_names = TRUE, 
                              ## 열의 타입을 설정, 처음 9개는 문자형으로 다음 79개는 수치형으로 설정
                              col_types = c('date', rep('numeric', 5), 'text'))

df_spss_user <- read_excel('d:/R/data/spss.xlsx', 
                            ## '학과별' 시트의 데이터를 불러오는데,
                            sheet = '사용자별로그분석',
                            ## 첫번째 행은 열 이름으로 설정
                            col_names = TRUE, 
                            ## 열의 타입을 설정, 처음 9개는 문자형으로 다음 79개는 수치형으로 설정
                            col_types = c(rep('text', 3), rep('numeric', 2), 'text'))


df_spss_monthly <- df_spss_monthly |> filter(is.na(년월) != TRUE) |>
  group_by(년월) |>
  summarise(승인자수 = sum(승인자수), 거부자수= sum(거부자수), 
            접속성공횟수 = sum(접속성공횟수), 접속거부횟수 = sum(접속거부횟수), 
            최대동시접속수 = sum(최대동시접속수))

df_spss_daily$일자 <- as.Date(df_spss_daily$일자, format = '%Y-%m-%d')

df_spss_daily <- df_spss_daily |> filter(is.na(일자) != TRUE) |>
  group_by(일자) |>
  summarise(승인자수 = sum(승인자수), 거부자수= sum(거부자수), 
            접속성공횟수 = sum(접속성공횟수), 접속거부횟수 = sum(접속거부횟수), 
            최대동시접속수 = sum(최대동시접속수))

glimpse(df_spss_monthly)

df_spss_monthly$년월 <- as.Date(paste0(df_spss_monthly$년월, ' 01일'), format = '%Y년 %m월 %d일')

df_spss_monthly |> plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', x = ~년월, y = ~승인자수)


df_spss_daily |> plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', x = ~일자, y = ~승인자수, connectgaps=TRUE) |>
  layout(xaxis = list(rangebreaks=list(   ##  rangebreaks 설정
    ## 주말 제거
    list(bounds=list("sat", "mon"))
    )
    ))

df_spss_user

df_spss_user$년월 <- as.Date(paste0(df_spss_user$년월, ' 01일'), format = '%Y년 %m월 %d일')

df_spss_user |> filter(is.na(년월) != TRUE, PC명 != '-', lubridate::year(년월) == '2022') |> 
#  mutate(연도 = lubridate::year(년월)) |>
  group_by(PC명) |>
  summarise(사용시간 = sum(사용시간), 사용횟수= sum(사용횟수)) |> plot_ly() |>
  add_trace(type = 'histogram', x = ~사용시간, nbinsx = 100) |>
  layout(title = '2022년 사용시간별 사용자수 현황', 
         margin = list(t = 50, b = 10))

df_spss_user |> filter(is.na(년월) != TRUE) |> mutate(연도 = lubridate::year(년월)) |>
  group_by(연도, PC명) |>
  summarise(사용시간 = sum(사용시간), 사용횟수= sum(사용횟수)) |> ungroup() |>
  mutate(사용시간_그룹 = cut_width(사용시간, width = 2, center = 1)) |>
  count(연도, 사용시간_그룹) |> 
  pivot_wider(names_from = `사용시간_그룹`, values_from = n) |>
  clipr::write_clip()

df_spss_user |> View()

  library(clipr)
gapminder |> write_clip()
  
cut_number(df_spss_user$사용시간, n = 100)

mean(df_spss_user$사용시간, na.rm = TRUE)

df_spss_user |> filter(is.na(년월) != TRUE, PC명 != '-') |>
  mutate(year = lubridate::year(년월)) |> plot_ly() |>
  add_trace(type = 'box', x = ~year, y = ~사용시간, boxmean = TRUE)
  
df_spss_user |> filter(is.na(년월) != TRUE, PC명 != '-', 사용횟수 != 0) |>
  mutate(year = lubridate::year(년월)) |> plot_ly() |>
  add_trace(type = 'box', x = ~year, y = ~사용횟수, boxmean = TRUE)


df_spss_user |> filter(is.na(년월) != TRUE, PC명 != '-', 사용시간 != 0) |>
  mutate(year = lubridate::year(년월)) |>
  group_by(year) |>
  summarise(min = min(사용시간), q1 = quantile(사용시간, 0.25), median = median(사용시간), 
            mean = mean(사용시간), q3 = quantile(사용시간, 0.75), max = max(사용시간)) |>
  clipr::write_clip()

df_spss_user |> filter(is.na(년월) != TRUE, PC명 != '-', 사용횟수 != 0) |>
  mutate(year = lubridate::year(년월)) |>
  group_by(year) |>
  summarise(min = min(사용횟수), q1 = quantile(사용횟수, 0.25), median = median(사용횟수), 
            mean = mean(사용횟수), q3 = quantile(사용횟수, 0.75), max = max(사용횟수)) |>
  clipr::write_clip()
