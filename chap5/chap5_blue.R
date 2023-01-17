################################################################################
df_covid19 <- read_csv(file = "D:/R/data/Rnpy/owid-covid-data.csv",
                       col_types = cols(date = col_date(format = "%Y-%m-%d")
                       )
)
df_covid19_100 <- df_covid19 |> 
  filter(iso_code %in% c('KOR', 'OWID_ASI', 'OWID_EUR', 'OWID_OCE', 'OWID_NAM', 'OWID_SAM', 'OWID_AFR')) |>
  filter(date >= max(date) - 100) |>
  mutate(location = case_when(
    location == 'South Korea' ~ '한국', 
    location == 'Asia' ~ '아시아', 
    location == 'Europe' ~ '유럽', 
    location == 'Oceania' ~ '오세아니아', 
    location == 'North America' ~ '북미', 
    location == 'South America' ~ '남미', 
    location == 'Africa' ~ '아프리카')) |>
  mutate(location = fct_relevel(location, '한국', '아시아', '유럽', '북미', '남미', '아프리카', '오세아니아')) |>
  arrange(date)

df_covid19_100_wide <- df_covid19_100 |>
  select(date, location, new_cases, people_fully_vaccinated_per_hundred) |>
  rename('date' = 'date', '확진자' = 'new_cases', '백신접종완료자' = 'people_fully_vaccinated_per_hundred') |>
  pivot_wider(id_cols = date, names_from = location, 
              values_from = c('확진자', '백신접종완료자')) |>
  arrange(date)

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

margins_R <- list(t = 50, b = 25, l = 25, r = 25)


df_취업률 <- read_excel('d:/R/data/Rnpy/2021년 학과별 고등교육기관 취업통계.xlsx', 
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

## df_취업률에서 졸업자가 500명 이하인 학과 2000개 샘플링
df_취업률_500 <- df_취업률 |> 
  filter(졸업자_계 < 500) |>
  mutate(id = row_number()) |>
  filter(row_number() %in% seq(from = 1, to = nrow(df_취업률), by = 4))

## 열 이름을 적절히 설정
names(df_취업률_500)[10:12] <- c('졸업자수', '취업률', '취업자수')


################################################################################
#### fig 5-1

total_deaths_5_nations_by_day <- df_covid19 |> 
  filter((iso_code %in% c('KOR', 'USA', 'JPN', 'GBR', 'FRA'))) |>
  filter(!is.na(total_deaths_per_million))

total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 5, by = -1)]
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/vector/5-1'))



################################################################################
################### fig 5-3

total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(7, 'Blues')[3:7]
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', rangeslider = list(visible = T)), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         showlegend = T, margin = margins_R, 
         title='Time Series with Rangeslider',
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/vector/5-3'))



################################################################################
################### fig 5-7
total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(7, 'Blues')[3:7]) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', spikemode = 'across', tickformat = '%Y년 %m월',
                      dtick = 'M3'), 
         yaxis = list(title = '10만명당 사망자수 누계', 
                      spikemode = 'toaxis'), 
         margin = margins_R, 
         hovermode="x") |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/vector/5-7'))



################################################################################
library(tqk)
library(lubridate)
##  주가 코드를 가져옴
code <- code_get()

## 삼성전자 코드값을 가져옴
sse_code <- code |> filter(name == '삼성전자') |>
  select(code) |> pull()

##  삼성전자의 최근 100일 주가를 가져옴
samsung <- tqk_get(sse_code, from=today() - 100, to=today())
samsung |> head()
samsung |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    ##  상승시 선 색상 설정
    increasing = list(line = list(color = 'red')), 
    ##  하락시 선 색상 설정
    decreasing = list(line = list(color = 'blue'))) |> 
  layout(title = "삼성전자 Candlestick Chart",
         ## rangeslider는 안보이도록 설정
         xaxis = list(rangeslider = list(visible = F)), 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/vector/5-10'))

################################################################################
fig1 <- samsung |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    increasing = list(line = list(color = 'red')), 
    decreasing = list(line = list(color = 'blue'))
  ) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         xaxis = list(rangeslider = list(visible = F), 
                      ##  rangebreaks 설정
                      rangebreaks=list(
                        ## 주말 제거
                        list(bounds=list("sat", "mon")), 
                        ## 특정 공휴일 제거
                        list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
                      )),
         yaxis = list(title = '주가'),
         showlegend = FALSE)

fig2 <- samsung %>% plot_ly() |>
  add_trace(type = 'bar', x=~date, y=~volume, type='bar',
            color =I('gray'), showlegend = FALSE) |>
  layout(xaxis = list(rangebreaks=list(   ##  rangebreaks 설정
    ## 주말 제거
    list(bounds=list("sat", "mon")), 
    ## 특정 공휴일 제거
    list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
  )),
  yaxis = list(title = '거래량'))

subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2, shareX = TRUE) |>
  layout(margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/vector/5-12'))


################################################################################
####################### 5-15
if(!require(zoo)) {
  install.packages('zoo')
  library(zoo)
}

samsung_moving <- samsung %>% 
  mutate(MA_5 = zoo::rollmean(x = close, # column to take
                              k = 5, # rolling time period
                              align = "right", #leave values above the top
                              fill = NA), 
         MA_20 = zoo::rollmean(x = close, k = 20,  
                               align = "right", fill = NA), 
         MA_40 = zoo::rollmean(x = close, k = 40, 
                               align = "right", fill = NA)) 

fig1 <- samsung_moving |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    increasing = list(line = list(color = 'red')), 
    decreasing = list(line = list(color = 'blue')),
    showlegend = FALSE) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         xaxis = list(rangeslider = list(visible = F), 
                      rangebreaks=list(
                        list(bounds=list("sat", "mon")), 
                        list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
                      )),
         yaxis = list(title = '주가'))

fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'solid', color = '#08306B'), 
                          x = ~date, y = ~MA_5, name = '5일 이동평균')

fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'dash', color = '#2171B5'), 
                          x = ~date, y = ~MA_20, name = '20일 이동평균')

fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'dot', color = '#6BAED6'), 
                          x = ~date, y = ~MA_40, name = '40일 이동평균')

fig2 <- samsung %>% plot_ly() |>
  add_trace(type = 'bar', x=~date, y=~volume, type='bar',
            color =I('gray'), showlegend = FALSE) |>
  layout(xaxis = list(rangebreaks=list(
    list(bounds=list("sat", "mon")), 
    list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
  )),
  yaxis = list(title = '거래량'))

subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2, shareX = TRUE) |>
  layout(margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/vector/5-14'))



################################################################################
##  퍼널 차트를 위한 데이터 전처리
df_funnel <- df_취업률 |>
  summarise(전체취업자 = sum(`취업자_교외취업자_계` + `취업자_교내취업자_계`), 
            유지취업자_1차 = sum(`1차 유지취업자_계`), 
            유지취업자_2차 = sum(`2차 유지취업자_계`), 
            유지취업자_3차 = sum(`3차 유지취업자_계`), 
            유지취업자_4차 = sum(`4차 유지취업자_계`), 
  ) |>
  pivot_longer(1:5, names_to = '구분', values_to = '유지취업자')

##  funnel 트레이스 생성
df_funnel |>
  plot_ly() |>
  add_trace(type = 'funnel', x = ~유지취업자, y = ~구분, 
            text = ~유지취업자, textinfo = "text+percent initial") |>
  layout(title = '유지취업자 Funnel Chart', 
         yaxis = list(categoryorder = "total descending"), 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/vector/5-16'))


################################################################################
######################## 5-19
df_funnelarea <- df_covid19_100_wide |>
  summarise(아프리카 = sum(확진자_아프리카), 
            아시아 = sum(확진자_아시아), 
            유럽 = sum(확진자_유럽), 
            북미 = sum(확진자_북미), 
            남미 = sum(확진자_남미), 
            오세아니아 = sum(확진자_오세아니아)) |>
  pivot_longer(1:6, names_to = '대륙', values_to = '전체확진자')

df_funnelarea |>
  plot_ly() |>
  add_trace(type = 'funnelarea', text = ~대륙, values = ~전체확진자, 
            textinfo = "text+value+percent") |>
  layout(title = '최근 100일간 대륙별 확진자수 Funnelarea 차트', 
         margin = margins_R, 
         funnelareacolorway = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 4, by = -1)]) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/vector/5-18'))


################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



################################################################################



