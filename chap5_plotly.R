covid19_stat <- covid19_df |> group_by(iso_code, continent, location) |>
  summarise(인구수 = max(population, na.rm = T), 인당GDP = max(gdp_per_capita, na.rm = T),
               전체확진자수 = sum(new_cases, na.rm = T),
               전체사망자수 = sum(new_deaths, na.rm = T), 
               십만명당중환자실 = last(icu_patients_per_million),
               재생산지수 = last(reproduction_rate),
               봉쇄지수 = max(stringency_index), 
               전체검사자수 = max(total_tests, na.rm = T), 
               신규검사자수 = sum(new_tests, na.rm = T),
               전체백신접종자수 = max(total_vaccinations, na.rm = T),
               백신접종자완료자수 = max(people_fully_vaccinated, na.rm = T),
               부스터접종자수 = max(total_boosters, na.rm = T),
               인구백명당백신접종완료률 = max(people_fully_vaccinated_per_hundred, na.rm = T),
               인구백명당부스터접종자수 = max(total_boosters_per_hundred, na.rm = T), 
               인구밀도 = max(population_density, na.rm = T)
  ) |> 
  ungroup() |>
  mutate(십만명당사망자수 = round(전체사망자수 / 인구수 *100000, 5),
                 백신접종완료률 = 백신접종자완료자수 / 인구수)





covid19_stat |> 
  filter((iso_code %in% c('KOR', 'OWID_ASI', 'OWID_EUR', 'OWID_OCE', 'OWID_NAM', 'OWID_SAM', 'OWID_AFR')))


  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~인구밀도, y = ~십만명당사망자수, 
            text = ~location, hoverinfo = 'x+y',
            hovertemplate = '접종률:%{x},사망자:%{y}(명/십만명)<br>국가명:%{text}<extra></extra>',
            showlegend = FALSE) |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~인구밀도, 
            y = ~fitted(lm(십만명당사망자수 ~ 인구밀도)), 
            name = '추세선', 
            color = I('darkblue')
  ) |>
  layout(title = '백신 접종률과 사망자수의 상관관계', 
         margin = margins,
         showlegend = TRUE, 
         xaxis = list(tickfont = list(size = 10),
                      title = list(text = '백신접종완료률', 
                                   standoff = 5), 
                      tickformat = ".1f"),
         yaxis = list(title = list(text = '십만명당사망자수'),
                      tickformat = ".0f"),
         annotations = list(x = 0.5 , y = 1.02,
                            text = ~paste0('회귀 방정식 : y = ',
                                           round(lm(십만명당사망자수 ~ 인구밀도)$coefficients[2], 2), 
                                           'x + ', 
                                           round(lm(십만명당사망자수 ~ 인구밀도)$coefficients[1], 2)),
                            showarrow = F, xref='paper',
                            yref='paper', xanchor = 'center')
  )



covid19_df |> filter(location == 'United Kingdom') 

covid19_df |> 
  filter((iso_code %in% c('KOR', 'USA', 'JPN', 'UK', 'FRA'))) |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines+text', 
            x = ~date, y = ~new_deaths_per_million , linetype = ~location)




covid19_df |> 
  filter((iso_code %in% c('KOR', 'USA', 'JPN', 'GBR', 'FRA'))) |>
  ## date에서 연월을 추출한 yearmonth 열을 생성
  mutate(yearmonth = tsibble::yearmonth(date)) |> 
  ## location과 yearmonth로 그룹화
  group_by(location, yearmonth) |>
  ## new_cases의 합계 산출
  summarise(new_deaths = sum(new_deaths), population = max(population)) |>
  mutate(date = as.Date(yearmonth, format = '%Y %m'), 
         million_per_new_deaths = new_deaths / population * 100000) |>
  ungroup() |>
  ## plotly 객체 생성
  plot_ly() |>
add_trace(type = 'scatter', mode = 'lines+text', 
          x = ~date, y = ~million_per_new_deaths , linetype = ~location, connectgaps = T)



fig <-   plot_ly() |>
  add_trace(data = oecd, type = 'scatter', mode = 'markers', 
            x = ~백신접종완료률, y = ~십만명당사망자수, 
            name = 'OECD국가', color = I('darkblue'), 
            marker = list(size = ~(인당GDP/2500)
            ),
            text = ~location, 
            hoverinfo = 'x+y',
            hovertemplate = '접종률:%{x},사망자:%{y}(명/십만명)<br>국가명:%{text}<extra></extra>', 
            showlegend = T) |>
  add_trace(data = nooecd, type = 'scatter', mode = 'markers', 
            x = ~백신접종완료률, y = ~십만명당사망자수, 
            name = '비OECD국가', color = I('#1f77b4'),
            marker = list(size = ~(인당GDP/2500)
            ),
            text = ~location, 
            hoverinfo = 'x+y',
            hovertemplate = '접종률:%{x},사망자:%{y}(명/십만명)<br>국가명:%{text}<extra></extra>', 
            showlegend = T) |>
  add_trace(data = korea, type = 'scatter', mode = 'markers', 
            x = ~백신접종완료률, y = ~십만명당사망자수, 
            name = '한국', color = I('black'),
            marker = list(size = ~(인당GDP/2500), symbol = 100
            ),
            text = ~location, 
            hoverinfo = 'x+y+text',
            hovertemplate = '접종률:%{x},사망자:%{y}(명/십만명)<br>국가명:%{text}<extra></extra>', 
            showlegend = T) |>
  layout(title = '백신 접종률과 사망자수, 1인당 GDP의 상관관계', 
         margin = margins,
         showlegend = TRUE, 
         xaxis = list(tickfont = list(size = 10),
                      title = list(text = '백신접종완료률', 
                                   standoff = 5), 
                      tickformat = ".0%"),
         yaxis = list(title = list(text = '십만명당사망자수'),
                      tickformat = ".0f"))




install.packages('dash')
install.packages('dashCoreComponents')
install.packages('dashHtmlComponents')



fig <- plot_ly() 
# fig <- fig %>% add_trace( ... )
# fig <- fig %>% layout( ... ) 

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      dccGraph(figure=fig) 
    )
  )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)





