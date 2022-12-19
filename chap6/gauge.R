f <- list(family="Courier New, monospace",size = 18,color = "black")
fig <- plot_ly()
fig1 <- fig %>% add_pie(data = count(diamonds, cut), labels = ~cut, values = ~n,
                        name = "Cut", domain = list(row = 0, column = 0)) %>%
  layout(annotations=list(text = "Cut",font = f,xref = "paper",yref = "paper",yanchor = "bottom",xanchor = "center",align = "center",x = 0.5,y = 1,showarrow = FALSE))
fig2 <- fig %>% add_pie(data = count(diamonds, color), labels = ~color, values = ~n,
                        name = "Color", domain = list(row = 0, column = 1)) %>%
  layout(annotations=list(text = "Color",font = f,xref = "paper",yref = "paper",yanchor = "bottom",xanchor = "center",align = "center",x = 0.5,y = 1,showarrow = FALSE))
fig3 <- fig %>% add_pie(data = count(diamonds, clarity), labels = ~clarity, values = ~n,
                        name = "Clarity", domain = list(row = 0, column = 2)) %>%
  layout(annotations=list(text = "Clarity",font = f,xref = "paper",yref = "paper",yanchor = "bottom",xanchor = "center",align = "center",x = 0.5,y = 1,showarrow = FALSE))
fig <- fig %>% layout(showlegend = F,grid=list(rows=1, columns=3),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p <- subplot(fig1, fig2, fig3, titleX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE, grid=list(rows=1, columns=3),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p 


number_KOR <- total_deaths_5_nations_by_day |>
  filter(date == max(date), iso_code == 'KOR') |>
  select(total_deaths_per_million) |> pull()


fig1 <- 
  total_deaths_5_nations_by_day |>
  plot_ly() |>
  ## scatter 트레이스 생성
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  ## layout의 제목, 축제목, 여백 속성 설정
  layout(title = list(text = '코로나 19 사망자수 추세', pad = list(b = 5)), 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계', range = c(0, 4000)), 
         margin = margins_R)


fig1 |>
  add_trace(type = 'indicator', mode = 'number', 
            value = number_KOR,  
            title = list(text = '한국 코로나 발생자',
                     font = list(family = '나눔고딕', size = 15)
                     ),
            number = list(font = list(family = '나눔고딕', 
                                      size = 15)),
            domain = list(x = c(0.4, 0.6), y = c(0.85, 0.95))
  )

fig3 <- plot_ly() |>
  add_trace(type = 'indicator', mode = 'number', 
            value = number_KOR,  
            domain = list(x = c(0.35, 0.65), y = c(0, 0.2))
  )

fig4 <- plot_ly() |>
  add_trace(type = 'indicator', mode = 'number', 
            value = number_KOR,  
            domain = list(x = c(0.7, 1), y = c(0, 0.2))
  )


s <- subplot(fig1, fig2, nrows = 2, heights = c(0.7,0.2)) |>
  layout(margin = margins_R)

s|>
  add_trace(type = 'indicator', mode = 'number', 
            value = number_KOR,  
            domain = list(x = c(0.35, 0.65), y = c(0, 0.2))
  )





class(as.Date(lubridate::today()))

class()

lubridate::today()

lubridate::year(lubridate::today())

max_deaths_per_million_by_day <- total_deaths_5_nations_by_day |> group_by(location) |>
  summarise(최대사망자 = max(new_deaths_per_million, na.rm = TRUE))

deaths_per_million_in_lateast <- total_deaths_5_nations_by_day |> group_by(location) |>
  filter(is.na(new_deaths_per_million) == FALSE) |>
  filter(date == max(date)) |>
  select(iso_code, date, new_deaths_per_million)


df_gauge <- left_join(max_deaths_per_million_by_day, deaths_per_million_in_lateast, by = 'location') |> arrange(location)

fig_gauge <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[3, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.82, 0.9)),
            value = pull(df_gauge[3, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[3, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[3, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[3, 2])*1.2*0.5, pull(df_gauge[3, 2])*1.2*0.75), color = "gray")
              ),
              shape = "bullet",
              threshold = list(
                line = list(color = 'black'),
                value = pull(df_gauge[3, 2])), 
              bar = list(color = "darkblue")
            )             
          )

fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[1, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.64, 0.72)),
            value = pull(df_gauge[1, 5]), 
            gauge = list(axis = list(
              shape = "bullet",
              range = list(NULL, pull(df_gauge[1, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[1, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[1, 2])*1.2*0.5, pull(df_gauge[1, 2])*1.2*0.75), color = "gray")
              ),
              shape = "bullet",
              threshold = list(
              line = list(color = 'black'),
              value = pull(df_gauge[1, 2])), 
            bar = list(color = "darkblue")
            )             
  )

fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[2, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.49, 0.57)),
            value = pull(df_gauge[2, 5]), 
            gauge = list(axis = list(
              shape = "bullet",
              range = list(NULL, pull(df_gauge[2, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[2, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[2, 2])*1.2*0.5, pull(df_gauge[2, 2])*1.2*0.75), color = "gray")
              ),
              shape = "bullet",
              threshold = list(
              line = list(color = 'black'),
              value = pull(df_gauge[2, 2])), 
            bar = list(color = "darkblue")
            )             
  )

fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[4, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.34, 0.42)),
            value = pull(df_gauge[4, 5]), 
            gauge = list(axis = list(
              shape = "bullet",
              range = list(NULL, pull(df_gauge[4, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[4, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[4, 2])*1.2*0.5, pull(df_gauge[4, 2])*1.2*0.75), color = "gray")
              ),
              shape = "bullet",
              threshold = list(
              line = list(color = 'black'),
              value = pull(df_gauge[4, 2])), 
            bar = list(color = "darkblue")
            )             
  )

fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[5, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.19, 0.27)),
            value = pull(df_gauge[5, 5]), 
            gauge = list(axis = list(
              shape = "bullet",
              range = list(NULL, pull(df_gauge[5, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[5, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[5, 2])*1.2*0.5, pull(df_gauge[5, 2])*1.2*0.75), color = "gray")
              ),
              shape = "bullet",
              threshold = list(
              line = list(color = 'black'),
              value = pull(df_gauge[5, 2])), 
            bar = list(color = "darkblue")
            )             
  )

fig_gauge |> layout(margin = margins_R, 
                    title = '10만명당 사망자수(공식 발표 최근일)')




최근사망자 <- df_gauge |> arrange(iso_code) |> select(new_deaths_per_million)

최근사망자[1, 1] |> pull()

최대사망자 <- df_gauge |> filter(iso_code == 'KOR') |> select(최대사망자) |>  |> arrange(iso_code)
