fig <- p_box_add |> 
  ## boxmode를 group으로 설정
  layout(boxmode = "group", 
         title = list(text = '대학 계열별 취업률 분포'), 
         margin = margins) |>
  config(
           toImageButtonOptions = list(
             format = "pdf",
             width = 800,
             height = 600
           )
         )
p_line <- df_covid19_100 |> plot_ly()

p_line |> 
  ## type을 scatter, mode를 lines로 설정한 trace 추가
  add_trace(type = 'scatter', mode = 'lines', 
            ## X축을 date, 
            ## Y축을 people_fully_vaccinated_per_hundred로 매핑
            x = ~date, y = ~people_fully_vaccinated_per_hundred, 
            ## color를 location으로 매핑
            color = ~ location) |>
  config(edits = list(legendPosition = TRUE, legendText = TRUE), 
         displayModeBar = T) |>
  ## 제목, X, Y축 제목, 여백 설정
  layout(title = '최근 100일간 코로나19 백신접종완료자수(인구 100명당)', 
         xaxis = list(title = list(text = NULL)), 
         yaxis = list(title = '인구 100명당 접종자수'), margin = margins)

#install.packages('dash')
#install.packages('dashCoreComponents')
#install.packages('dashHtmlComponents')

config

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
