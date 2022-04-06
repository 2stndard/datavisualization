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

install.packages('dash')
install.packages('dashCoreComponents')
install.packages('dashHtmlComponents')

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
