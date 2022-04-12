fig <- df_sunburst_cases |>
  plot_ly() |>
  add_trace(type = 'sunburst',  
            labels = ~location, 
            parents = ~continent, 
            values = ~전체확진자수,
            branchvalues = 'total', 
            insidetextorientation='radial', 
            marker = list(colors = RColorBrewer::brewer.pal(5, 'Blues')), 
            textinfo = 'label+percent parent+percent entry', 
            texttemplate = '국가:%{label}<br>전체의 %{percentParent}<br>%{parent}대륙의%{percentEntry}'
  )

fig <- df_sunburst_cases |>
  plot_ly() |>
  add_trace(type = 'treemap',  
            labels = ~location, 
            parents = ~continent, 
            values = ~전체확진자수,
            marker = list(colors = RColorBrewer::brewer.pal(5, 'Blues')), 
            textinfo = 'label+value+percent parent+percent entry'
  )


fig <- plot_ly(
  type = 'scatterpolar',
  fill = 'tonext'
) 
fig <- fig %>%
  add_trace(
    r = c(45, 45, 10, 45, 45, 45),
    theta = c('A','B','C', 'D', 'E', 'A'),
    name = 'Group A'
  ) 
fig <- fig %>%
  add_trace(
    r = c(1.5, 10, 39, 31, 15, 1.5),
    theta = c('A','B','C', 'D', 'E', 'A'),
    name = 'Group B'
  ) 
fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,50)
      )
    )
  )

fig






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

