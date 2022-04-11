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

