library(readr)
library(tidyverse)
library(plotly)
readxl::read_excel('clipboard')

read.csv(clipboard, header = TRUE)

df_취학률 <- read.table("clipboard") 

df_취학률 <- as.data.frame(t(df_취학률))

df_취학률 <- as.data.frame(apply(df_취학률, 2, as.numeric))

rownames(df_취학률)<- 1980:2021

df_취학률$year <- 1980:2021

margins <- list(t = 50, b = 25, l = 25, r = 25)

df_취학률 |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers+lines', 
            x = ~year, y = ~유치원, 
            name = 'pre-primary') |>
  add_trace(type = 'scatter', mode = 'markers+lines', 
            x = ~year, y = ~초등학교, 
            name = 'primary') |>
  add_trace(type = 'scatter', mode = 'markers+lines', 
            x = ~year, y = ~중학교, 
            name = 'lower-secondary') |>
  add_trace(type = 'scatter', mode = 'markers+lines', 
            x = ~year, y = ~고등학교,
            name = 'upper-secondary') |>
  add_trace(type = 'scatter', mode = 'markers+lines', 
            x = ~year, y = ~고등교육기관,
            name = 'higher education') |>
  layout(title = list(text = 'Enrollment Rate', 
                      size = 20), 
         xaxis = list(title = ''), 
         yaxis = list(title = 'Enrollment Rate', 
                      tickformat = '.0f', 
                      ticksuffix = '%'),
         legend = list(font = list(size = 15)),
         margin = margins)
  
RColorBrewer::brewer.pal(5, 'Blues')

#####################################################

df_18세인구 <- readxl::read_excel("D:/R/data/대입적령인구.xlsx", 
                               sheet = '데이터', col_names = T, 
                               col_types = c('text', 'numeric')) 

df_18세인구 |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~시점, y = ~대입적령인구)
  
unique(covid19_stat$continent)[!is.na(unique(covid19_stat$continent))]
covid19_stat$location

df_sunburst <- rbind(covid19_stat |> 
  filter(iso_code %in% c('OWID_AFR', 'OWID_ASI', 'OWID_EUR', 'OWID_NAM', 'OWID_OCE', 'OWID_SAM')) |>
  select(continent, location, 전체확진자수), 
  covid19_stat |> 
    filter(!is.na(continent)) |>
    group_by(continent) |> top_n(5, wt = 전체확진자수) |>
    select(continent, location, 전체확진자수)
)

df_sunburst_root <- covid19_stat |> 
  filter(!is.na(continent)) |>
  group_by(continent) |> top_n(5, wt = 전체확진자수) |>
  select(continent, location, 전체확진자수)



  plot_ly() |>
  add_trace(type = 'sunburst',  
            labels = pull(df_sunburst[2]), 
            parents = pull(df_sunburst[1]), 
            values = pull(df_sunburst[3]),
            branchvalues = 'total', 
            insidetextorientation='radial', 
            marker = list(colors = RColorBrewer::brewer.pal(5, 'Blues')), 
            count = "branches"
            )

covid19_df |> filter(iso_code == 'AIA') |> View()




