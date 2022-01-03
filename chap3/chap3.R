library(tidyverse)


df_입학자 |> 
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_line(aes(group = 지역))
vignette('ggplot()')
