library(tidyverse)
library(readxl)
library(showtext)
showtext_auto()

df <- read_excel('C:/R/git/univ_data/학교별합본.xlsx', skip = 4, na = '-', col_names = T, col_types = c(rep('text', 14), rep('numeric', 60)))



df <- df[, -c(10:14)]

df$연도 <- factor(df$연도, ordered = T)

df$시도 <- fct_relevel(df$시도,'전체', '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도) |>
  summarise(전체정원 = sum(입학정원_전체)) 



df.미래 <- read_excel('d:/R/git/univ/미래1.xlsx', na = '-', col_names = F, col_types = c('numeric', 'numeric', 'numeric'))

names(df.미래) <- c('연도', '학생수', '정원')

df.미래$연도 <- as.Date(paste(df.미래$연도, 1, 1, sep = "-"), '%Y-%m-%d')
df$연도 <- as.Date(paste(df$연도, 1, 1, sep = "-"), '%Y-%m-%d')

View(df.미래)


df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도) |>
  summarise(전체입학자 = sum(입학자_전체_계), 전체정원 = sum(입학정원_전체)) |>
  ggplot(aes(x = 연도)) + 
  geom_line(aes(y = 전체정원, group = 1), color = 'dodgerblue') +
  scale_y_continuous(limits = c(250000, 650000)) +
  scale_x_date(date_breaks = "3 years", date_labels = '%y', limits = c(as.Date('2009-06-01', format = '%Y-%m-%d'), as.Date('2038-21-31', format = '%Y-%m-%d'))) +
#  geom_line(data = df.미래 |> filter(lubridate::year(연도) <= 2021), 
#            aes(x = 연도 + lubridate::years(1), y = 학생수, group = 1, color = 학생수)) + 
  geom_line(data = df.미래 |> filter(lubridate::year(연도) >= 2021), 
            aes(x = 연도 + lubridate::years(1), y = 학생수, group = 1, color = 학생수), linetype = 2) + 
  geom_line(data = df.미래 |> filter(is.na(정원) == FALSE), aes(x = 연도, y = 정원, group = 1), color = 'dodgerblue') +
  annotate('rect', xmin = as.Date('2019-06-01', '%Y-%m-%d'), xmax = as.Date('2024-06-01', '%Y-%m-%d'), ymin = 380000, ymax = 580000, alpha = 0.2) +
  annotate('rect', xmin = as.Date('2034-06-01', '%Y-%m-%d'), xmax = as.Date('2038-06-01', '%Y-%m-%d'), ymin = 260000, ymax = 420000, alpha = 0.2) +
  annotate('text', x = as.Date('2022-01-01', '%Y-%m-%d'), y = 600000, label = '1차 충격', vjust = -0.8) + 
  annotate('segment', x = as.Date('2022-01-01', '%Y-%m-%d'), y = 600000, xend = as.Date('2022-01-01', '%Y-%m-%d'), yend = 580000, arrow = arrow(length = unit(0.01, "npc"))) + 
  annotate('text', x = as.Date('2034-01-01', '%Y-%m-%d'), y = 325000, label = '2차 충격', hjust = 1.1) + 
  annotate('segment', x = as.Date('2034-01-01', '%Y-%m-%d'), y = 325000, xend = as.Date('2034-06-01', '%Y-%m-%d'), yend = 325000, arrow = arrow(length = unit(0.01, "npc"))) + 
  geom_text(data = df.미래 |> filter(연도 == min(연도)), 
           aes(x = 연도, y = 학생수, label = '고3 학생수'), hjust = 0.7) + 
  labs(x = NULL, y = NULL) + 
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Remove all grid lines
    panel.grid = element_blank(),
    # But add grid lines for the vertical axis, customizing color and size 
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks on the vertical axis by setting their length to 0
    axis.ticks.length.y = unit(0, "mm"), 
    # But keep tick marks on horizontal axis
    axis.ticks.length.x = unit(2, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only the bottom line of the vertical axis is painted in black
    axis.line.x.bottom = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16), 
    legend.position = 'none'
  ) + 
  geom_text(
    data = data.frame(x = as.Date('2009-06-01'), y = seq(300000, 600000, by = 100000)),
    aes(x, y, label = paste0(y/10000, '만명')),
    hjust = 1, # Align to the right
    vjust = -0.5, # Align to the bottom
    family = "Econ Sans Cnd",
    size = 3, 
    color = 'grey50'
  ) + 
  scale_color_gradient2(low = 'blue', high = 'red', mid = 'white', midpoint = 475000)
  
ggplot(data = df.미래 |> filter(lubridate::year(연도) >= 2021), 
       aes(x = 연도 + lubridate::years(1), y = 학생수, color = 학생수)) +
geom_path(linetype = 'dashed')
  