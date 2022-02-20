library(ggtext)
library(grid)

theme_set(
  theme_grey()
)

df_total_line <- df_입학자_long |> filter(지역 == '전체', lubridate::year(연도) %in% c(seq(from = 2001, to = 2021, by = 5)), 학교종류 %in% c('전문대학', '일반대학', '석사', '박사')) 

data_labels <- bind_rows(df_total_line |> filter(lubridate::year(연도) == 2001, 학교종류 %in% c('일반대학', '석사', '박사')), df_total_line |> filter(lubridate::year(연도) == 2006, 학교종류 %in% c('전문대학')))

df_total_line |>
  ggplot(aes(x = 연도, y = (입학생수)/1000)) + 
  ## geom_line 레이어를 생성
  geom_line(aes(group = 학교종류, color = 학교종류), size = 2.4) +
  geom_point(aes(fill = 학교종류), size = 5, 
             shape = 21, # Type of point that allows us to have both color (border) and fill.
             color = "white", 
             stroke = 1 # The width of the border, i.e. stroke.
  ) + 
  scale_x_date(
    expand = c(0, 0), # The horizontal axis does not extend to either side
    limits = c(as.Date('2000-01-01'), as.Date('2022-12-01')), 
    breaks = seq(from = as.Date("2001-01-01"), to = as.Date("2021-01-01"),
                 by = "5 years"), 
    labels = lubridate::year(seq(from = as.Date("2001-01-01"), to = as.Date("2021-01-01"),
                                 by = "5 years"))  # Set custom break locations
    # Set custom break locations
    #    labels = c("2008", "12", "16", "20") # And custom labels on those breaks!
  ) + 
  scale_y_continuous(
    limits = c(0, 380),
    expand = c(0, 0)
  ) -> p_line_1

p_line_1 +
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
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)
  ) -> line_ch7_2



p_line_2 + 
  geom_text(data = data_labels, aes(x = 연도, y = (입학생수/1000) + 20, label = 학교종류, color  = 학교종류), show.legend = F) + 
  geom_text(
    data = data.frame(x = as.Date('2022-09-01'), y = seq(0, 300, by = 100)),
    aes(x, y, label = y),
    hjust = 1, # Align to the right
    vjust = -0.5, # Align to the bottom
    family = "Econ Sans Cnd",
    size = 3, 
    color = 'grey50'
  ) + 
  theme(legend.position = 'none') +
  labs(
    title = "**교육과정별 졸업생수(k)**",
  ) + 
  theme(
    # theme_markdown() is provided by ggtext and means the title contains 
    # Markdown that should be parsed as such (the '**' symbols)
    plot.title = element_markdown(
      family = "Econ Sans Cnd", 
      size = 12
    )
  ) -> p_line_3


ggplot(df_total_line) +
  # color = "white" indicates the color of the lines between the areas
  geom_area(aes(x = 연도, y = (입학생수)/1000, group = 학교종류, fill = 학교종류), color = "white") +
  #  scale_fill_manual(values = c('grey', 'brown', 'green', 'blue')) +
  theme(legend.position = "None") + # no legend +
  scale_x_date(
    expand = c(0, 0), # The horizontal axis does not extend to either side
    limits = c(as.Date('2000-01-01'), as.Date('2022-12-01')), 
    breaks = seq(from = as.Date("2001-01-01"), to = as.Date("2021-01-01"),
                 by = "5 years"), 
    labels = lubridate::year(seq(from = as.Date("2001-01-01"), to = as.Date("2021-01-01"),
                                 by = "5 years"))  # Set custom break locations
    # Set custom break locations
    #    labels = c("2008", "12", "16", "20") # And custom labels on those breaks!
  ) + 
  scale_y_continuous(
    limits = c(0, 790),
    expand = c(0, 0)
  )  -> p_area_1

p_area_2 <- p_area_1 + 
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
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)
  ) -> p_area_3

p_area_2 + 
  geom_text(aes(x = as.Date('2011-01-01'), y = 600), label = '전문대학', color = 'white') + 
  geom_text(aes(x = as.Date('2011-01-01'), y = 300), label = '일반대학', color = 'white') + 
  geom_text(aes(x = as.Date('2011-01-01'), y = 80), label = '석사', color = 'white') + 
  geom_text(aes(x = as.Date('2014-01-01'), y = 80), label = '박사', color = 'white') + 
  geom_segment(aes(x = as.Date('2014-01-01'), xend = as.Date('2014-01-01'), y = 12, yend = 60), color = 'white', arrow = arrow(angle = 30, length = unit(0.1, "inches"))) + 
  geom_text(
    data = data.frame(x = as.Date('2022-09-01'), y = seq(0, 800, by = 200)),
    aes(x, y, label = y),
    hjust = 1, # Align to the right
    vjust = -0.5, # Align to the bottom
    family = "Econ Sans Cnd",
    size = 3, 
    color = 'grey50'
  ) + 
  theme(legend.position = 'none') + 
  labs(
    title = "고등교육기관 졸업생 수(k)",
  ) + 
  theme(
    plot.title = element_markdown(
      family = "Econ Sans Cnd", 
      size = 12
    )
  ) -> p_area_3


plt1 <- p_line_3 + theme(plot.margin = margin(0, 0.05, 0, 0, "npc"))
plt2 <- p_area_3 + theme(plot.margin = margin(0, 0, 0.05, 0, "npc"))
plt <- plt1 | plt2

title_theme <- theme(
  plot.title = element_text(
    hjust = 0.02,
    size = 15,
    margin = margin(0.8, 0, 0.3, 0, "npc")
  ),
  plot.subtitle = element_text(
    hjust = 0.02,
    size = 12,
    margin = margin(0.4, 0, 0.5, 0, "npc")
  )
)

plt <- plt + plot_annotation(
  title = "전문대학의 위기",
  subtitle = "전문대학 학생수 감소 추세",
  theme = title_theme
) +
  theme(
    plot.margin = margin(0.075, 0, 0.1, 0, "npc"),
  )

plt

# Add line on top of the chart
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#e5001c", lwd = 4)
)

# Add rectangle on top-left
# lwd = 0 means the rectangle does not have an outer line
# 'just' gives the horizontal and vertical justification
grid.rect(
  x = 0,
  y = 1,
  width = 0.05,
  height = 0.025,
  just = c("left", "top"),
  gp = gpar(fill = "#e5001c", col = "#e5001c", lwd = 0)
)
# Add first caption
grid.text(
  '출처: 실전에서 바로쓰는 데이터 시각화 in R', 
  x = 0.005, 
  y = 0.06, 
  just = c("left", "bottom"),
  gp = gpar(
    col = "grey50",
    fontsize = 10,
    fontfamily = "Econ Sans Cnd"
  )
)

# Add third caption
grid.text(
  "참조 : https://www.r-graph-gallery.com/", 
  x = 0.995, 
  y = 0.06, 
  just = c("right", "bottom"),
  gp = gpar(
    col = "grey50",
    fontsize = 10,
    fontfamily = "Econ Sans Cnd"
  )
)
