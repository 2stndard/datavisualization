library(ggtext)
font_add('NanumBarunGothicBold', 'c:/windows/fonts/NanumBarunGothicBold.ttf')

font_add('NanumBarunGothic', 'c:/windows/fonts/NanumBarunGothic.ttf')

df_전체_요약 <- df_취업통계 |>
  summarise(mean = mean(취업률_계), median = median(취업률_계), n = n())

subtitle <- paste0('전체 사례수 : ', scales::comma(df_전체_요약$n, accuracy = 1))

df_계열_요약 <- df_취업통계 |>
  group_by(대계열) |>
  summarise(median = median(취업률_계), n = n())

labels <- paste0('<span style= "font-family: NanumBarunGothicBold;font-size: 12pt" >', df_계열_요약$대계열, '</span> <br>', '중앙값 : ', scales::percent(df_계열_요약$median, scale = 1), '<br>', '사례수 : ', scales::comma(df_계열_요약$n, accuracy = 1))

labels_name <- pull(df_계열_요약, 대계열)

title <- "<b><span style= 'font-family: NanumBarunGothicBold'>대학의 계열별 학과 취업률 분포</b>"

subtitle <- "<span style = 'font-size:10pt'>전체 대학의 취업률 평균은 11.11%인데 <span style = 'color:red;'>의약계열</span>의 평균이 <span style = 'color:red;'> 75.8%</span> 중앙값이 <span style = 'color:red;'> 81.4%</span>로 가장 높게 나타나고 전체적인 분포도 높게 분포함.</span>"

df_취업통계$대계열 = reorder(df_취업통계$대계열, df_취업통계$취업률_계, mean)

df_취업통계 |>
  ggplot(aes(x = 대계열, y = 취업률_계)) +
  geom_jitter(aes(color = 대계열), width = 0.1, alpha = 0.1, show.legend = F) +
  geom_boxplot(fill = NA, width = 0.4) +
  geom_violin(fill = NA, width = 0.4) +
  geom_point(aes(x = 대계열, y = 취업률_계), stat = 'summary', fun.y = 'mean', color = 'tomato3', size = 4) +
  geom_label(aes(x = 대계열, y = 취업률_계, label = paste0('평균 : ', round(..y.., 1))), stat = 'summary', fun.y = 'mean', color = 'tomato3', size = 4, nudge_y = -7) + 
  labs(title = title, x =  NULL, y =  NULL, subtitle = subtitle) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5)), labels = setNames(labels, labels_name), limits = rev) +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.2,.9), 
        legend.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA), 
        plot.title = element_text(size = rel(5)),
        plot.subtitle = element_textbox_simple(width = unit(0.95, "npc"), size = 13,
                                        lineheight = 1,
                                        padding = margin(5.5, 0.5, 5.5, 5.5),
                                        margin = margin(10, 0, 5.5, 0),
                                        halign = 0.5, 
                                        box.color = 'red',
                                        r = grid::unit(8, "pt"),
                                        linetype = 1),
        axis.line = element_blank(), 
        plot.background = element_rect(fill = 'lemonchiffon'), 
        panel.background = element_rect(fill = 'lemonchiffon'), 
        axis.text.x = element_markdown(), 
        plot.margin = margin(0.025, 0, 0.075, 0, "npc"),
  )


grid.text(
  '출처: 실전에서 바로쓰는 데이터 시각화 in R', 
  x = 0.005, 
  y = 0.03, 
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
  y = 0.03, 
  just = c("right", "bottom"),
  gp = gpar(
    col = "grey50",
    fontsize = 10,
    fontfamily = "Econ Sans Cnd"
  )
)
