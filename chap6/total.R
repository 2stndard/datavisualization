
font_add('NanumBarunGothicBold', 'c:/windows/fonts/NanumBarunGothicBold.ttf')

df_전체_요약 <- df_취업통계 |>
  summarise(mean = mean(취업률_계), median = median(취업률_계), n = n())

subtitle <- paste0('전체 사례수 : ', scales::comma(df_전체_요약$n, accuracy = 1))

df_계열_요약 <- df_취업통계 |>
  group_by(대계열) |>
  summarise(median = median(취업률_계), n = n())

labels <- paste0('<span style= "font-family: NanumBarunGothicBold;font-size: 12pt" >', df_계열_요약$대계열, '</span> <br>', '중앙값 : ', scales::percent(df_요약$median, scale = 1), '<br>', '사례수 : ', scales::comma(df_계열_요약$n, accuracy = 1))

labels_name <- pull(df_계열_요약, 대계열)

setNames(labels, labels_name)

df_취업통계 |>
  ggplot(aes(x = 대계열, y = 취업률_계)) +
  geom_jitter(aes(color = 대계열), width = 0.1, alpha = 0.1, show.legend = F) +
  geom_hline(yintercept = df_전체_요약$median, color = 'blue', linetype = 3) + 
  geom_hline(yintercept = df_전체_요약$mean, color = 'red', linetype = 2) + 
  geom_boxplot(fill = NA, width = 0.4) +
  geom_violin(fill = NA, width = 0.4) +
  geom_point(aes(x = 대계열, y = 취업률_계), stat = 'summary', fun.y = 'mean', color = 'tomato3', size = 4) +
  geom_label(aes(x = 대계열, y = 취업률_계, label = paste0('평균 : ', round(..y.., 1))), stat = 'summary', fun.y = 'mean', color = 'tomato3', size = 4, nudge_y = -7) + 
  labs(title = '대계열 학과별 취업률 분포', x = NULL, y = '취업률', subtitle = subtitle) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5)), labels = setNames(labels, labels_name)) +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.2,.9), 
        legend.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA), 
        plot.title = element_text(size = 20, family = 'NanumBarunGothicBold'),
        plot.subtitle = element_text(vjust = 0.5, size = 12, family = 'NanumBarunGothic'),
        axis.line = element_blank(), 
        plot.background = element_rect(fill = 'lemonchiffon'), 
        panel.background = element_rect(fill = 'lemonchiffon'), 
        axis.text.x = ggtext::element_markdown()
  )

  
  
  
