library(ggalt)
library(grid)

df_의약 <- df_취업통계 |> filter(대계열 == '의약계열')
df_인문 <- df_취업통계 |> filter(대계열 == '인문계열')

model_lm_의약 <- lm(df_의약$취업자_합계_계 ~ df_의약$졸업자_계)
model_lm_인문 <- lm(df_인문$취업자_합계_계 ~ df_인문$졸업자_계)

glance_의약 <- broom::glance(model_lm_의약)
glance_인문 <- broom::glance(model_lm_인문)

tidy_의약 <- broom::tidy(model_lm_의약)
tidy_인문 <- broom::tidy(model_lm_인문)

equ_의약 <- paste0('의약계열 : y = ', round(tidy_의약$estimate[2], 2), 'x + ', round(tidy_의약$estimate[1], 2), ', R\u00B2', ' = ',round(glance_의약$r.squared, 3))

equ_인문 <- paste0('인문계열 : y = ', round(tidy_인문$estimate[2], 2), 'x + ', round(tidy_인문$estimate[1], 2), ', R\u00B2', ' = ',round(glance_인문$r.squared, 3))

df_취업통계 |> 
  ggplot() +
  ## X축이 졸업자_계, Y축이 취업자_합계_계에 매핑된 geom_point 레이어 생성
  geom_point(aes(x = 졸업자_계, y = 취업자_합계_계), color = 'grey75', alpha = 0.5) + 
  geom_smooth(aes(x = 졸업자_계, y = 취업자_합계_계), color = 'grey75', se = F, method = 'lm') + 
  geom_point(data = df_인문,
             aes(x = 졸업자_계, y = 취업자_합계_계, color = '인문계열'), alpha = 0.5) + 
  geom_smooth(data = df_인문,
              aes(x = 졸업자_계, y = 취업자_합계_계, color = '인문계열'), se = F, method = 'lm') + 
  geom_point(data = df_의약,
             aes(x = 졸업자_계, y = 취업자_합계_계, color = '의약계열'), alpha = 0.5) + 
  geom_smooth(data = df_의약,
             aes(x = 졸업자_계, y = 취업자_합계_계, color = '의약계열'), se = F, method = 'lm') + 
  geom_encircle(data = df_의약, 
                aes(x = 졸업자_계, y = 취업자_합계_계, color='의약계열')) + 
  geom_rug(data = df_의약,
           aes(x = 졸업자_계, y = 취업자_합계_계), col= "steelblue", alpha=0.5) + 
  ## X축과 Y축의 범위를 설정
  labs(title =  '의약계열 졸업현황', x = '졸업자수', y = '취업자수', subtitle = '졸업자수 대비 취업자수', caption = '출처 : 실전에서 바로쓰는 데이터 시각화 in R') +
  scale_x_continuous(breaks = c(100, 300, 500), labels = c(100, 300, 500), limits = c(0, 500)) + 
  scale_y_continuous(breaks = c(100, 300, 500), labels = c(100, 300, 500), limits = c(0, 500)) + 
  scale_color_manual(name = NULL, values = c('의약계열' = '#4169E1', '인문계열' = '#FA8072'), 
                     labels = c(equ_의약, equ_인문)) +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.2,.9), 
        legend.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA), 
        plot.title = element_text(hjust = 0, size = 20, family = 'NanumBarunGothicBold'),
        plot.subtitle = element_text(hjust = 0, vjust = 0.5, size = 12, family = 'NanumBarunGothic'),
        axis.line = element_blank(), 
        plot.background = element_rect(fill = '#FFFFF0'), 
        panel.background = element_rect(fill = '#FFFFF0'), 
        plot.margin = margin(t = 0.075, 0.01, 0.01, 0.01, "npc")
  ) 
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#e5001c", lwd = 4)
)

grid.rect(
  x = 0,
  y = 1,
  width = 0.1,
  height = 0.025,
  just = c("left", "top"),
  gp = gpar(fill = "#e5001c", lwd = 0)
)

grid.text(
  'Source: "Child Labour: Global estimates 2020, trends and the road forward", ILO and UNICEF', 
  x = 0.005, 
  y = 0.06, 
  just = c("left", "bottom"),
  gp = gpar(
    col = "grey50",
    fontsize = 5,
    fontfamily = "Econ Sans Cnd"
  )
)
