library(tidyverse)
library(patchwork)

df_입학자_연도_전문대 <- df_입학자 |> filter(지역 == '전체') |> group_by(연도) |>
  mutate(전문대학 = 전문대학, 그외= (교육대학 + 일반대학 + 방송통신대학 + 산업대학 + 원격및사이버대학), 
         rate_전문대학 = 전문대학 / (전문대학 + 그외))


df_입학자_연도지역_전문대 <- df_입학자 |> filter(지역 != '전체') |> 
  mutate(권역 = case_when(
    지역 %in% c('서울', '인천', '경기') ~ '수도권', 
    지역 %in% c('충북','충남', '대전', '세종') ~ '충청권', 
    지역 %in% c('강원') ~ '강원권', 
    지역 %in% c('전북', '전남', '광주') ~ '전라권', 
    지역 %in% c('경북', '경남', '부산', '대구', '울산', '제주') ~ '경상권'
    ) 
  ) |>
  mutate(권역 = fct_relevel(권역, '수도권', '강원권', '충청권', '전라권', '경상권')) |>
  select(-지역) |>
  group_by(연도, 권역) |>
  summarise_all(sum) |>
  arrange(연도, 권역)

p_dot_전문대 <- df_입학자_연도_전문대 |>
  ggplot() +
  geom_point(aes(x = 연도, y = rate_전문대학), size = 5, color = 'skyblue') +
  geom_text(aes(x = 연도, y = rate_전문대학, label = round(rate_전문대학*100, 1)), size = 2, color = 'white') +
  scale_x_discrete(limits = rev) + 
  coord_flip() + 
  labs(x = NULL)

p_col_전문대 <- df_입학자_연도지역_전문대 |>
  ggplot() +
  geom_col(aes(x = 연도, y = 전문대학, fill = 권역), position = 'dodge') +
  scale_x_discrete(limits = rev) + 
  scale_fill_discrete(limits = rev) + 
  coord_flip() + 
  labs(x = NULL) + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = 'none'
        )

p_dot_전문대 + p_col_전문대



df_입학자 |> filter(지역 != '전체', 연도 == 2021) |>
  ggplot() +
  geom_col(aes(x = 지역, y = 전문대학, fill = 지역))


df_입학자_연도지역_전문대 |>
  ggplot() +
  geom_col(aes(x = 연도, y = 전문대학, fill = 권역), position = 'dodge') + 
  lims(y = c(-50000, 120000)) + 
  coord_polar()
