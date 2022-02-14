getwd()

library(tidyverse)
library(readxl)
library(showtext)
showtext_auto()


df_재적생 <- read_excel('./chap12/2021_연도별 재적학생수.xlsx', skip = 3, col_name = FALSE, col_types = c(rep('text', 2), rep('numeric', 30)))

df_재적생 <- df_재적생 |> select(1, 2, 3, 5, 9) |> rename('연도' = '...1', '지역' = '...2', '전체재적생' = '...3', '전문대학' = '...5', '대학' = '...9') |> filter(is.na(지역) == FALSE)



df_재적생_전문대 <- df_재적생 |>
  select(1, 2, 3, 4) |>
  mutate(권역 = case_when(
    지역 == '전체' ~ '전체',
    지역 %in% c('서울', '인천', '경기') ~ '수도권',
    TRUE ~ '비수도권')
  ) |>
  group_by(연도, 권역) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  ungroup() |>
  group_by(권역) |>
  mutate(diffrate_재적생 = (전체재적생-lag(전체재적생))/전체재적생, 
         diffrate_전문대 = (전문대학-lag(전문대학))/전문대학) |>
  ungroup() |>
  mutate(권역 = fct_relevel(권역, '전체', '수도권', '비수도권'))


df_재적생_전문대 |> filter(권역 == '비수도권') |>
  ggplot() +
  geom_line(aes(x = 연도, y = diffrate_재적생, group =권역, color = diffrate_재적생), linetype = 'solid') + 
  scale_color_gradient2(low = 'blue', high = 'red', mid = 'grey80', midpoint = 0) +
  scale_x_discrete(breaks = seq(1999, 2021, 3)) + 
  scale_y_continuous(labels = scales::percent)

                     