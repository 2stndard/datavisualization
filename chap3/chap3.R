library(tidyverse)


df_입학자 |> 
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_line(aes(group = 지역))
vignette('ggplot()')

p_histogram + 
  geom_histogram(aes(x = 교육대학), color = 'blue', fill = 'red', alpha = 0.5)

df_입학자 |> ggplot(aes(x = 연도, y = 전문대학)) +
  geom_point(aes(color = 지역))

df_입학자 |> ggplot(aes(x = 연도, y = 전문대학)) +
  geom_point(color = 'red')


df_입학자 |> ggplot(aes(x = 연도, y = 전문대학)) +
  geom_point(aes(color = 지역)) + 
  scale_color_manual(values = c('서울' = 'red', '부산' = 'blue', '대구' = 'blue', '인천' = 'red', '광주' = 'blue', '대전' = 'blue', '울산' = 'blue', '세종' = 'blue', '경기' = 'red', '강원' = 'blue', '충북' = 'blue', '충남' = 'blue', '전북' = 'blue', '전남' = 'blue', '경북' = 'blue', '경남' = 'blue', '제주' = 'blue'))

colors() |> head(10)

rgb(red)

df_입학자 |> filter(지역 == '전체') |>
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_col(color = '#ff0000')

df_입학자 |> filter(지역 == '전체') |>
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_col(fill = '#ff0000')


df_입학자 |> filter(지역 == '전체') |>
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_col(alpha = 0.3)

ggplot() +
  geom_segment(aes(x = 10, xend = 100, y = 1, yend = 1), linetype = 0) +
  geom_segment(aes(x = 10, xend = 100, y = 2, yend = 2), linetype = 1) +
  geom_segment(aes(x = 10, xend = 100, y = 3, yend = 3), linetype = 2) +
  geom_segment(aes(x = 10, xend = 100, y = 4, yend = 4), linetype = 3) +
  geom_segment(aes(x = 10, xend = 100, y = 5, yend = 5), linetype = 4) +
  geom_segment(aes(x = 10, xend = 100, y = 6, yend = 6), linetype = 5) +
  geom_segment(aes(x = 10, xend = 100, y = 7, yend = 7), linetype = 6) +
  geom_text(aes(x = 0, y = 1), label = 'blank, 0') +
  geom_text(aes(x = 0, y = 2), label = 'solid, 1') +
  geom_text(aes(x = 0, y = 3), label = 'dashed, 2') +
  geom_text(aes(x = 0, y = 4), label = 'dotted, 3') +
  geom_text(aes(x = 0, y = 5), label = 'dotdash, 4') +
  geom_text(aes(x = 0, y = 6), label = 'longdash, 5') +
  geom_text(aes(x = 0, y = 7), label = 'twodash, 6') +
  theme_void()

df_입학자 |> filter(지역 == '전체') |>
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_line(aes(group = 1), linetype = 'dashed')

df_입학자 |> filter(지역 == '전체') |>
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_line(aes(group = 1), size = 3)

df_입학자 |> filter(지역 == '전체') |>
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_point(size = 3)

shape()

shapes <- data.frame(
  shape_name = c('square open, 0', 
            'circle open, 1', 
            'triangle open, 2', 
            'plus, 3', 
            'cross, 4', 
            'diamond open, 5', 
            'triangle down open, 6', 
            'square cross, 7', 
            'astrisk, 8', 
            'diamond plus, 9',
            'circle plus, 10', 
            'triangle up and down,11', 
            'square plus, 12', 
            'circle cross, 13', 
            'triangle square, 14', 
            'square, 15', 
            'circle, 16', 
            'triangle, 17', 
            'diamond, 18', 
            'circle small, 19', 
            'bullet, 20', 
            'circle fill, 21', 
            'square fill, 22', 
            'diamond fill, 23', 
            'triangle fill, 24'),
  shape = 0:24, 
  x = 0:24 %/% 5,
  y = -(0:24 %% 5)
)
ggplot(shapes, aes(x, y)) + 
  geom_point(aes(shape = shape), size = 5, fill = "red") +
  geom_text(aes(label = shape_name), hjust = 0.5, vjust = 3) +
  scale_shape_identity() +
  lims(x = c(-0.5, 4.55), y = c(-4.5, 0)) +
  theme_void()
