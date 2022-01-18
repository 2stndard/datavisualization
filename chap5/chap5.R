library(tidyverse)
library(patchwork)

df_입학자_long_total <- df_입학자_long |>
  filter(지역 == '전체')

df_입학자_long_total |>
  ggplot(aes(x = 연도, y = 입학생수)) +
  geom_col()

df_입학자_long_total |>
  ggplot(aes(x = 연도, y = 입학생수)) +
  geom_col(aes(fill = 학교종류))

df_입학자_long_total |>
  filter(학교종류 %in% c('전문대학', '일반대학', '석사', '박사')) |>
  mutate(학교종류 = fct_relevel(학교종류, '전문대학', '일반대학', '석사', '박사')) |>
  ggplot(aes(x = 연도, y = 입학생수)) +
  geom_col(aes(fill = 학교종류))

p_col_fill1 <- df_입학자_long_total |>
  filter(학교종류 %in% c('전문대학', '일반대학', '석사', '박사')) |>
  mutate(학교종류 = fct_relevel(학교종류, '전문대학', '일반대학', '석사', '박사')) |>
  ggplot(aes(x = 연도, y = 입학생수)) +
  geom_col(aes(fill = 학교종류), position = 'fill')

  
p_col_fill2 <-  df_입학자_long_total |>
  filter(학교종류 %in% c('전문대학', '일반대학', '석사', '박사')) |>
  mutate(학교종류 = fct_relevel(학교종류, '전문대학', '일반대학', '석사', '박사')) |>
  ggplot(aes(x = 연도, y = 입학생수)) +
  geom_col(aes(fill = 학교종류), position = 'stack')
  
p_col_fill3 <-  df_입학자_long_total |>
  filter(학교종류 %in% c('전문대학', '일반대학', '석사', '박사')) |>
  mutate(학교종류 = fct_relevel(학교종류, '전문대학', '일반대학', '석사', '박사')) |>
  ggplot(aes(x = 연도, y = 입학생수)) +
  geom_col(aes(fill = 학교종류), position = 'dodge')  

(p_col_fill1 / p_col_fill2 / p_col_fill3) + 
  plot_layout(guides = 'collect')

df_입학자_long_total |>
  ggplot(aes(x = 연도, y = 입학생수)) +
  geom_col(aes(fill = 학교종류), position = 'fill')


p_col <- df_입학자_long_total |>
  filter(학교종류 %in% c('전문대학', '일반대학', '석사', '박사')) |>
  ggplot(aes(x = 연도, y = 입학생수, fill = 학교종류))

p_col + geom_col(fill = 'skyblue')

p_col2 <- df_입학자_long_total |>
  filter(학교종류 %in% c('전문대학', '일반대학', '석사', '박사')) |>
  mutate(학교종류 = fct_relevel(학교종류, '전문대학', '일반대학', '석사', '박사')) |>
  ggplot() +
  geom_col(aes(x = 연도, y = 입학생수, fill = 학교종류))


p_col2 + 
  geom_text(aes(x = 연도, y = 입학생수, label = 입학생수, fill = 학교종류), position = position_stack(vjust = 0.5), size = 3)

+
  stat_summary(aes(x = 연도, y = 입학생수, label = stat(y)), fun = 'sum', geom = 'text', color = 'red', vjust = -0.5,  inherit.aes = FALSE)



p_col2 +   
  geom_text(aes(label = ..sum..), stat = 'sum', size = 3)

stat_summary()
p_col2 + 
  geom_col(position = 'dodge')



