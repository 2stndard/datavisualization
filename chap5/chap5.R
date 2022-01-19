library(tidyverse)
library(patchwork)
library(showtext)
showtext_auto()

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

library(readxl)
df_nation <- read_xlsx('d:/R/git/datavisualization/chap5/연도별 유학국가별 유학생수.xlsx', sheet = 'Sheet0', skip = 2, col_types = c('numeric', 'text', rep('numeric', 25)), col_names = TRUE)

df_nation <- df_nation |>
  filter(!is.na(학년도), 학제 == '소계') |>
  select(!contains(c('계', '학제', '기타', '미확인', '그외동남아', '남미'))) |>
  gather('국가명', '유학생수', -'학년도')

df_nation_top10 <- df_nation |>
  group_by(국가명) |>
  summarise(sum = sum(유학생수)) |>
  arrange(desc(sum)) |>
  top_n(10)

df_nation_top10


df_nation_top10 |>
  ggplot(aes(x = 국가명, y = sum)) +
  geom_col(fill = 'dark blue') +
  geom_text(aes(x = 국가명, y = sum, label = sum), vjust = -0.5) + 
  theme_minimal() + 
  labs(title = '국가별 유학생수 Top 10', y = '유학생수')


df_nation_top10$국가명 <- fct_reorder(df_nation_top10$국가명, desc(df_nation_top10$sum))

p_nation_top10 <- df_nation_top10 |>
  ggplot(aes(x = 국가명, y = sum)) +
  geom_col(fill = 'dark blue') +
  geom_text(aes(x = 국가명, y = sum, label = sum), vjust = -0.5) + 
  theme_minimal() + 
  labs(title = '국가별 유학생수 Top 10', y = '유학생수')


flag_usa <- 'd:/R/git/datavisualization/chap5/usa.png'
flag_canada <- 'd:/R/git/datavisualization/chap5/can.png'
flag_china <- 'd:/R/git/datavisualization/chap5/chi.png'
flag_phi <- 'd:/R/git/datavisualization/chap5/phi.png'
flag_nz <- 'd:/R/git/datavisualization/chap5/nz.png'
flag_aus <- 'd:/R/git/datavisualization/chap5/aus.png'
flag_jap <- 'd:/R/git/datavisualization/chap5/jap.png'
flag_eng <- 'd:/R/git/datavisualization/chap5/eng.png'
flag_mal <- 'd:/R/git/datavisualization/chap5/mal.png'
flag_ger <- 'd:/R/git/datavisualization/chap5/ger.png'

flags <- data.frame(nations = c('미국', '캐나다', '중국','필리핀', '뉴질랜드', '호주', '일본', '영국', '말레이시아', '독일'), flag_path = c(flag_usa, flag_canada, flag_china, flag_phi, flag_nz, flag_aus, flag_jap, flag_eng, flag_mal, flag_ger))

labels <- setNames(
  paste0("<img src='", flags$flag_path, "' width='30'  height = '20'> <br> ", flags$nations),  flags$nations)


p_nation_top10_1 <- p_nation_top10 +
  scale_x_discrete(labels = labels)

p_nation_top10_1


if(!require(ggtext)) {
  install.packages('ggtext')
  library(ggtext)
}
library(ggtext)

p_nation_top10_1 +
  theme(axis.text.x = ggtext::element_markdown())

install.packages('Rcpp')
