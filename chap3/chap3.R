library(tidyverse)
library(showtext)
showtext_auto()
library(tidyverse)
library(readxl)
setwd('./chap3')
df_입학자 <- read_excel('2021_연도별 입학자수.xlsx', 
                     ## 'data' 시트의 데이터를 불러오는데,
                     sheet = 'Sheet0',
                     ## 앞의 10행을 제외하고
                     skip = 3, 
                     ## 첫번째 행은 열 이름을 설정
                     col_names = FALSE, 
                     ## 열의 타입을 설정, 처음 8개는 문자형으로 다음 56개는 수치형으로 설정
                     col_types = c(rep('text', 2), rep('numeric', 30)))
df_입학자 <- df_입학자 |> select(1, 2, 5, 7, 9, 11, 13, 19, 29, 31)

## df_입학자의 열이름을 적절한 이름으로 설정
colnames(df_입학자) <- c('연도', '지역', '전문대학', '교육대학', '일반대학', '방송통신대학', '산업대학', '원격및사이버대학', '석사', '박사')

df_입학자 <- df_입학자 |> filter(!is.na(지역))

df_취업통계 <- read_excel('2020년 학과별 고등교육기관 취업통계.xlsx', 
                      ## '학과별' 시트의 데이터를 불러오는데,
                      sheet = '학과별',
                      ## 앞의 13행을 제외하고
                      skip = 13, 
                      ## 첫번째 행은 열 이름으로 설정
                      col_names = TRUE, 
                      ## 열의 타입을 설정, 처음 9개는 문자형으로 다음 79개는 수치형으로 설정
                      col_types = c(rep('text', 9), rep('numeric', 79)))


df_취업통계 <- df_취업통계 |> select(1:9, ends_with('계'))


df_입학자 |> 
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_line(aes(group = 지역))
vignette('ggplot()')

p_histogram <- df_취업통계 |>
  ggplot()

p_histogram + 
  geom_histogram(aes(x = 취업률_계), color = 'blue', fill = 'red', alpha = 0.5)

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

df_입학자 |># |> filter(지역 == '전체') |>
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_point(aes(shape = 지역))

df_입학자 |> filter(지역 == '전체') |>
  ggplot(aes(x = 연도, y = 전문대학)) +
  geom_point(shape = 15)


##  df_취업통계를 ggplot 객체를 생성하고 p_histogram에 저장
p_histogram <- df_취업통계 |>
  ggplot()

## p_histogram에 geom_histogram 레이어를 생성하는데 x축을 '취업률_계'열로 매핑, binning 옵션을 주지 않았으므로 bins = 30이 기본값으로 설정됨 
p_histogram +
  geom_histogram(aes(x = 취업률_계))

## p_histogram에 geom_histogram 레이어를 생성하는데 x축을 '취업률_계'열로 매핑, bins = 90으로 설정 
p_histogram +
  geom_histogram(aes(x = 취업률_계), bins = 90)

## p_histogram에 geom_histogram 레이어를 생성하는데 x축을 '취업률_계'열로 매핑, binwidth = 10으로 설정 
p_histogram +
  geom_histogram(aes(x = 취업률_계), binwidth = 10)

## p_histogram에 geom_histogram 레이어를 생성하는데 x축을 '취업률_계'열로 매핑, binwidth = 5으로 설정 
p_histogram +
  geom_histogram(aes(x = 취업률_계), binwidth = 5)

p_histogram + 
  geom_histogram(aes(x = 취업률_계), color = 'red', fill = 'red', alpha = 0.2, linetype = 2)








df_입학자 |> 
  ggplot() +
  geom_freqpoly(aes(x = 일반대학), bins = 30, na.rm = TRUE)
p_freqpoly +
  geom_freqpoly(aes(x = 일반대학), binwidth = 2000, color = 'red', linetype = 3)

##  df_입학자에서 연도가 2021인 데이터만 추출하여 ggplot 객체를 생성하고 p_density에 저장
p_density <- df_입학자 |>
  ggplot()

## p_density객체에 geom_density 레이어를 생성하는데 x축을 일반대학으로 매핑
p_density + 
  geom_density(aes(x = 일반대학))

## p_density객체에 geom_density 레이어를 생성하는데 x축을 일반대학으로 매핑, color를 red, fill을 blue, linetype을 2, size를 3으로 설정
p_density + 
  geom_density(aes(x = 일반대학), color = 'blue', fill = 'skyblue', linetype = 2, size = 1, alpha = 0.5)


p_bar <- df_입학자 |>
  ggplot()

p_bar +
  geom_bar(aes(x = 지역))
