library(tidyverse)
gender <- c("M", "F", "F", "M", "M", "M")
ages <- c(20, 21, 20, 23, 23, 25)
name <- c('이몽룡', '성춘향', '향단이', '방자', '변학도', '이방')
df_춘향전 <- data.frame(name, gender, ages)
df_춘향전

df_춘향전$gender <- factor(df_춘향전$gender)
str(df_춘향전)

data <- c("10대","30대이상","20대","20대","10대","30대이상","20대","30대이상","10대")

data_level <- factor(data, level = c('10대', '20대', '30대이상'))

data_level_unique <- factor(data, level = unique(data))


data_label <- factor(data, label = c('10s', '20s', '30 or older'))

data

df_춘향전 |> nrow()
df_춘향전 %>% mean(.$ages)

df_춘향전[, 1]
df_춘향전 %>% .[, 1]

aggregate(ages ~ gender, df_춘향전, mean)
df_춘향전 %>% aggregate(., ages ~ gender, ., mean)
aggregate(df_춘향전, ages ~ gender, df_춘향전, mean)
df_춘향전 |> aggregate(ages ~ gender, ., mean)

df_춘향전$ages %>% mean()
df_춘향전$ages %>% mean
df_춘향전$ages %>% mean(.)
df_춘향전 %>% mean(.$ages)


if(!require(readxl)) {
  install.packages('readxl')
  library(readxl)
}
setwd('C:/R/git/datavisualization/chap2')
## `read_excel()`을 사용하여 엑셀파일을 읽어옴
df <- read_excel('주요-06 (유초)지역규모별 개황(1999-2021)_211214y.xlsx', 
                 ## 'data' 시트의 데이터를 불러오는데,
                 sheet = 'data',
                 ## 앞의 10행을 제외하고
                 skip = 10, 
                 ## 첫번째 행은 열 이름을 설정
                 col_names = FALSE, 
                 ## 열의 타입을 설정, 처음 8개는 문자형으로 다음 56개는 수치형으로 설정
                 col_types = c(rep('text', 3), rep('numeric', 10)))


df

as_tibble(df)

vignette('tibble')

getwd()

as_tibble(df_춘향전)

str(df_춘향전)

glimpse(df_춘향전)

View(df_입학자)

df_입학자 <- read_excel('./chap2/2021_연도별 입학자수.xlsx', 
                 ## 'data' 시트의 데이터를 불러오는데,
                 sheet = 'Sheet0',
                 ## 앞의 10행을 제외하고
                 skip = 3, 
                 ## 첫번째 행은 열 이름을 설정
                 col_names = FALSE, 
                 ## 열의 타입을 설정, 처음 8개는 문자형으로 다음 56개는 수치형으로 설정
                 col_types = c(rep('text', 2), rep('numeric', 30)))

df_입학자 <- df_입학자 |> filter(!is.na(지역))

df_입학자 <- df_입학자 |> select(1, 2, 5, 7, 9, 11, 13, 19, 29, 31)

## df_입학자의 열이름을 적절한 이름으로 설정
colnames(df_입학자) <- c('연도', '지역', '전문대학', '교육대학', '일반대학', '방송통신대학', '산업대학', '원격및사이버대학', '석사', '박사')

## df_입학자에서 연도가 2021년인 관측치(행) 만 추출
df_입학자 |> filter(연도 == '2021')

## df_입학자에서 연도가 2021년이고 지역이 서울인 관측치(행) 만 추출
df_입학자 |> filter(연도 == '2021', 지역 == '서울')

## df_입학자에서 연도가 2021년이거나 연도가 2019인 관측치(행) 만 추출
df_입학자 |> filter(연도 == '2021' | 연도 == '2019')

df_입학자 |> filter(연도 %in% c('2017', '2021'))


## df_입학자에서 연도가 2021년이면서 전문대학 입학생수가 100000명 초과인 관측치(행) 만 추출
df_입학자 |> filter(연도 == '2021' ,전문대학 > 10000)

## df_입학자에서 연도열에 포함된 값을 추출
df_입학자 |> distinct(연도)

## df_입학자에서 지역열에 포함된 값을 추출
df_입학자 |> distinct(지역)

df_입학자 |> slice(3:6)

df_입학자 |> slice(c(3, 5, 9))


df_입학자 |> top_n(3)

df_입학자 |> top_n(3, 원격및사이버대학)

df_입학자 |> filter(연도 == '2021') |> arrange(일반대학)


df_입학자 |> filter(연도 == '2021') |> select(1:3)

df_입학자 |> filter(연도 == '2021') |> select(일반대학, 석사)

df_입학자 |> filter(연도 == '2021') |> select(contains('사'))

df_입학자 |> filter(연도 == '2021') |> select(ends_with('사'))

df_입학자 |> mutate(합계 = 전문대학 + 교육대학 + 일반대학 + 방송통신대학 + 산업대학 + 원격및사이버대학 + 석사 + 박사)

str(df_입학자 |> group_by(연도))

str(df_입학자)

df_입학자 |> group_by(연도, 지역)

df_입학자 |>  str()

df_입학자 |> group_by(지역) |> str()

df_입학자 |> group_by(지역) |> ungroup() |> str()


View(df_입학자)

df_입학자 |> mutate(합계 = 전문대학 + 교육대학 + 일반대학 + 방송통신대학 + 산업대학 + 원격및사이버대학 + 석사 + 박사)

df_입학자 |> mutate(전문대학비율 = 전문대학 / (전문대학 + 교육대학 + 일반대학 + 방송통신대학 + 산업대학 + 원격및사이버대학 + 석사 + 박사))

df_입학자 |> transmute(합계 = 전문대학 + 교육대학 + 일반대학 + 방송통신대학 + 산업대학 + 원격및사이버대학 + 석사 + 박사)

df_입학자 |> transmute(연도, 지역, 전문대학비율 = 전문대학 / (전문대학 + 교육대학 + 일반대학 + 방송통신대학 + 산업대학 + 원격및사이버대학 + 석사 + 박사))

df_입학자 |> summarise(전문대학합계 = sum(전문대학), 전문대학평균 = mean(전문대학), 전문대학최고 = max(전문대학), 전문대학최저 = min(전문대학))


df_입학자 |> group_by(연도) |> summarise(전문대학합계 = sum(전문대학), 전문대학평균 = mean(전문대학), 전문대학최고 = max(전문대학), 전문대학최저 = min(전문대학))

df_입학자 |> group_by(지역) |> summarise(일반대학합계 = sum(일반대학), 일반대학평균 = mean(일반대학), 일반대학최고 = max(일반대학), 일반대학최저 = min(일반대학))

for(i in 1999:2021) {
  print(df_입학자 |> filter(연도 == i) |> transmute(연도, 지역, 일반대학비율 = 일반대학/sum(일반대학)))
}

df_입학자 |> filter(연도 == 1999, 지역 != '전체') |> transmute(연도, 지역, 일반대학비율 = 일반대학/sum(일반대학))


df_입학자 |> filter(지역 != '전체') |> group_by(연도) |> transmute(연도, 지역, 일반대학합계 = sum(일반대학), 일반대학비율 = 일반대학/일반대학합계) |> filter(연도 == 1999)

df_reshape <- data.frame(A = c(1, 1, 1, 2, 2), B = c('가', '가', '나', '다', '다'), C = c(3, 5, 7, 4, 2))
gather(df_reshape, key, value, 2:3)

df_입학자 |> pivot_longer(c('전문대학', '교육대학', '일반대학', '방송통신대학', '산업대학', '원격및사이버대학', '석사', '박사'), names_to = '학교종류', values_to = '입학생수')

df_입학자 |> pivot_longer(3:10, names_to = '학교종류', values_to = '입학생수')

df_입학자 |> pivot_longer(c(3, 4, 5, 6, 7, 8, 9, 10), names_to = '학교종류', values_to = '입학생수')

df_입학자_long <- df_입학자 |> pivot_longer(3:10, names_to = '학교종류', values_to = '입학생수')

df_입학자_long |> pivot_wider(names_from = '학교종류', values_from = '입학생수')


df_join1 <- data.frame(A = c(1, 2, 3), B = c('가', '가', '나'))

df_join2 <- data.frame(A = c(1, 2, 4), C = c('A', 'B', 'B'))

left_join(df_join1, df_join2, by = 'A')

right_join(df_join1, df_join2, by = 'A')

inner_join(df_join1, df_join2, by = 'A')

full_join(df_join1, df_join2, by = 'A')

df_bind1 <- data.frame(A = c(1, 2, 3), B = c('가', '가', '나'))

df_bind2 <- data.frame(A = c(1, 2, 4), C = c('A', 'B', 'B'))

bind_cols(df_bind1, df_bind2)

bind_rows(df_bind1, df_bind2)
