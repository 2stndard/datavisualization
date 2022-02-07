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

df_입학자_long

df_입학자 |> filter(지역 != '전체', 연도 == 2021) |>
  ggplot() +
  geom_col(aes(x = 지역, y = 전문대학, fill = 지역))


df_입학자_연도지역_전문대 |>
  ggplot() +
  geom_col(aes(x = 연도, y = 전문대학, fill = 권역), position = 'dodge') + 
  lims(y = c(-50000, 120000)) + 
  coord_polar()



df_취업통계_계열별 <- df_취업통계 |> 
  group_by(과정구분, 대계열) |>  
  summarise(졸업자 = sum(졸업자_계), 
               취업자 = sum(취업자_합계_계), 
               교외취업자 = sum(취업자_교외취업자_계), 
               교내취업자 = sum(취업자_교내취업자_계), 
               해외취업자 = sum(취업자_해외취업자_계), 
               농림어업종사자 = sum(취업자_농림어업종사자_계), 
               개인창작활동종사자 = sum(취업자_개인창작활동종사자_계), 
               일인창사업자 = sum(`취업자_1인창(사)업자_계`), 
               프리랜서 = sum(취업자_프리랜서_계), 
               진학자 = sum(진학자_계), 
               입대자 = sum(입대자),
               취업불가능자 = sum(취업불가능자_계), 
               외국인유학생 = sum(외국인유학생_계), 
               제외인정자 = sum(제외인정자_계), 
               기타 = sum(기타_계), 
               미상 = sum(미상_계), 
               ## 백분률인 취업률은 그 자체로 합계나 평균을 낼 수 없으니 각 그룹별로 재계산
               취업률 = 취업자 / (졸업자 - (진학자+입대자+취업불가능자+외국인유학생+제외인정자))) |>
  ## 계열의 표시 순서를 설정하기 위해 레벨을 재조정
  mutate(대계열 = fct_relevel(대계열, '인문계열', '사회계열', '교육계열', '자연계열', '공학계열', '의약계열', '예체능계열'))


df_취업통계_계열별 |>
  ggplot(aes(x = 과정구분, y = 취업률, fill = 대계열)) +
  geom_col(position = 'dodge') + 
  geom_segment(aes(x = 0.6, xend = 1.4, y = -0.1, yend = -0.1), size = 2, color = 'red') +
  geom_segment(aes(x = 1.6, xend = 2.4, y = -0.1, yend = -0.1), size = 2, color = 'blue') +
  geom_segment(aes(x = 2.6, xend = 3.4, y = -0.1, yend = -0.1), size = 2, color = 'green') +
  geom_segment(aes(x = 0.6, xend = 1.4, y = 0.95, yend = 1), size = 10, color = 'red') +
  geom_segment(aes(x = 1.6, xend = 2.4, y = 0.95, yend = 1), size = 10, color = 'blue') +
  geom_segment(aes(x = 2.6, xend = 3.4, y = 0.95, yend = 1), size = 10, color = 'green') +
  geom_text(aes(x = 1, y = 0.95), label = '전문대학과정') + 
  scale_y_continuous(labels = scales::percent, limits = c(-0.5, 1.1)) +
  coord_polar()

df_취업통계_계열별 |>
  ggplot(aes(x = 과정구분, y = 취업률, fill = 대계열)) +
  geom_col(position = 'dodge') + 
  geom_segment(aes(x = 0.6, xend = 1.4, y = -0.1, yend = -0.1), size = 2, color = 'red') +
  geom_segment(aes(x = 1.6, xend = 2.4, y = -0.1, yend = -0.1), size = 2, color = 'blue') +
  geom_segment(aes(x = 2.6, xend = 3.4, y = -0.1, yend = -0.1), size = 2, color = 'green') +
  geom_segment(aes(x = 0.6, xend = 1.4, y = 0.95, yend = 1), size = 10, color = 'red') +
  geom_segment(aes(x = 1.6, xend = 2.4, y = 0.95, yend = 1), size = 10, color = 'blue') +
  geom_segment(aes(x = 2.6, xend = 3.4, y = 0.95, yend = 1), size = 10, color = 'green') +
  geom_text(aes(x = 1, y = 0.95), label = '전문대학과정') + 
  scale_y_continuous(labels = scales::percent, limits = c(-0.5, 1.1)) +
  coord_polar()


install.packages('geomtextpath')
library(geomtextpath)


z <- data.frame( 
  a=c("sensor 1","sensor 2","sensor 3","sensor 4",
      "sensor 5","sensor 6","sensor 7","sensor 8"),  
  b=c(50, 60, 70, 20,90,110,30,100)
)

cxc <- ggplot(z, aes(x=a, y=b, fill=factor(b))) + 
  geom_bar(width = 1,stat="identity",colour = "black")

cxc + coord_curvedpolar() + 
  theme_linedraw() +
  theme(axis.ticks =element_blank(), 
        axis.text.y =element_blank(), 
        axis.title=element_blank(), 
        axis.text.x=element_text(size = 12))
