library(tidyverse)
library(patchwork)
showtext_auto()

font_add('NanumBarunGothic', 'c:/windows/fonts/NanumBarunGothic.ttf')

df_취업통계$대계열 = fct_relevel(df_취업통계$대계열, '인문계열', '사회계열', '교육계열', '자연계열', '공학계열', '의약계열', '예체능계열')

df_취업통계$과정구분 = fct_relevel(df_취업통계$과정구분, '전문대학과정', '대학과정',  '대학원과정')


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
  arrange(과정구분, 대계열) |>
  ungroup() |>
  mutate(id = seq(1:n())) |>
  mutate(angle = 90 - (id-0.5)/n() * 360) |>
  mutate(angle1 = case_when(
    id > n()/2 ~ angle + 180, 
    id <= n()/2 ~ angle
  ))


df_취업통계_과정별 <- df_취업통계 |> 
  group_by(과정구분) |>  
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
  arrange(과정구분) |>
  ungroup() |>
  mutate(id = seq(1:n())) |>
  mutate(angle = 90 - (id-0.5)/n() * 360) |>
  mutate(angle1 = case_when(
    id >= n()/2 ~ angle + 270, 
    id < n()/2 ~ angle -90, 
  ))

df_취업통계_전체 <- df_취업통계 |> 
  summarise(## 백분률인 취업률은 그 자체로 합계나 평균을 낼 수 없으니 각 그룹별로 재계산
            취업률 = sum(취업자_합계_계) / (sum(졸업자_계) - (sum(진학자_계)+sum(입대자)+sum(취업불가능자_계)+sum(외국인유학생_계)+sum(제외인정자_계)))) |>
  select(취업률) |>
  pull()
  

#View(df_취업통계_과정별)

#View(df_취업통계_계열별)
# angle <- 90 - (df_취업통계_계열별$id-0.5)/nrow(df_취업통계_계열별) * 360
# calculate the ANGLE of the labels
# I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
#label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
#label_data$angle<-ifelse(angle < -90, angle+180, angle)
library(geomtextpath)
library(ragg)
library(showtext)

library(png)
library(ggpubr)

im = readPNG("C:/R/git/datavisualization/chap12/back.png")


df_취업통계_계열별 |>
  ggplot(aes(x = id, y = 취업률, fill = 대계열)) + 
  geom_col(position = 'dodge', show.legend = F)+ 
  scale_y_continuous(labels = scales::percent, limits = c(-0.5, 1.2)) +
  scale_x_continuous(limits = c(0.5, 21.5)) +
  annotate(xmin = 0.5, xmax = 7.5, ymin = -0.1, ymax = 1, alpha = 0.1, geom = 'rect', fill = 'red') +
  annotate(xmin = 7.5, xmax = 14.5, ymin = -0.1, ymax = 1, alpha = 0.1, geom = 'rect', fill = 'green') +
  annotate(xmin = 14.5, xmax = 21.5, ymin = -0.1, ymax = 1, alpha = 0.1, geom = 'rect', fill = 'blue') +
  geom_textpath(data = df_취업통계_과정별,aes(x = 4+((id-1)*7), y = 1.0, label = paste0(과정구분, ', ', round(취업률*100, 1), '%'), color = as.factor(id)), inherit.aes = F, show.legend = F, rich = TRUE, family = 'NanumBarunGothic') +
 geom_text(data = df_취업통계_계열별, aes(x=id, y=0.5, label=paste0(대계열, ', ', round(취업률*100, 1), '%'), angle= angle1), color="black", inherit.aes = FALSE, size = rel(3), hjust = 0.5) +
  geom_segment(data = df_취업통계_과정별, aes(x = 0.5+((id-1)*7), xend = 7.5+((id-1)*7), y = 취업률, yend = 취업률, color = as.factor(id)), inherit.aes = F, show.legend = F) + 
  scale_fill_brewer(palette = 'Set3') + 
  coord_curvedpolar() +
  theme_void() + 
  labs(title = '계열별 취업률')+ 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        plot.margin = margin(0.25, 0, 0, 0), 
        text = element_text(family = 'NanumBarunGothic')) + 
  annotate(x = 0.5, y = -0.4, geom = 'text', label = '전체취업률') + 
  geom_text(aes(x = 0.5, y = -0.5, label = paste0(round(df_취업통계_전체, 3)*100, '%')), size = rel(5)) 


  annotate(x = 0.5, y = -0.5, geom = 'text', label = round(df_취업통계_전체, 3)*100, size = rel(5), )

  

