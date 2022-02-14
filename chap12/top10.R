library(tidyverse)
library(showtext)
showtext_auto()

font_add('NanumBarunGothic', 'c:/windows/fonts/NanumBarunGothic.ttf')
font_add('NanumBarunGothicBold', 'c:/windows/fonts/NanumBarunGothicBold.ttf')



df_취업통계_계열별 <- df_취업통계 |> 
  group_by(과정구분, 대계열, 중계열, 소계열) |>  
  summarise(학과수 = n(), 졸업자 = sum(졸업자_계), 
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

df_취업통계_과정별 <- df_취업통계 |> 
  group_by(과정구분) |>  
  summarise(학과수 = n(), 졸업자 = sum(졸업자_계), 
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
  mutate(rank = min_rank(-취업률), 
         pos = case_when(
            과정구분 == '대학원과정' ~ 1,
            과정구분 == '대학과정' ~ 2,
            과정구분 == '전문대학과정' ~ 3
         ))
  


View(df_취업통계_과정별)


df_취업통계_소계열별 <- df_취업통계_계열별 |> filter(학과수 >= 3) |>
  group_by(과정구분) |>
  top_n(n = 10, wt = 취업률) |>
  mutate(소계열순위 = min_rank(-취업률)) |>
  mutate(과정구분 = fct_relevel(과정구분, '대학원과정', '대학과정', '전문대학과정'))
  

View(df_취업통계_소계열별)

df_취업통계_소계열별 |>
  ggplot() + 
  geom_col(aes(x = 과정구분, y = 취업률, fill = as.factor(소계열순위)), position = position_dodge2(width = 0.9, padding = 0.1, reverse = TRUE), show.legend = FALSE, size = 0) + 
  geom_text(aes(x = 과정구분, y = 0.01, fill = as.factor(소계열순위), label = 소계열), position = position_dodge2(width = 0.9, padding = 0.1, reverse = TRUE), hjust = 0, show.legend = FALSE) +
  geom_text(aes(x = 과정구분, y = 취업률, fill = as.factor(소계열순위), label = paste0(round(취업률, 3)*100, '%')), position = position_dodge2(width = 0.9, padding = 0.1, reverse = TRUE), hjust = 1.1, show.legend = FALSE) +
  geom_segment(aes(x = 0.55, xend = 1.45, y = -0.01, yend = -0.01), color = 'goldenrod1') +
  geom_segment(aes(x = 1.55, xend = 2.45, y = -0.01, yend = -0.01), color = 'coral2') +
  geom_segment(aes(x = 2.55, xend = 3.45, y = -0.01, yend = -0.01), color = 'mediumpurple1') +
  scale_fill_manual(values = c("goldenrod1",  "mediumpurple1", "coral2", rep("gray70", 7))) +
  scale_x_discrete(labels = c('대\n학\n원\n과\n정', '대\n학\n과\n정', '전\n문\n대\n학\n과\n정'), 
                   expand = expansion(add = c(0, 0))) +
  scale_y_continuous(expand = expansion(add = c(0.005, 0.05)), label = scales::percent) +
  theme(text = element_text(family = 'NanumBarunGothic', size = rel(4)), 
        axis.text.y = element_text(hjust = 0, size = 15), 
        axis.ticks.y = element_line(unit(0, 'mm')), 
        panel.background = element_blank()) +
  labs(x = NULL, y = NULL) + 
  geom_segment(data = df_취업통계_과정별 |> filter(과정구분 == '대학원과정'), aes(x = 0.55, xend = 1.45, y = 취업률, yend = 취업률), color = 'goldenrod1') + 
  geom_segment(data = df_취업통계_과정별 |> filter(과정구분 == '대학과정'), aes(x = 1.55, xend = 2.45, y = 취업률, yend = 취업률), color = 'coral2') + 
  geom_segment(data = df_취업통계_과정별 |> filter(과정구분 == '전문대학과정'), aes(x = 2.55, xend = 3.45, y = 취업률, yend = 취업률), color = 'mediumpurple1') + 
  geom_label(data = df_취업통계_과정별 |> filter(과정구분 == '대학원과정'), aes(x = 1, y = 취업률, label = round(취업률, 3)*100), color = 'goldenrod1', fill = 'white') + 
  geom_label(data = df_취업통계_과정별 |> filter(과정구분 == '대학과정'), aes(x = 2, y = 취업률, label = round(취업률, 3)*100), color = 'coral2', fill = 'white') + 
  geom_label(data = df_취업통계_과정별 |> filter(과정구분 == '전문대학과정'), aes(x = 3, y = 취업률, label = round(취업률, 3)*100), color = 'mediumpurple1', fill = 'white') + 
  coord_flip()

df_취업통계_소계열별 |>
  ggplot() + 
  geom_col(aes(x = 과정구분, y = 취업률, fill = as.factor(소계열순위)), position = position_dodge2(width = 0.9, padding = 0.1, reverse = TRUE), show.legend = FALSE, size = 0) + 
  geom_text(aes(x = 과정구분, y = 0.01, fill = as.factor(소계열순위), label = 소계열), position = position_dodge2(width = 0.9, padding = 0.1, reverse = TRUE), hjust = 0, show.legend = FALSE) +
  geom_text(aes(x = 과정구분, y = 취업률, fill = as.factor(소계열순위), label = paste0(round(취업률, 3)*100, '%')), position = position_dodge2(width = 0.9, padding = 0.1, reverse = TRUE), hjust = 1.1, show.legend = FALSE) +
  geom_segment(aes(x = 0.55, xend = 1.45, y = -0.01, yend = -0.01), color = 'goldenrod1') +
  geom_segment(aes(x = 1.55, xend = 2.45, y = -0.01, yend = -0.01), color = 'coral2') +
  geom_segment(aes(x = 2.55, xend = 3.45, y = -0.01, yend = -0.01), color = 'mediumpurple1') +
  scale_fill_manual(values = c("goldenrod1",  "mediumpurple1", "coral2", rep("gray70", 7))) +
  scale_color_manual(values = c("goldenrod1",  "mediumpurple1", "coral2", rep("gray70", 7))) +
  scale_x_discrete(labels = c('대\n학\n원\n과\n정', '대\n학\n과\n정', '전\n문\n대\n학\n과\n정'), 
                   expand = expansion(add = c(0, 0))) +
  scale_y_continuous(expand = expansion(add = c(0.005, 0.05)), label = scales::percent) +
  theme(text = element_text(family = 'NanumBarunGothic', size = rel(4)), 
        axis.text.y = element_text(hjust = 0, size = 15), 
        axis.ticks.y = element_line(unit(0, 'mm')), 
        panel.background = element_blank(), 
        legend.position = 'none') +
  labs(x = NULL, y = NULL) + 
  geom_segment(data = df_취업통계_과정별 |> filter(과정구분 == '대학원과정'), aes(x = 0.55, xend = 1.45, y = 취업률, yend = 취업률), color = 'goldenrod1') + 
  geom_segment(data = df_취업통계_과정별 |> filter(과정구분 == '대학과정'), aes(x = 1.55, xend = 2.45, y = 취업률, yend = 취업률), color = 'coral2') + 
  geom_segment(data = df_취업통계_과정별 |> filter(과정구분 == '전문대학과정'), aes(x = 2.55, xend = 3.45, y = 취업률, yend = 취업률), color = 'mediumpurple1') + 
  geom_label(data = df_취업통계_과정별 |> filter(과정구분 == '대학원과정'), aes(x = 1, y = 취업률, label = round(취업률, 3)*100), color = 'goldenrod1', fill = 'white') + 
  geom_label(data = df_취업통계_과정별 |> filter(과정구분 == '대학과정'), aes(x = 2, y = 취업률, label = round(취업률, 3)*100), color = 'coral2', fill = 'white') + 
  geom_label(data = df_취업통계_과정별 |> filter(과정구분 == '전문대학과정'), aes(x = 3, y = 취업률, label = round(취업률, 3)*100), color = 'mediumpurple1', fill = 'white') + 
  geom_linerange(data = df_취업통계_소계열별 |> filter(과정구분 == '대학원과정'), aes(x = 1, y = 취업률, ymin = 0.802, ymax = 취업률, color = as.factor(소계열순위)), position = position_dodge2(width = 0.9, reverse = TRUE), width = 0.1) + 
  geom_linerange(data = df_취업통계_소계열별 |> filter(과정구분 == '대학과정'), aes(x = 2, y = 취업률, ymin = 0.611, ymax = 취업률, fill = as.factor(소계열순위), color = as.factor(소계열순위)), position = position_dodge2(width = 0.9, reverse = TRUE), width = 0.1) + 
  geom_linerange(data = df_취업통계_소계열별 |> filter(과정구분 == '전문대학과정'), aes(x = 3, y = 취업률, ymin = 0.691, ymax = 취업률, fill = as.factor(소계열순위)), position = position_dodge2(width = 0.9, reverse = TRUE), width = 0.1) +
  coord_flip()

  





df_취업통계_소계열별 |>
  ggplot() + 
  geom_col(aes(x = 과정구분, y = 취업률, fill = as.factor(소계열순위)), position = position_dodge2(width = 0.9, reverse = TRUE)) + 
  geom_text(aes(x = 과정구분, y = 0.01, fill = as.factor(소계열순위), label = 소계열), position = position_dodge2(width = 0.9, padding = 0.1, reverse = TRUE), hjust = 0, show.legend = FALSE) +
  geom_text(aes(x = 과정구분, y = 취업률, fill = as.factor(소계열순위), label = paste0(round(취업률, 3)*100, '%')), position = position_dodge2(width = 0.9, padding = 0.1, reverse = TRUE), hjust = -0.1, show.legend = FALSE) +
  geom_segment(data = df_취업통계_과정별, aes(x = pos - 0.45, xend = pos + 0.45, y = -0.01, yend = -0.01, color = as.factor(rank)), size = 2) + 
  geom_segment(data = df_취업통계_과정별, aes(x = pos - 0.45, xend = pos + 0.45, y = 취업률, yend = 취업률, color = as.factor(rank))) + 
  geom_label(data = df_취업통계_과정별, aes(x = pos, y = 취업률, label = paste0(round(취업률, 3)*100, '%'), color = as.factor(rank)), fill = 'white', size = 8, hjust = 1.1) + 
  scale_fill_manual(values = c("goldenrod1",  "mediumpurple1", "coral2", rep("gray70", 7))) +
  scale_color_manual(values = c("goldenrod1",  "mediumpurple1", "coral2", rep("gray70", 7))) +
  scale_x_discrete(labels = c('대\n학\n원\n과\n정', '대\n학\n과\n정', '전\n문\n대\n학\n과\n정'), 
                   expand = expansion(add = c(0, 0))) +
  scale_y_continuous(expand = expansion(add = c(0.005, 0.05)), label = scales::percent)  +
  theme(text = element_text(family = 'NanumBarunGothic', size = rel(4)), 
        axis.text.y = element_text(hjust = 0, size = 15), 
        axis.ticks.y = element_line(unit(0, 'mm')), 
        panel.background = element_blank(), 
        legend.position = 'none')  +
  labs(x = NULL, y = NULL)  + 
  coord_flip()
