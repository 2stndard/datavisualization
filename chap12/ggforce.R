library(ggforce)
install.packages('concaveman')
library(concaveman)
df_취업통계 |> 
  ggplot(aes(x = 졸업자_계, y = 취업자_합계_계, color = 대계열)) +
  ## X축이 졸업자_계, Y축이 취업자_합계_계에 매핑된 geom_point 레이어 생성
  geom_point() +
  geom_mark_rect() 
  
  ## X축과 Y축의 범위를 설정
  lims(x = c(0, 2500), y = c(0, 2000)) +
  labs(title = '기본 산점도', x = '졸업자수', y = '취업자수')

  
p_scatter <- df_취업통계_sample |> 
  ggplot() + 
  labs(x = '졸업자수', y = '취업자수')

p_scatter +
  ## X축을 졸업자_계, Y축을 취업자_합계_계로 매핑한 geom_point로 산점도 레이어 생성
  geom_point(aes(x = 졸업자_계, y = 취업자_합계_계, color = 대계열)) +
  facet_zoom(xlim = c(0, 50), ylim = c(0, 50))
             
             
             + 
  theme_void()


+
  ## X축을 졸업자_계, Y축을 취업자_합계_계로 매핑한 geom_smooth로 추세선 레이어 생성
  geom_smooth(aes(x = 졸업자_계, y = 취업자_합계_계, color = 대계열)) +
  ## 제목, X축 제목, Y축 제목 설정
  labs(title = '추세선이 추가된 산점도')
  