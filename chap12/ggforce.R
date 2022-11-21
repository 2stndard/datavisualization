library(ggforce)
install.packages('concaveman')
library(concaveman)
library(readxl)
library(tidyverse)

df_취업통계 <- read_excel('c:/R/git/datavisualization/chap3/2020년 학과별 고등교육기관 취업통계.xlsx', 
                      ## '학과별' 시트의 데이터를 불러오는데,
                      sheet = '학과별',
                      ## 앞의 13행을 제외하고
                      skip = 13, 
                      ## 첫번째 행은 열 이름으로 설정
                      col_names = TRUE, 
                      ## 열의 타입을 설정, 처음 9개는 문자형으로 다음 79개는 수치형으로 설정
                      col_types = c(rep('text', 9), rep('numeric', 79)))

## df_취업통계에서 첫번째부터 9번째까지의 열과 '계'로 끝나는 열을 선택하여 다시 df_취업통계에 저장
df_취업통계 <- df_취업통계 |> select(1:9, ends_with('계'), '입대자')

df_취업통계 |> 
  ggplot(aes(x = 졸업자_계, y = 취업자_합계_계, color = 대계열)) +
  ## X축이 졸업자_계, Y축이 취업자_합계_계에 매핑된 geom_point 레이어 생성
  geom_point() +
  geom_mark_rect() 

df_취업통계 |> 
  ggplot(aes(x = 졸업자_계, y = 취업자_합계_계, color = 대계열)) +
  ## X축이 졸업자_계, Y축이 취업자_합계_계에 매핑된 geom_point 레이어 생성
  geom_point() +
  geom_mark_ellipse() 


df_취업통계 |> 
  ggplot(aes(x = 졸업자_계, y = 취업자_합계_계, color = 대계열)) +
  ## X축이 졸업자_계, Y축이 취업자_합계_계에 매핑된 geom_point 레이어 생성
  geom_point() +
  geom_mark_hull(aes(color = 대계열, filter = 대계열 == '의약계열')) 



  
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
  


oecd_country <- c('United States', 'Canada', 'United Kingdom', 'Denmark', 
                  'Iceland', 'Norway', 'Turkey', 'Spain', 'Portugal', 
                  'France', 'Ireland', 'Belgium', 'Germany', 'Greece', 'Sweden', 
                  'Austria', 'Italy', 'Switzerland', 'Luxembourg', 'Netherlands', 
                  'Australia', 'Chile', 'Colombia', 'Costa Rica', 'Czechia', 
                  'Estonia', 'Finland', 'Hungary', 'Israel', 'Japan', 
                  'South Korea', 'Latvia', 'Lithuania', 'Mexico', 
                  'New Zealand', 'Poland', 'Slovakia', 'Slovenia')

covid19_stat |> filter(is.finite(백신접종완료률), 
                       is.finite(십만명당사망자수), 
                       !is.na(백신접종완료률), 
                       !is.na(십만명당사망자수), 
                       십만명당사망자수 != 0, 
                       인구백명당백신접종완료률 != 0) |>
  ggplot(aes(x = 백신접종완료률, y = 십만명당사망자수)) +
  geom_point(aes(size = 인당GDP)) +
  ggforce::geom_mark_hull(data = covid19_stat |> filter(location %in% oecd_country)) 
