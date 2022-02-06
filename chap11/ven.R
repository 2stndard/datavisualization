library(showtext)
showtext_auto()


## df_입학자_long 데이터에서
df_donut_all <- df_입학자_long |>
  ## 일부 데이터만 필터링
  filter(학교종류 %in% c('전문대학', '일반대학', '석사', '박사'), 지역 == '전체') |>
  ## 표시 순서를 설정
  mutate(학교종류 = fct_relevel(학교종류, '전문대학', '일반대학', '석사', '박사')) |> 
  ## 연도별로 그루핑
  group_by(연도) |>
  ## 각 연도별로 학교 종류별 비율을 계산
  mutate(비율 = 입학생수 / sum(입학생수))

|>
  ungroup()

|>
  mutate(비율 = round(입학생수 / sum(입학생수), 3))

p_donut_all <- df_donut_all |>
  ggplot()

p_donut_all1 <- p_donut_all +
  ## X축을 1, Y축을 입학생수, fill을 학교종류로 매핑하고 비율로 표현하기 위해 position을 fill로 설정한 geom_col 레이어를 생성
  geom_col(aes(x = 1, y = 입학생수, fill = 학교종류), position = 'fill') +
  geom_text(aes(x = 1, y = 비율, label = scales::percent(비율, accuracy = 0.1), fill = 학교종류), position = position_stack(vjust = 0.5), size = 2) + 
  ## X축을 0부터 2까지로 한정
  xlim(0,2) +
  facet_wrap(~연도)

p_donut_all1


p_donut_all1 +
  coord_polar(theta = "y", direction = -1) +
  geom_text(aes(label = 연도, x = 0, y = 0), size = 3, fontface='bold') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = '연도별 입학생 구성비') +
  theme(strip.background = element_blank(), strip.text = element_blank())



df_취업통계 |> group_by(학과명) |> distinct(학과명, 과정구분) |>
  count(학과명) |> arrange(desc(n)) |> View()



vec_전문대학과명 <- df_취업통계 |> 
  filter(대계열 == '공학계열', 과정구분 == '전문대학과정') |>
  select(학과명) |> 
  pull()


vec_대학학과명 <- df_취업통계 |> 
  filter(대계열 == '공학계열', 과정구분 == '대학과정') |>
  select(학과명) |> 
  pull()

vec_대학원학과명 <- df_취업통계 |> 
  filter(대계열 == '공학계열', 과정구분 == '대학원과정') |>
  select(학과명) |> 
  pull()

vec_석사학과명 <- df_취업통계 |> 
  filter(대계열 == '공학계열', 과정구분 == '대학원과정', 학위구분 == '석사') |>
  select(학과명) |> 
  pull()

vec_박사학과명 <- df_취업통계 |> 
  filter(대계열 == '공학계열', 과정구분 == '대학원과정', 학위구분 == '박사') |>
  select(학과명) |> 
  pull()



# Chart
## 벤다이어그램 생성
venn.diagram(
  ## 앞서 생성한 세개의 벡터를 리스트로 만들어 X에 할당
  x = list(vec_전문대학과명, vec_대학학과명, vec_대학원학과명),
  ## 카테고리 이름을 설정
  category.names = c("전문대학" , "Set 2" , "Set 3"),
  ## 벤다이어그램 출력 파일 이름 설정
  filename = '기본벤다이어그램.png', 
  lwd = 1,
  col=c("#440154ff", '#21908dff', '#fde725ff'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3),alpha('#fde725ff',0.3)), 
  cat.cex = 0.3,
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  cat.col = c("#440154ff", '#21908dff', '#fde725ff'),
)


getwd()

? VennDiagram


grid.newpage()                                        # Move to new plotting page
draw.triple.venn(area1 = 10,                          # Create venn diagram with three sets
                 area2 = 20,
                 area3 = 15,
                 n12 = 2,
                 n23 = 3,
                 n13 = 7,
                 n123 = 2, 
                 category = c("가타나 1", "Group 2", "Group 3"))


install.packages('ggVennDiagram')
library(ggVennDiagram)

list_venn_diagram <- list(전문대학 = vec_전문대학과명, 대학 = vec_대학학과명, 대학원 = vec_대학원학과명)

list_venn_diagram1 <- list(전문대학 = vec_전문대학과명, 대학 = vec_대학학과명, 석사 = vec_석사학과명, 박사 = vec_박사학과명)


ggVennDiagram(list_venn_diagram, 
              category.names = c("전문대학" , "대학" , "대학원"))

ggVennDiagram(list_venn_diagram1)


df_venn_diagram <- process_data(Venn(list_venn_diagram))

df_venn_diagram

ggplot() + 
  geom_sf(aes(fill=count), data = venn_region(df_venn_diagram)) +
  geom_sf(color = "grey", data = venn_setedge(df_venn_diagram), show.legend = F) +
  geom_sf_text(aes(label = name), data = venn_setlabel(df_venn_diagram)) +
  geom_sf_label(aes(label= paste0(round(prop.table(count)*100, 1), '%')), fontface = "bold", data = venn_region(df_venn_diagram)) +
#  geom_sf_label(aes(label=percent), fontface = "bold", data = venn_region(df_venn_diagram)) +
  theme_void()


list_venn_diagram <- list(전문대학 = vec_전문대학과명, 대학 = vec_대학학과명, 대학원 = vec_대학원학과명)

ggVennDiagram(list_venn_diagram, 
              category.names = c("전문대학" , "대학" , "대학원"))

df_venn_diagram <- process_data(Venn(list_venn_diagram))

df_venn_diagram

ggplot() + 
  geom_sf(aes(fill=count), data = venn_region(df_venn_diagram)) +
  geom_sf(color = "grey", data = venn_setedge(df_venn_diagram), show.legend = F) +
  geom_sf_text(aes(label = name), data = venn_setlabel(df_venn_diagram)) +
  geom_sf_label(aes(label= paste0(round(prop.table(count)*100, 1), '%')), fontface = "bold", data = venn_region(df_venn_diagram)) +
  #  geom_sf_label(aes(label=percent), fontface = "bold", data = venn_region(df_venn_diagram)) +
  theme_void()


df_춘향전 |>
  (function(x) 
  { 
    filter(gender == 'M') |> 
      head()
  })

install.packages('ggthemes', dependencies = TRUE)

vignette('ggthemes')


