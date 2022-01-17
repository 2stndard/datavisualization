library(gt)
library(tidyverse)
library(readxl)
df_취업통계 <- read_excel('d:/R/git/datavisualization/chap3/2020년 학과별 고등교육기관 취업통계.xlsx', 
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

## df_취업통계 정보 확인
str(df_취업통계)

df_gt <- df_취업통계 |>
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
            취업률 = 취업자 / (졸업자 - (진학자+입대자+취업불가능자+외국인유학생+제외인정자))) |>
  ungroup()  |> 
  arrange(과정구분)


df_gt_summary <- df_gt |> group_by(대계열) |> 
  summarise(취업률 = sum(취업자) / (sum(졸업자) - (sum(진학자)+sum(입대자)+sum(취업불가능자)+sum(외국인유학생)+sum(제외인정자))))

df_gt_grand_summary <- df_gt |>  
  summarise(취업률 = sum(취업자) / (sum(졸업자) - (sum(진학자)+sum(입대자)+sum(취업불가능자)+sum(외국인유학생)+sum(제외인정자))))



df_gt$과정구분 <- fct_relevel(df_gt$과정구분, '전문대학과정', '대학과정', '대학원과정')

df_gt$대계열 <- fct_relevel(df_gt$대계열, '인문계열', '사회계열', '교육계열', '자연계열', '공학계열', '의약계열', '예체능계열')

##########################################################################
gt_table1 <- df_gt |> arrange(과정구분) |>
  gt(rowname_col = '과정구분', 
     groupname_col = '대계열')

gt_table1 %>%
  gtsave(
    "4-6.pdf",
    path = 'C:/R/git/datavisualization/chap4/'
  )

gt_table1 %>%
  gtsave(
    "4-6.rtf",
    path = 'C:/R/git/datavisualization/chap4/'
  )

gt_table1 %>%
  gtsave(
    "4-6.html",
    path = 'C:/R/git/datavisualization/chap4/'
  )

##########################################################################
gt_table2 <- gt_table1 |>
  tab_header(title = '고등교육기관 취업통계', subtitle = '2021년 전체 고등교육기관 대상')

gt_table2 %>%
  gtsave(
    "4-7.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )

gt_table2 %>%
  gtsave(
    "4-7.pdf",
    path = 'C:/R/git/datavisualization/chap4/'
  )

###########################################################################

gt_table3 <- gt_table2 |> 
  fmt_number(columns = 4:18, decimals = 0, use_seps = TRUE) |>
  fmt_percent(columns = 19, decimals = 1, use_seps = FALSE)

gt_table3 %>%
  gtsave(
    "4-8.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################


gt_table4 <- gt_table3 |> 
  tab_spanner(columns = 5:11, label = '취업 상세') |>
  tab_spanner(columns = 12:18, label = '비취업 상세') 
  

gt_table4 %>%
  gtsave(
    "4-9.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################

gt_table5 <- gt_table4 |> 
  cols_label(일인창사업자 = '1인창(사)업자'
  )


gt_table5 %>%
  gtsave(
    "4-10.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################


gt_table6 <- gt_table5 |> 
  row_group_order(
    groups = c('인문계열', '사회계열', '교육계열', '자연계열', '공학계열', '의약계열', '예체능계열')
  )


gt_table6 %>%
  gtsave(
    "4-11.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################


gt_table7 <- gt_table6 |> 
  summary_rows(
    groups = T,
    columns = 3:18,
    fns = list(
      소계 = ~sum(.)),
    formatter = fmt_number, 
    decimals = 0,
    use_seps = TRUE
  ) |>
  summary_rows(
    groups = '인문계열',
    columns = 19,
    fns = list(
      소계 = ~pull(df_gt_summary |> filter(대계열 == '인문계열') |> select(취업률))
      ),
    formatter = fmt_percent, 
    decimals = 1
  ) |>
  summary_rows(
    groups = '사회계열',
    columns = 19,
    fns = list(
      소계 = ~pull(df_gt_summary |> filter(대계열 == '사회계열') |> select(취업률))
    ),
    formatter = fmt_percent, 
    decimals = 1
  ) |>
  summary_rows(
    groups = '교육계열',
    columns = 19,
    fns = list(
      소계 = ~pull(df_gt_summary |> filter(대계열 == '교육계열') |> select(취업률))
    ),
    formatter = fmt_percent, 
    decimals = 1
  ) |>
  summary_rows(
    groups = '자연계열',
    columns = 19,
    fns = list(
      소계 = ~pull(df_gt_summary |> filter(대계열 == '자연계열') |> select(취업률))
    ),
    formatter = fmt_percent, 
    decimals = 1
  ) |>
  summary_rows(
    groups = '공학계열',
    columns = 19,
    fns = list(
      소계 = ~pull(df_gt_summary |> filter(대계열 == '공학계열') |> select(취업률))
    ),
    formatter = fmt_percent, 
    decimals = 1
  ) |>
  summary_rows(
    groups = '의약계열',
    columns = 19,
    fns = list(
      소계 = ~pull(df_gt_summary |> filter(대계열 == '의약계열') |> select(취업률))
    ),
    formatter = fmt_percent, 
    decimals = 1
  ) |>
  summary_rows(
    groups = '예체능계열',
    columns = 19,
    fns = list(
      소계 = ~pull(df_gt_summary |> filter(대계열 == '예체능계열') |> select(취업률))
    ),
    formatter = fmt_percent, 
    decimals = 1
  )


gt_table7 %>%
  gtsave(
    "4-12.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################


gt_table8 <- gt_table7 |>
  grand_summary_rows(
    columns = 3:18,
    fns = list(
      총계 = ~sum(.)), 
    formatter = fmt_number, 
    decimals = 0,
    use_seps = TRUE
  ) |>
  grand_summary_rows(
    columns = 19,
    fns = list(
      총계 = ~pull(df_gt_grand_summary |> select(취업률))), 
    formatter = fmt_percent, 
    decimals = 1
  )

gt_table8 %>%
  gtsave(
    "4-13.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################

colors()

gt_table9 <- gt_table8 |>
  tab_options(
    table.font.size = 12,
    ## 행 그룹 요약 행의 배경색 설정
    summary_row.background.color = "deepskyblue3",
    ## 전체 요약 행의 배경색 설정
    grand_summary_row.background.color = "deepskyblue4",
    ## 구분(Stub) 행의 헤더 외곽선 스타일 설정
    stub.border.style = 'solid', 
    ## 구분(Stub) 행의 배경색 설정
    stub.background.color = 'lightskyblue1',
    ## 행 그룹 이름 표현 셀 배경색 설정
    row_group.background.color = 'aliceblue',
    ## 열 제목(Heading) 배경색 설정
    heading.background.color = 'dodgerblue4',
    ## 표 몸체(Body) 수평선 스타일 설정
    table_body.hlines.style = 'dashed', 
    ## 표 몸체(Body) 수직선 색깔 설정 
    table_body.vlines.color = 'grey',
    ## 표 몸체(BOdy) 수직선 스타일 설정
    table_body.vlines.style = 'dashed'
  )

gt_table9 <- gt_table8 |>
  tab_options(
    table.font.size = 12,
    ## 행 그룹 요약 행의 배경색 설정
    summary_row.background.color = "#009acd",
    ## 전체 요약 행의 배경색 설정
    grand_summary_row.background.color = "#00688B",
    ## 구분(Stub) 행의 헤더 외곽선 스타일 설정
    stub.border.style = 'solid', 
    ## 구분(Stub) 행의 배경색 설정
    stub.background.color = '#00BFFF',
    ## 행 그룹 이름 표현 셀 배경색 설정
    row_group.background.color = '#F0F8FF',
    ## 열 제목(Heading) 배경색 설정
    heading.background.color = '#104E8B',
    ## 표 몸체(Body) 수평선 스타일 설정
    table_body.hlines.style = 'dashed', 
    ## 표 몸체(Body) 수직선 색깔 설정 
    table_body.vlines.color = 'grey',
    ## 표 몸체(BOdy) 수직선 스타일 설정
    table_body.vlines.style = 'dashed'
  )

gt_table9 %>%
  gtsave(
    "4-14.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################

gt_table10 <- gt_table9 |>
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = px(25))
    )
  ) |>
  tab_style(
    locations = cells_title(groups = "subtitle"),
    style     = list(
      cell_text(size = px(15))
    )
  )


gt_table10 %>%
  gtsave(
    "4-15.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################


gt_table11 <- gt_table10 |>
  tab_style(
    locations = cells_column_spanners(spanners = "취업 상세"),
    style     = list(
      cell_text(weight = "bold", color = 'grey20', align = 'center')
    )
  ) %>% 
  tab_style(
    locations = cells_column_labels(columns = 5:11),
    style     = list(
      cell_text(weight = "lighter", color = 'grey50', align = 'center'), 
      "vertical-align:middle"   ## css attribute
    )
  ) %>% 
  tab_style(
    locations = cells_column_spanners(spanners = "비취업 상세"),
    style     = list(
      cell_text(weight = "bold",color = "grey20", align = 'center')
    )
  ) |>
  tab_style(
    locations = cells_column_labels(columns = 12:18),
    style     = list(
      cell_text(weight = "lighter", color = 'grey50', align = 'center'), 
      "vertical-align:middle"   ## css attribute
    )
  ) |>
  tab_style(
    locations = cells_column_labels(columns = c(3:4, 19)),
    style     = list(
      cell_text(weight = "bold", color = 'grey20', align = 'center'), 
      "vertical-align:middle"   ## css attribute
    )
  )

gt_table11 %>%
  gtsave(
    "4-16.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################



gt_table12 <- gt_table11 |> 
  tab_style(
    locations = cells_row_groups(groups = everything()),
    style     = list(
      cell_text(color = "grey25", size = 24, align = 'center', weight = 'bold'), 
      cell_fill(color = 'aliceblue')
    )
  ) |>
  tab_style(
    locations = cells_summary(groups = everything()),
    style     = list(
      cell_text(color = "white", size = 24, align = 'center', weight = 'bold')
    )
  ) |>
  tab_style(
    locations = cells_grand_summary(),
    style     = list(
      cell_text(color = "white", size = 24, align = 'center', weight = 'bold')
    )
  ) |>
  tab_style(
    locations = cells_stub_summary(),
    style     = list(
      cell_text(color = "white", size = 24, align = 'center', weight = 'bold')
    )
  ) |>
  tab_style(
    locations = cells_stub_grand_summary(),
    style     = list(
      cell_text(color = "white", size = 24, align = 'center', weight = 'bold')
    )
  )

gt_table12 %>%
  gtsave(
    "4-17.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################


gt_table13 <- gt_table12 |> 
  cols_width(everything() ~ px(70))

gt_table13 %>%
  gtsave(
    "4-18.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################


gt_table14 <- gt_table13  |>
  data_color(
    columns = c(19),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) |> as.character(),
      domain = NULL
    )
  )


gt_table14 %>%
  gtsave(
    "4-19.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################




library(gtExtras)

gtextras_table1 <- gt_table14 |>
  gt_duplicate_column(취업률, dupe_name = '취업률_막대그래프', after = 취업률) |>
  cols_width(
    취업률_막대그래프 ~ px(100) 
  ) |>
  gt_plt_percentile(column = 취업률_막대그래프, scale = 100)

gtextras_table1 %>%
  gtsave(
    "4-20.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################

gtextras_table2 <- gtextras_table1 |>
  gt_color_box(columns = 3, domain = c(min(df_gt$졸업자), max(df_gt$졸업자)), palette = "ggsci::blue_material") |>
    cols_width(
      졸업자 ~ px(80) 
    )

gtextras_table2 %>%
  gtsave(
    "4-21.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################

gtextras_table3 <- gtextras_table2 |>
  gt_duplicate_column(취업자, dupe_name = '취업자_비율', after = 취업자) |>
  fmt(columns = 취업자_비율,
      fns = function(x) {
        paste0(round(prop.table(x) * 100, 1), '%')
      }) |>
  cols_merge_n_pct(취업자, 취업자_비율, autohide = TRUE) |>
  tab_spanner(columns = 19:20, label = '취업 결과') |>
  cols_label(취업률_막대그래프 = '그래프') |>
  tab_style(
    locations = cells_column_spanners(spanners = "취업 결과"),
    style     = list(
      cell_text(weight = "bold",color = "grey20", align = 'center')
    )
  ) |>
  tab_style(
    locations = cells_column_labels(columns = 19:20),
    style     = list(
      cell_text(weight = "lighter", color = 'grey50', align = 'center'), 
      "vertical-align:middle"   ## css attribute
    )
  )


gtextras_table3 %>%
  gtsave(
    "4-22.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################





gtextras_table4 <- gtextras_table3 |>
  gt_duplicate_column(column = 교외취업자) |>
  gt_duplicate_column(column = 교내취업자) |>
  gt_duplicate_column(column = 해외취업자) |>
  gt_duplicate_column(column = 농림어업종사자) |>
  gt_duplicate_column(column = 개인창작활동종사자) |>
  gt_duplicate_column(column = 일인창사업자) |>
  gt_duplicate_column(column = 프리랜서) |>
  gt_duplicate_column(column = 진학자) |>
  gt_duplicate_column(column = 입대자) |>
  gt_duplicate_column(column = 취업불가능자) |>
  gt_duplicate_column(column = 외국인유학생) |>
  gt_duplicate_column(column = 제외인정자) |>
  gt_duplicate_column(column = 기타) |>
  gt_duplicate_column(column = 미상) |>
  fmt(columns = ends_with('dupe'),
      fns = function(x) {
        paste0(round(prop.table(x) * 100, 1), '%')
      })

gtextras_table4 %>%
  gtsave(
    "4-23.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )
##########################################################################

temp <- temp1 <- gtextras_table4

for(i in 5:18) {
  temp1 <- temp |>
    cols_merge_n_pct(i, i+17, autohide = TRUE)
  temp <- temp1
}

gtextras_table5 <- temp

gtextras_table5 %>%
  gtsave(
    "4-24.pdf", expand = 10,
    path = 'C:/R/git/datavisualization/chap4/'
  )

gtextras_table5 %>%
  gtsave(
    "4-24.html", 
    path = 'C:/R/git/datavisualization/chap4/'
  )

##########################################################################
