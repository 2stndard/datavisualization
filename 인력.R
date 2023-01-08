df_인력 <- read_excel('D:/R/git/datavisualization/plotly/RnPy/인력_149.xlsx', 
                    ## '학과별' 시트의 데이터를 불러오는데,
                    sheet = 'Sheet1',
                    ## 첫번째 행은 열 이름으로 설정
                    col_names = TRUE, 
                    ## 열의 타입을 설정, 처음 9개는 문자형으로 다음 79개는 수치형으로 설정
                    col_types = c('text', rep('numeric', 8)))

df_인력 <- df_인력 |> pivot_longer(2:9, names_to = '구분', values_to = '인원')

df_인력$연도 <- as.numeric(df_인력$연도)
View(df_인력)
################################################
model_lm <- df_인력 |> filter(구분 == '정원') |> lm(formula = 인원 ~ 연도)

result <- data.frame(인원 = predict(model_lm, newdata = data.frame(연도 = 2024:2030)))

result$연도 <- 2024:2030

result$구분 <- '정원'

summary(model_lm)

rbind(df_인력 |> filter(구분 == '정원'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = ceiling(인원)), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'lm') + 
  scale_x_continuous(breaks=seq(2017,2030, by=1))


################################################
model_loess <- df_인력 |> filter(구분 == '정원') |> loess(formula = 인원 ~ 연도)

result <- data.frame(인원 = predict(model_loess, newdata = data.frame(연도 = 2024:2030)))

result$연도 <- 2024:2030

result$구분 <- '정원'

summary(model_loess)

rbind(df_인력 |> filter(구분 == '정원'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = floor(인원)), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'loess') + 
  scale_x_continuous(breaks=seq(2017,2030, by=1))


################################################
model_lm <- df_인력 |> filter(구분 == '연구직') |> lm(formula = 인원 ~ 연도)

result <- data.frame(인원 = predict(model_lm, newdata = data.frame(연도 = 2023:2028)))

result$연도 <- 2023:2028

result$구분 <- '연구직'


rbind(df_인력 |> filter(구분 == '연구직'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = floor(인원)), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'lm') + 
  scale_x_continuous(breaks=seq(2017,2028, by=1))


################################################
model_lm <- df_인력 |> filter(구분 == '전문직') |> lm(formula = 인원 ~ 연도)

result <- data.frame(인원 = predict(model_lm, newdata = data.frame(연도 = 2023:2028)))

result$연도 <- 2023:2028

result$구분 <- '전문직'


rbind(df_인력 |> filter(구분 == '전문직'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = floor(인원)), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'lm') + 
  scale_x_continuous(breaks=seq(2017,2028, by=1))


################################################
model_lm <- df_인력 |> filter(구분 == '행정직') |> lm(formula = 인원 ~ 연도)

result <- data.frame(인원 = predict(model_lm, newdata = data.frame(연도 = 2023:2028)))

result$연도 <- 2023:2028

result$구분 <- '행정직'


rbind(df_인력 |> filter(구분 == '행정직'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = 인원), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'lm') + 
  scale_x_continuous(breaks=seq(2017,2028, by=1))



################################################
model_lm <- df_인력 |> filter(구분 == '무기출연') |> lm(formula = 인원 ~ 연도)

result <- data.frame(인원 = predict(model_lm, newdata = data.frame(연도 = 2023:2028)))

result$연도 <- 2023:2028

result$구분 <- '무기출연'


rbind(df_인력 |> filter(구분 == '무기출연'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = 인원), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'lm') + 
  scale_x_continuous(breaks=seq(2017,2028, by=1))


################################################
model_lm <- df_인력 |> filter(구분 == '무기수탁') |> lm(formula = 인원 ~ 연도)

result <- data.frame(인원 = predict(model_lm, newdata = data.frame(연도 = 2023:2028)))

result$연도 <- 2023:2028

result$구분 <- '무기수탁'


rbind(df_인력 |> filter(구분 == '무기수탁'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = 인원), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'lm') + 
  scale_x_continuous(breaks=seq(2017,2028, by=1))


################################################
model_lm <- df_인력 |> filter(구분 == '비정규') |> lm(formula = 인원 ~ 연도)

result <- data.frame(인원 = predict(model_lm, newdata = data.frame(연도 = 2023:2028)))

result$연도 <- 2023:2028

result$구분 <- '비정규'


rbind(df_인력 |> filter(구분 == '비정규'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = 인원), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'lm') + 
  scale_x_continuous(breaks=seq(2017,2028, by=1))




loess(formula = 인원 ~ 연도, data = df_인력 |> filter(구분 == '정원'))

model_loess <- df_인력 |> filter(구분 == '정원') |> loess(formula = 인원 ~ 연도)

summary(model_loess)

result <- data.frame(인원 = predict(model_loess, newdata = data.frame(연도 = 2023:2028)))

result$연도 <- 2023:2028

result$구분 <- '정원'

rbind(df_인력 |> filter(구분 == '정원'), result |> select(2, 3, 1)) |>
  ggplot(aes(x = 연도, y = 인원)) +
  geom_point() +
  geom_text(aes(label = 인원), hjust = 1, vjust = -1) + 
  geom_smooth(method = 'lm') + 
  scale_x_continuous(breaks=seq(2017,2028, by=1))
