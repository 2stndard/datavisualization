library(readxl)
library(ggupset)
showtext_opts(dpi = 96)

df_과정구분_upset <- df_취업통계

df_과정구분_upset |> distinct(학제)

df_과정구분_upset_plotdata <- df_과정구분_upset |> 
  group_by(학과명) %>% 
  mutate(학과리스트 = list(unique(sort(학제)))) %>%
  select(3:6, 8, 학과리스트) |>
  unique()

View(df_과정구분_upset_plotdata)



ggplot(df_과정구분_upset_plotdata, aes(x=학과리스트)) +
  geom_bar() +
  scale_x_upset() +
  theme_combmatrix(combmatrix.label.make_space = TRUE)


ggplot(df_과정구분_upset_plotdata, aes(x=학과리스트)) +
  geom_bar() +
  scale_x_upset(n_intersections = 10) +
  theme_combmatrix(combmatrix.label.make_space = TRUE)

#Customization
## Theming and data labels
ggplot(df_과정구분_upset_plotdata, aes(x=학과리스트)) +
  geom_bar(fill = "#549490") +
  geom_text(aes(label = ..count..), stat = 'count', vjust = -1, size = 3, color = "#549490") +
  scale_x_upset(n_intersections = 10) +
  theme_combmatrix(combmatrix.label.make_space = TRUE) +
  #  theme_minimal() +
  ylab("Number of recipes") + xlab("Combination of ingredients") +
  theme(panel.grid = element_blank())


## Add a fill aesthetic
ggplot(df_과정구분_upset_plotdata, aes(x=학과리스트, fill = 중계열)) +
  geom_bar() +
  scale_x_upset(n_intersections = 10) +
  theme_combmatrix(combmatrix.label.make_space = TRUE) +
  #  theme_minimal() +
  ylab("Number of recipes") + xlab("Combination of ingredients") +
  theme(panel.grid = element_blank())


## Change the geometry
ggplot(df_과정구분_upset_plotdata, aes(x=학과리스트)) +
  geom_jitter(aes(y = 1), width = 0.3, alpha = 0.2, size = 0.7) +
  scale_x_upset(n_intersections = 10) +
  theme_combmatrix(combmatrix.label.make_space = TRUE) +
  #  theme_minimal() +
  ylab("Number of recipes") + xlab("Combination of ingredients") +
  theme(panel.grid = element_blank())


ggplot(df_과정구분_upset_plotdata, aes(x=학과리스트, color = 중계열)) +
  geom_bar(fill = "#549490") +
  geom_text(aes(label = ..count..), stat = 'count', vjust = -1, size = 3, color = "#549490") +
  scale_x_upset(n_intersections = 10,  order_by="degree", reverse = TRUE) +
  theme_combmatrix(combmatrix.label.make_space = TRUE) +
  #  theme_minimal() +
  ylab("Number of recipes") + xlab("Combination of ingredients") +
  theme(panel.grid = element_blank())


## Customizing the combination matrix
ggplot(df_과정구분_upset_plotdata, aes(x=학과리스트)) +
  geom_bar(fill = "#549490", width = 0.6) +
  geom_text(aes(label = ..count..), stat = 'count', vjust = -1, size = 3, color = "#549490") +
  scale_x_upset(n_intersections = 10) +
  theme_minimal() +
  theme_combmatrix(combmatrix.label.make_space = TRUE, combmatrix.panel.point.size = 2, combmatrix.panel.line.size = 0.4, combmatrix.panel.point.color.fill = "#549490", combmatrix.panel.point.color.empty = "#f6e0d5", panel.grid = element_blank()) +
  ylab("Number of recipes") +
  xlab("Combination of ingredients") +
ggtitle("The most common ingredients in Greek cuisine, and how they are combined (on Yummly)")



fig0 <- theme_element + labs(title = 'theme_base()') + theme_base()
fig1 <- theme_element + labs(title = 'theme_wsj()') + theme_wsj()
fig2 <- theme_element + labs(title = 'theme_tufte()') + theme_tufte()
fig3 <- theme_element + labs(title = 'theme_stata()') + theme_stata()
fig4 <- theme_element + labs(title = 'theme_solid()') + theme_solid()
fig5 <- theme_element + labs(title = 'theme_solarized()') + theme_solarized()
fig6 <- theme_element + labs(title = 'theme_map()') + theme_map()
fig7 <- theme_element + labs(title = 'theme_igray()') + theme_igray()
fig8 <- theme_element + labs(title = 'theme_hc()') + theme_hc()
fig9 <- theme_element + labs(title = 'theme_gdocs()') + theme_gdocs()
fig10 <- theme_element + labs(title = 'theme_fivethirtyeight()') + theme_fivethirtyeight()
fig11 <- theme_element + labs(title = 'theme_few()') + theme_few()
fig12 <- theme_element + labs(title = 'theme_excel()') + theme_excel()
fig13 <- theme_element + labs(title = 'theme_economist()') + theme_economist()
fig14 <- theme_element + labs(title = 'theme_calc()') + theme_calc()

fig0 + fig1 + fig2 + fig3 + fig4 + fig5 + fig6 + fig7 + fig8 + fig9 + fig10 + fig11 + fig12 + fig13 + fig14 + plot_layout(ncol = 3)

