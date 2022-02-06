remotes::install_github("bbc/bbplot")
install.packages("dlstats")

library("ggplot2")
library("dlstats")

df = cran_stats(c("devtools", "remotes"))
ggplot(data = df,
       aes(x = end, 
           y = downloads, 
           group = package, 
           color = package)) +
  geom_line() + 
  geom_point(aes(shape = package)) +
  labs(x = NULL, y = "Downloads") + 
  theme_bw() + 
  theme(legend.position = c(0.1, 0.8),
        legend.background = element_rect(size = 1, color = "#000000"))


line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
  geom_line(colour = "#007f7f", size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style()

library("gapminder") # line_df 데이터
library("dplyr") # 파이프 연산자 

library("ggplot2")
library("bbplot")

line_df <- gapminder %>%
  filter(country == "Malawi") # 1

line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
  geom_line(color = "#007f7f", size = 1) +
  geom_hline(yintercept = 0, size = 1, color = "#333333") # 2
line

line + 
  bbc_style() # 3

bbc_style2 = function(font = "Helvetica"){
  ggplot2::theme(plot.title = ggplot2::element_text(family = font,
                                                    size = 28, face = "bold", color = "#222222"), 
                 plot.subtitle = ggplot2::element_text(family = font, size = 22, margin = ggplot2::margin(9, 0, 9, 0)), plot.caption = ggplot2::element_blank(),
                 legend.position = "top", legend.text.align = 0, legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(family = font, size = 18,
                                                     color = "#222222"), axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(family = font, size = 18,
                                                   color = "#222222"), axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,
                                                                                                                                    b = 10)), axis.ticks = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "white"),
                 strip.text = ggplot2::element_text(size = 22, hjust = 0))
}

line + 
  labs(title = "Title",
       subtitle = "Subtitle") +
  bbc_style2(font = NULL)
