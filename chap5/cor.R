install.packages("simstudy")
library(simstudy)
def <- defData(varname="x", formula = 10, variance = 2)
def <- defData(def, varname="y", formula = "3 - 0.8 * x", variance = 1)
dd <- genData(250, def)
dd <- trtAssign(dd, nTrt = 4, grpName = "grp", balanced = TRUE)

cor_m0.8 <- ggplot(dd) +
  geom_point(aes(x = x, y = y)) + 
  geom_smooth(aes(x = x, y = y), method = 'lm') + 
  labs(title = 'corr = -0.8')

library(patchwork)

cor_0.8 + cor_0.3 + cor_0.0 + cor_m0.8 + cor_m0.3
