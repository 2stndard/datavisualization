library(rgdal)
library(sp)
library(sf)
spdf <- geojson_read("C:/R/git/datavisualization/map/TL_SCCO_CTPRVN.json",  what = "sp")
library(tmap)
spdf <- geojson_read("C:/R/git/datavisualization/chap10/TL_SCCO_SIG.json",  what = "sp")

# Show it
plot(spdf)
sp_spdf1 <- st_make_valid(sp_spdf)
df_sf <- st_transform(sp_spdf1)

tmap::tm_shape(sp_spdf1) + 
  tm_polygons("CTPRVN_CD", palette = "viridis", legend.show = FALSE) +
  tm_text("CTP_KOR_NM")
tm_polygons("SIG_CD", palette = "viridis", legend.show = FALSE) +
  tm_text("SIG_KOR_NM")

new_cells_hex <- calculate_grid(shape = sp_spdf1, grid_type = "hexagonal", seed = 3)
resulthex <- assign_polygons(sp_spdf1, new_cells_hex)

new_cells_reg <- calculate_grid(shape = sp_spdf1, grid_type = "regular", seed = 3)
resultreg <- assign_polygons(sp_spdf1, new_cells_reg)  


write.csv(resultreg, 'resultreg.csv')
getwd()
hexplot <- tm_shape(resulthex) +
  tm_polygons("CTPRVN_CD", palette = "viridis", legend.show = FALSE) +
  tm_text("CTP_KOR_NM", size = 0.5)

regplot <- tm_shape(resultreg) +
  tm_polygons("CTPRVN_CD", palette = "viridis", legend.show = FALSE) +
  tm_text("CTP_KOR_NM")
tm_polygons("SIG_CD", palette = "viridis", legend.show = FALSE) +
  tm_text("SIG_KOR_NM", size = 0.2)

View(regplot)

tmap_arrange(hexplot, regplot, nrow = 2)






library(geofacet)

sf_spdf_seoul_joined |> ggplot(aes(variable, rank, fill = variable)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  facet_grid(~ state, grid = "kr_seoul_district_grid1")

str(kr_seoul_district_grid1)
head(state_unemp)

grid_design(data = df_seoul_grid)

df_seoul_grid$code = df_seoul_grid$name

grid_preview(df_seoul_grid)

grid_design(data = df_seoul_grid)