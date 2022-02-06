library(tidyverse)
library(geojsonio)
#library(RColorBrewer)
#library(rgdal)
library(leaflet)
spdf <- NULL
spdf <- geojson_read("C:/R/git/datavisualization/map/TL_SCCO_SIG.json",  what = "sp")

detach('package:ggVennDiagram', unload = T)

head(spdf@data)
spdf_seoul <- spdf[spdf@data$SIG_CD < 12000, ]
spdf_seoul <- spdf
View(spdf_seoul)


test <- data.frame(SIG_CD = spdf_seoul@data[["SIG_CD"]], data = 1:25)

spdf_seoul_joined <- sp::merge(spdf_seoul, test, by.x="SIG_CD", by.y="SIG_CD")



sp_spdf_seoul <- st_as_sf(spdf_seoul)

ggplot() + 
  geom_sf(data = sp_spdf_seoul)

sp_spdf_seoul <- st_make_valid(sp_spdf_seoul)

## df_sf_seoul_join <- st_transform(sp_spdf_seoul_joined1)

tmap::tm_shape(sp_spdf_seoul) + 
  tm_polygons("SIG_CD", palette = "viridis", legend.show = FALSE)

new_cells_hex <- calculate_grid(shape = sp_spdf_seoul, grid_type = "hexagonal", seed = 3)
resulthex <- assign_polygons(sp_spdf_seoul, new_cells_hex)

new_cells_reg <- calculate_grid(shape = sp_spdf_seoul_joined1, grid_type = "regular", seed = 3)
resultreg <- assign_polygons(sp_spdf_seoul_joined1, new_cells_reg)  


hexplot <- tm_shape(resulthex) +
  tm_polygons("SIG_CD", palette = "viridis", legend.show = FALSE)

+
  tm_text("SIG_KOR_NM", size = 1)

regplot <- tm_shape(resultreg) +
  tm_polygons("data", palette = "viridis", legend.show = FALSE) +
  tm_text("SIG_KOR_NM")


tmap_arrange(hexplot, regplot, nrow = 2)


################################################

class(spdf)
names(spdf)
m <- leaflet(spdf) %>%
  addPolygons()


