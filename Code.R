library(tidyverse)
library(arrow)
library(sf)
library(tmap)

# 1. Overture Data --------------------------------------------------------

## Overture UK data - available from https://dagshub.com/cjber/overture-uk/src/1c0534ca8cf86141e91afa209f061cda6234518c/data/processed/uk_places_admin.parquet. 
places <- open_dataset("uk_places_admin.parquet")

# 2. Retail Centre Boundaries ---------------------------------------------

## Retail centre boundaries for the UK - available from https://data.cdrc.ac.uk/dataset/retail-centre-boundaries-and-open-indicators. 
rc <- st_read("Retail_Boundaries_UK/Retail_Boundaries_UK.gpkg")

# 3. OpenStreetMap Data ---------------------------------------------------

## OpenStreetMap data for Newcastle - available at http://download.geofabrik.de/europe/great-britain/england/tyne-and-wear.html. 
buildings <- st_transform(st_read("tyne-and-wear-latest-free/gis_osm_buildings_a_free_1.shp"), 27700)
roads <- st_transform(st_read("tyne-and-wear-latest-free/gis_osm_roads_free_1.shp"), 27700)

# 3. Map Data -------------------------------------------------------------

## Get Greggs locations from Overture
greggs <- places %>%
  filter(grepl("Greggs", names_value)) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("lat", "lng")) %>%
  st_set_crs(4326) %>%
  st_transform(27700) %>%
  select(names_value, brand_name_value)

## Newcastle City Centre (Retail Centre Boundary)
rc_newc <- rc %>%
  filter(RC_Name == "Newcastle City; Newcastle upon Tyne (North East; England)")

## Build a buffer
rc_cent <- st_centroid(rc_newc)
bbox <- st_buffer(rc_cent, 1500)

## Clip greggs to boundary
greggs_newc <- st_intersection(greggs, bbox)

## Clip OSM features
buildings_newc <- st_intersection(buildings, bbox)
roads_newc <- st_intersection(roads, bbox)

# 4. Map ------------------------------------------------------------------

## Map 1 - Greggs distribution in Newcastle City Centre
map <- tm_shape(bbox) +
  tm_fill(col = NA, alpha = 0.05) +
  tm_shape(rc_newc) +
  tm_polygons("#035690", alpha = .75, border.col = "#035690") +
  tm_shape(buildings_newc) +
  tm_fill(col = "gray", alpha = 0.3) +
  tm_shape(roads_newc) +
  tm_lines(col = "gray", alpha = .3) +
  tm_shape(greggs_newc) +
  tm_dots(col = "#FAB622", size = 1.5, alpha = 1, border.col = "black") +
  tm_text("names_value", col = "white", just = "right", shadow = TRUE, size = 1.25) +
  tm_credits("Data: Overture Places, CDRC Retail Centre Boundaries and OpenStreetMap", col = "white", size = .75, 
             position = c("left", "bottom")) +
  tm_logo("https://assets.stickpng.com/images/5a1d2f624ac6b00ff574e297.png", position = c("right", "bottom")) +
  tm_layout(frame = FALSE, bg.color = "black",
            title = "Newcastle really is the home of Greggs!", title.size = 1.5,
            fontfamily = "sans", title.fontface = "bold", 
            title.color = "white",
            legend.outside = FALSE)
tmap_save(map, "map.png", dpi = 300)

