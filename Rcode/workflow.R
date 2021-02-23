# create biomes layer by
# 1. rasterize all layers at same resolution with same extent (or least same origin)
# 2. reclassify values according to:
#     100 Open ocean                      A
#     200 Shelf                           A
#     201 Estuaries                       B
#     202 Seagrass beds                   C
#     203 Coral reefs                     D
#     12 Temperate forests               E
#     11 Boreal forests                  E
#     13 Subtropical forests             E
#     14 Tropical forests                E
#     20 Grassland/Rangeland             E
#     31 Coastal wetlands/mangrove       F; G
#     32 Inland wetlands/Floodplain      F; E ((cool OR warm) temperate moist shrubland on (plains OR tablelands)) in (Palearctic 8 OR Nearctic 5)
#     40 Inland open water/lakes/rivers  E (no data is water)
#     50 Desert                          E
#     60 Tundra                          E
#     70 Ice/rock                        E
#     80 Cropland                        E
#     90 Urban                           E
#
# A DOPA ecoregions
# B Estuaries
# C Seagrass Py
# D Coral Py
# E World Ecosystems (Sayre 2020)
# F CIFOR Tropical wetlands
# G saltmarshes

# WORKFLOW
# A. TERRESTRIAL - raster
# 1. reclassify WE (incl. open water)
# 2. Resample CIFOR and reclassify
# 3. Rasterize saltmarshes
# 4. Mosaic (by topmost value)

# B. MARINE - vectors
# 1. union ecoregions by source
# 2. intersect estuaries, seagrass, coral. create new field with most valuable habitat. union
# 3. intersect with unionised ecoregions. create new field with most valuable. union.

# C. process some coastal PA to check.

library(raster)
library(tidyverse)

# A. TERRESTRIAL - raster
## 1. reclassify WE
library(raster) #
# by chunck, reclassify WE to new classes
we <- raster("../data/WorldEcosystems/Realm_WE_lzw.tif")
blockSize(we)
# execute ./Rcode/WEatt.R, which creates WEatt_tmp.rdata
if (!file.exists("./tmp_output/WEatt_tmp.rdata")){
  source("./Rcode/WEatt.R")
}
load("./tmp_output/WEatt_tmp.rdata")
rcl <- as.matrix(WEatt_tmp %>% 
                   select(Value, Costanza_Value) %>%
                   rename(is = Value, becomes = Costanza_Value) %>%
                   add_row(is = 131071, becomes = 40)
                   )
if (!file.exists("./tmp_output/we_recl_water.tif")){
  we_recl <- reclassify(we, rcl, filename = "./tmp_output/we_recl_water.tif",
                        format = "GTiff",
                        datatype = "INT1U",
                        options = c("COMPRESS=LZW"))
  ## 2.5 hours ##
  ###############
  } else{
  we_recl <- raster("./tmp_output/we_recl_water.tif")
} 

## 2. resample CIFOR
if (!file.exists("./tmp_output/cifor_recl2.tif")){
  cifor <- raster("../data/Tropical and Subtropical Wetlands/TROP-SUBTROP_WetlandV2_2016_CIFOR.tif")
  
  cifor_resampled <- resample(cifor,we_recl,
                              method = "ngb",
                              filename = "./tmp_output/cifor_resampled.tif",
                              format = "GTiff",
                              datatype = "INT1U",
                              options = c("COMPRESS=LZW"))
  # 7 hours! 
  load("./tmp_output/cifor_Cost.rdata")
  rcl_cifor <- as.matrix(cifor_Cost %>%
                           rename(is = cifor, becomes = Costanza_Value))
  cifor_recl <- reclassify(cifor_resampled, filename = "./tmp_output/cifor_recl2.tif",
                           format = "GTiff",
                           datatype = "INT1U",
                           NAflag = 0,
                           options = c("COMPRESS=DEFLATE"))
  # 1 hour
} else {
  cifor_recl <- raster("./tmp_output/cifor_recl2.tif")
}

## 3. Rasterize saltmarshes
if (!file.exists("./tmp_output/saltmarshes_raster.tif")){
  saltm_path <- "../data/WCMC_Saltmarsh_v6/01_Data/WCMC027_Saltmarshes_Py_v6.shp"
  system(
    paste(
      "gdal_rasterize -l ../data/WCMC027_Saltmarshes_Py_v6.shp",
      "-burn 31 -ts 160299.0 62171.0 -init 0.0 -a_nodata 0.0",
      "-te -179.999999 -55.99953000000001 179.999334901 83.624039629",
      "-ot Byte -of GTiff -co COMPRESS=DEFLATE -co PREDICTOR=2 -co ZLEVEL=9",
      "../data/WCMC_Saltmarsh_v6/01_Data/WCMC027_Saltmarshes_Py_v6.shp",
      "./tmp_output/saltmarshes_raster.tif")
  )
  # 5 hours
}
saltm_raster <- raster("./tmp_output/saltmarshes_raster.tif")

## 4. Merge the 3 layers: the values get priority in the same order as the arguments, but NA values are ignored 
if (!file.exists("./tmp_output/land_biomes")){
  land_biomes <- raster::merge(saltm_raster, cifor_recl2, we_recl,
                             filename = "./tmp_output/land_biomes",
                             format = "GTiff",
                             datatype = "INT1U",
                             options = c("COMPRESS=DEFLATE"),
                             overwrite = TRUE)
} else {land_biomes <- raster("./tmp_output/land_biomes")}

# B. MARINE - vectors 

## 1. Dissolve ecoregions by source in qgis
# processing.runAlgorithm('qgis:dissolve',{ 'FIELD' : ['source'], 'INPUT' : '../data/DOPA_ecoregions_2019/ecoregions_2019.gpkg|layername=ecoregions_2019', 'OUTPUT' : './tmp_output/dissolved_ecoregions.gpkg' })
# maybe better to simply rasterize spatial polygons according to source

# ### a edit attribute table: add field source_num as:
# CASE
# WHEN "source" = 'teow' THEN  40
# WHEN "source" = 'meow' THEN 200
# WHEN "source" = 'ppow' THEN 100
# WHEN "source" = 'eeow' THEN  70
# ELSE                        255
# END
# the polygons that have first_level -9998 (Lake) and -9999 (Undefined land) can't be edited and will keep source_num = NULL
# gdal_rasterize -l ecoregions_2019 -a source_num -ts 160299.0 62171.0 -init 255.0 -a_nodata 40.0 -te -179.999999 -55.99952999999999 179.999334901 83.62403962900001 -ot Byte -of GTiff ../data/DOPA_ecoregions_2019/ecoregions_2019.gpkg ./tmp_output/ecoregions_source.tif
# -- TO DO: 
# - set no data value to 255
# - reclassify 0 as 40

### !!! Marine and land Ecoregions have very different shorelines, which makes the layer unusable.
## suggestion is to use batymetry to distinguish between land, shelf and open ocean.
## remaining no data will be filled by spatial interpolation (inverse distance)
# reclassify batymetry
# [ , -100[ Open ocean
# [-100, 0[ Shelf
# [0, [     Land where need to fill in gaps.
bathymetry <- raster("/vsizip/../data/ETOPO1_Ice_g_geotiff.zip/ETOPO1_Ice_g_geotiff.tif")
# reclassify
rcl_bathy <- as.matrix(
  data.frame(
    from = c(-Inf, -100, 10),
    to = c(-100, 10, Inf),
    becomes = c(100, 200, NA)
      )
)
if (!file.exists("./tmp_output/bathy_recl_resampled.tif")){
  system.time(
  bathy_recl <- reclassify(bathymetry, rcl_baty, filename = "./tmp_output/bathy_recl.tif",
                          include.lowest = FALSE, right = TRUE,
                          format = "GTiff",
                          datatype = "INT1U",
                          options = c("COMPRESS=LZW"),
                          overwrite = TRUE
                          )
  )
  crs(bathy_recl) <- crs(we_recl)
  ## time ? ##
  ###############
  # reproject raster to 
  system.time(
  bathy_recl_resampled <- resample(bathy_recl, we_recl,
                               method = "ngb",
                               filename = "./tmp_output/bathy_recl_resampled.tif",
                               format = "GTiff",
                               datatype = "INT1U",
                               options = c("COMPRESS=LZW"))
  )
  ## time ? ##
  ############
} else{
  bathy_recl_resampled <- raster("./tmp_output/bathy_recl_resampled.tif")
} 


## 2. intersect estuaries, seagrass, coral. create new field with most valuable habitat. union
# rasterize estuaries
if (!file.exists("./tmp_output/estuaries_raster.tif")){
path_2_gdal_function <- "/Library/Frameworks/GDAL.framework/Programs/gdal_rasterize"
outRaster <- "./tmp_output/estuaries_raster.tif"
inVector <- "../data/UBC_SAU_Estuaries2003_v2/01_Data/14_001_UBC003_SAU_Estuaries2003_v2.shp"
theCommand <- sprintf("%s -burn 201 -a_nodata 0.0 -ts 160299.0 62171.0 -te -179.999999 -55.99953000000001 179.999334901 83.624039629 -ot Byte -of GTiff -co COMPRESS=DEFLATE -co PREDICTOR=2 -co ZLEVEL=9 %s  %s", path_2_gdal_function, inVector, outRaster)
system(theCommand)
} 
estuaries_raster <- raster("./tmp_output/estuaries_raster.tif")

# rasterize seagrass
if (!file.exists("./tmp_output/seagrass_raster.tif")){
inVector <- "../data/WCMC_SeagrassPtPy2018_v6/01_Data/WCMC_013_014_SeagrassesPy_v6.shp"
outRaster <- "./tmp_output/seagrass_raster.tif"
theCommand <- sprintf("%s -burn 202 -a_nodata 0.0 -ts 160299.0 62171.0 -te -179.999999 -55.99953000000001 179.999334901 83.624039629 -ot Byte -of GTiff -co COMPRESS=DEFLATE -co PREDICTOR=2 -co ZLEVEL=9 %s  %s", path_2_gdal_function, inVector, outRaster)
system(theCommand)
# drives my computer crazy...
}
seagrass_raster <- raster("./tmp_output/seagrass_raster.tif")

# rasterize coral
if (!file.exists("./tmp_output/coral_raster.tif")){
inVector <- "../data/14_001_WCMC008_CoralReefs2018_v4/01_Data/WCMC008_CoralReef2018_Py_v4.shp"
outRaster <- "./tmp_output/coral_raster.tif"
theCommand <- sprintf("%s -burn 203 -a_nodata 0.0 -ts 160299.0 62171.0 -te -179.999999 -55.99953000000001 179.999334901 83.624039629 -ot Byte -of GTiff -co COMPRESS=DEFLATE -co PREDICTOR=2 -co ZLEVEL=9 %s  %s", path_2_gdal_function, inVector, outRaster)
system(theCommand)
}
coral_raster <- raster("./tmp_output/coral_raster.tif")

## 3. mosaic all layers with most valuable ES at the top:

system.time(
all_biomes <- raster::merge(coral_raster, 
                            estuaries_raster, 
                            seagrass_raster, 
                            land_biomes,
                            bathy_recl_resampled,
                            filename = "./tmp_output/all_biomes.tif",
                            format = "GTiff",
                            datatype = "INT1U",
                            options = c("COMPRESS=DEFLATE"),
                            overwrite = TRUE)
)
# fill in holes. 2options.
# Assign $ to raster (reclassify), then calculate missing values with a inverse distance moving window
# OR
# Fill no data with a moving window: take the mode value, with weighting factor proportional to inverse distance
# e.g. weights 7x7 
# 1 1 1 1 1 1 1
# 1 2 2 2 2 2 1
# 1 2 4 4 4 2 1
# 1 2 4 0 4 2 1
# 1 2 4 4 4 2 1
# 1 2 2 2 2 2 1
# 1 1 1 1 1 1 1
source("./Rcode/inverseDistanceWeighting.m")

if (! file.exists("./tmp_output/all_biomes_filled.tif")) {
  system.time(
    all_biomes_filled <- focal(
      all_biomes,
      w = matrix(1, nc = 17, nr = 17),
      fun = inverseDistanceWeightingMode,
      na.rm = TRUE,
      NAonly = TRUE,
      filename = "./tmp_output/all_biomes_filled.tif",
      format = "GTiff",
      datatype = "INT1U",
      options = c("COMPRESS=DEFLATE")
    )
  )
} else {
  all_biomes_filled <- raster("./tmp_output/all_biomes_filled.tif")
}
# C Test on coastal site
# area
if (! file.exists("./tmp_output/r_area.tif")) {
  r_area <- area(all_biomes_filled,
               filename = "./tmp_output/r_area",
               format = "GTiff",
               options = c("COMPRESS=DEFLATE"))
} else {
  r_area <- raster("./tmp_output/r_area.tif")
}
###
# stack land_biomes, r_area
s_biomes_area <- stack(all_biomes_filled, r_area)

# load Niumi national park, The Gambia, wdpid = 2290
# UNEP-WCMC and IUCN (2020), Protected Planet: The World Database on Protected Areas (WDPA) [Online], September 2020, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.
#sp_niumi <- shapefile("../data/WDPA_Apr2020_protected_area_2290/WDPA_Apr2020_protected_area_2290-shapefile-polygons.shp")

sp_niumi <- shapefile("../data/WDPA_WDOECM_protected_area_2290_shp/WDPA_WDOECM_protected_area_2290_shp0/WDPA_WDOECM_protected_area_2290_shp-polygons.shp")
# extract values from stack
df_niumi <- raster::extract(s_biomes_area, sp_niumi, df = TRUE, cellnumbers = TRUE, along = TRUE)
# load biomes_legend
biomes_legend <- readr::read_delim("./legend/biomes_legend.txt", delim = ",",col_names = FALSE , skip = 2)
names(biomes_legend) <- c("biome_code", "R", "G", "B", "alpha", "label")
# load Costanza values per ha
costanza_vals <- readr::read_csv("./legend/costanza.csv",skip = 2)
# summarise area by biome
df <- df_niumi %>% 
  rename("land_biomes" = "all_biomes_filled") %>%
  left_join(biomes_legend %>% 
              select(biome_code, label),
            by = c("land_biomes" = "biome_code")) %>%
  group_by(land_biomes, label)                    %>%
  select(land_biomes, label, r_area)              %>%
  summarise(Area = sum(r_area, na.rm = TRUE))     %>%
  arrange(desc(Area)) %>%
  left_join(costanza_vals) %>%
  mutate(Costanza_1997 = Area * USD_ha_1997 * 1e2) %>%
  mutate(Costanza_2011 = Area * USD_ha_2011 * 1e2)

# TLS
sp_TLS_pa <- rgdal::readOGR("../data/TimorLeste/WDPA_WDOECM_TLS_shp/WDPA_WDOECM_TLS_shp-polygons.gpkg", "WDPA_WDOECM_TLS_shp-polygons")
df_TLS_pa <- raster::extract(s_biomes_area, sp_TLS_pa, df = TRUE, cellnumbers = TRUE, along = TRUE)
df_TLS_pa_es <- df_TLS_pa %>% 
  rename("land_biomes" = "all_biomes_filled") %>%
  left_join(biomes_legend %>% 
              select(biome_code, label),
            by = c("land_biomes" = "biome_code")) %>%
  group_by(ID, land_biomes, label)                    %>%
  select(ID, land_biomes, label, r_area)              %>%
  summarise(Area = sum(r_area, na.rm = TRUE))     %>%
  arrange(ID, desc(Area)) %>%
  left_join(costanza_vals) %>%
  mutate(Costanza_1997 = Area * USD_ha_1997 * 1e2) %>%
  mutate(Costanza_2011 = Area * USD_ha_2011 * 1e2)

df_TLS_pa_es_plot <- df_TLS_pa_es %>%
  ungroup() %>%
  group_by(ID) %>%
  summarise(Area_pa = sum(Area),
            USD_1997 = sum(Costanza_1997, na.rm = TRUE)/Area_pa,
            USD_2011 = sum(Costanza_2011, na.rm = TRUE)/Area_pa) %>%
  mutate(ord_ID = factor(ID, levels = order_by(ID, sort(USD_2011)))) %>%
  right_join(df_TLS_pa_es, by = "ID") 

biomes_cols <- rgb(red = biomes_legend$R/255, green = biomes_legend$G/255, blue = biomes_legend$B/255)
names(biomes_cols) <- biomes_legend$label

save(df_TLS_pa_es_plot, file = "../tls/tmp_output/df_TLS_pa_es_plot.rdata")
p <- ggplot(df_TLS_pa_es_plot,
            aes( x="", 
                 y=Area/Area_pa, 
                 fill=label,
                 width = (USD_2011))
            ) +
  geom_bar(stat="identity", width=1) +
  #coord_polar("y", start=0) +
  facet_wrap(~ID) + 
  scale_fill_manual(values = biomes_cols)
# join with wdpa info
p  

sp_TLS <- shapefile("../data/TimorLeste/TLS_EEZ_land.shp")
df_TLS <- raster::extract(s_biomes_area, sp_TLS, df = TRUE, cellnumbers = TRUE, along = TRUE)
df_TLS_es <- df_TLS %>% 
  rename("land_biomes" = "all_biomes_filled") %>%
  left_join(biomes_legend %>% 
              select(biome_code, label),
            by = c("land_biomes" = "biome_code")) %>%
  group_by(land_biomes, label)                    %>%
  select(land_biomes, label, r_area)              %>%
  summarise(Area = sum(r_area, na.rm = TRUE))     %>%
  arrange(desc(Area)) %>%
  left_join(costanza_vals) %>%
  mutate(Costanza_1997 = Area * USD_ha_1997 * 1e2) %>%
  mutate(Costanza_2011 = Area * USD_ha_2011 * 1e2)

df_TLS_es %>%
  ungroup() %>%
  summarise(Area_total = sum(Area),
            USD_1997 = sum(Costanza_1997, na.rm = TRUE)/Area_total,
            USD_2011 = sum(Costanza_2011, na.rm = TRUE)/Area_total)
save (df_TLS_es, file = "../tls/tmp_output/df_TLS_es.rdata")
sp_TLS %>% 
  as.data.frame() %>%
  dplyr::select(WDPAID, NAME, DESIG_ENG, GOV_TYPE, MARINE, GIS_AREA, GIS_M_AREA) %>%
  print()
# lc cover stats
lc <- dbf %>% dplyr::select(class_name, area_ha) %>% group_by(class_name) %>% summarise(sum(area_ha))
write_excel_csv(lc, "../data/TimorLeste/TimeorLeste_CHS_LC_stats.csv")
