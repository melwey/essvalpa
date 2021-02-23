# data download
# dowload data outside essvalpa in ../data

if (!dir.exists("../data")){
  dir.create("../data")
}

# Sayre ecosystems
if (!file.exists("../data/WorldEcosystems/Realm_WE_lzw.tif")){
  # get data
}
# cifor
if(!file.exists("../data/Tropical and Subtropical Wetlands/TROP-SUBTROP_WetlandV2_2016_CIFOR.tif")){
  # get data
}

# saltmarshes
if (!file.exists("../data/WCMC_Saltmarsh_v6/01_Data/WCMC027_Saltmarshes_Py_v6.shp")){
  # get data
}

# batymetry
if (!file.exists("../data/ETOPO1_Ice_g_geotiff.zip")){
  # download data
}
