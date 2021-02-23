# World Ecosystems attributes
library(dplyr)
library(googledrive)
library(googlesheets4)
googledrive::drive_auth()
sheets_auth(token = drive_token())
# World Ecosystems attributes as described in Sayre et al, 2020
WEatt <- read_sheet("https://docs.google.com/spreadsheets/d/1VE-rGk6SObEGVIjiPGkbyWxia_DSnN2GaXW5STuFs7Y/edit#gid=1425520780")

# get matching between classes labels and codes

# Realms
WEatt %>%
  select(Realm_Clas, Realm_Cl_1) %>%
  group_by(Realm_Clas, Realm_Cl_1) %>%
  summarise()
# Temperature regimes
Temp <- WEatt %>%
  select(World_Temp, Temp_Class) %>%
  group_by(World_Temp, Temp_Class) %>%
  summarise()
# Moisture regimes
Mois <- WEatt %>%
  select(World_Mois, Moisture_C) %>%
  group_by(World_Mois, Moisture_C) %>%
  summarise()
# Land Cover/ Use classes
LC <- WEatt %>%
  select(World_La_1, LC_ClassNa) %>%
  group_by(World_La_1, LC_ClassNa) %>%
  summarise()
# Land forms classes
LF <- WEatt %>%
  select(World_Land, LF_ClassNa) %>%
  group_by(World_Land, LF_ClassNa) %>%
  summarise()

# apply rules to match World Ecosystems attributes to Biomes described in Costanza et al, 1997 and Costanza et al, 2014

WEatt_tmp <- WEatt %>%
  rename(LF = World_Land, Mois = World_Mois, Temp = World_Temp, LC = World_La_1) %>%
  mutate(Costanza = case_when(
    LC == 3 & Temp == 4  ~ "Subtrop_for",
    LC == 3 & Temp == 5  ~ "Trop_for", # & !(Mois == 3 & LF %in% c(3, 4))
    LC == 3 & Temp %in% c(1, 6) ~ "Bor_for",
    LC == 3 & Temp %in% c(2, 3) ~ "Temp_for",
    ((LC == 4) |
      (LC == 2 & !(Temp %in% c(2, 3) & Mois == 3 & LF %in% c(3, 4) & Realm_Clas %in% c(5,8))) |
      (LC == 6 & ! Temp %in% c(1,6))
     ) & Mois != 1 ~ "grass_range",
    # Temp %in% c(3, 4, 5) & Mois == 3 & LC %in% c(2, 3, 4) & LF == 4 ~ #& COASTAL
    #   "mangrove",
    LC == 2 & Temp %in% c(2, 3) & Mois == 3 & LF %in% c(3, 4) & Realm_Clas %in% c(5,8) ~ #& ! COASTAL ~ "swamps_floodplains",
       "swamps_floodplains",
    Mois == 1 & !c(LC %in% c(1, 5, 8)) ~ "Desert",
    Temp %in% c(1, 6) & Mois != 1 & LC %in% c(2, 4, 6) ~ "Tundra",
    LC == 8 ~ "ice_rock",
    LC == 1 ~ "cropland",
    LC == 5 ~ "urban",
    TRUE ~ "missing combination"
  )) %>%
  mutate(Costanza_Value = 
           recode(Costanza, 
                  Bor_for     = 11,
                  Temp_for    = 12,
                  Subtrop_for = 13,
                  Trop_for    = 14,
                  grass_range = 20,
                  swamps_floodplains = 32,
                  Desert      = 50,
                  Tundra      = 60,
                  ice_rock    = 70,
                  cropland    = 80,
                  urban       = 90,
                  no_data     = 40
                  ))

WEatt_tmp %>% 
  group_by(Costanza) %>%
  summarise(n())

WEatt_tmp %>% 
  filter(Costanza == "missing combination")

# create colour map for QGSI using categories created above
# define colour ramp for 10 Costanza's classes
costanza_cols <- col2rgb(
  c(cropland = "plum", Desert = "tan1", grass_range = "sienna1", ice_rock = "light blue",
    mangrove = "darkturquoise", swamps_floodplains = "turquoise1",
    Bor_for = "chartreuse",Temp_for = "chartreuse1", Trop_for = "seagreen", Subtrop_for = "seagreen1",
    Tundra = "cadetblue1", urban = "firebrick1"),
  alpha = TRUE)

# create empty file
if (!dir.exists("./legend")){
  dir.create("./legend")
}
cat(paste0("# R Generated Color Map File\n",
  "INTERPOLATION:EXACT\n"),
  file = "./legend/we_legend.txt")

for (iEco in 1:nrow(WEatt)){
  cat(paste0(WEatt_tmp[iEco,"Value"], ",", paste(costanza_cols[,WEatt_tmp[[iEco, "Costanza"]]], collapse = ","), ", ", WEatt_tmp[iEco,"Value"], "\n"),
      file = "./legend/we_legend.txt",
      append = TRUE
  )
}

# save WEatt_tmp to disk
if (!dir.exists("./tmp_output")){
  dir.create("./tmp_output")
}
save(WEatt_tmp, file ="../tmp_output/WEatt_tmp.rdata")

# Use CIFOR Tropical and Subtropical Wetlands dataset?
file.exists("../data/Tropical and Subtropical Wetlands/TROP-SUBTROP_WetlandV2_2016_CIFOR.tif")
cifor_rat <- foreign::read.dbf("../data/Tropical and Subtropical Wetlands/TROP-SUBTROP_WetlandV2_2016_CIFOR.tif.vat.dbf",as.is = TRUE)
# contains only counts per class
# classes are:
# 0, No data                  -> NA
# 10, Open water              -> 40
# 20, Mangrove                -> 31
# 30, Swamps                  -> 32
# 40, Fens                    -> 32
# 50, Riverine and lacustrine -> 32
# 60, Floodouts               -> 32
# 70, Floodplains             -> 32
# 80, Marshes                 -> 32
# 90, Wetlands in arid clim.  -> 32
# 100, Wet meadows            -> 32
cifor_Cost <- data.frame(cifor = (0:10)*10, Costanza_Value = c(NA,40, 31, rep(32, 8)))
save(cifor_Cost, file = "./tmp_output/cifor_Cost.rdata")




#