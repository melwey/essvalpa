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
    LC == 3 & Temp %in% c(4, 5) & !(Mois == 3 & LF %in% c(3, 4)) ~ "Trop_for",
    LC == 3 & Temp %in% c(1, 2, 3, 6) ~ "Temp_Bor_for",
    (LC %in% c(2, 4) | (LC == 6 & ! Temp %in% c(1,6))) &  Mois != 1 ~ "grass_range",
    Temp %in% c(3, 4, 5) & Mois == 3 & LC %in% c(2, 3, 4) & LF == 4 ~ #& COASTAL
      "mangrove",
    Temp %in% c(3, 4, 5) & Mois == 3 & LC %in% c(2, 3, 4) & LF %in% c(3, 4) ~ #& ! COASTAL ~ "swamps_floodplains",
      "swamps_floodplains",
    Mois == 1 & !c(LC %in% c(1, 5, 8)) ~ "Desert",
    Temp %in% c(1, 6) & Mois != 1 & LC %in% c(2, 4, 6) ~ "Tundra",
    LC == 8 ~ "ice_rock",
    LC == 1 ~ "cropland",
    LC == 5 ~ "urban",
    TRUE ~ "missing combination"
  ))
WEatt_tmp %>% 
  group_by(Costanza) %>%
  summarise(n())

WEatt_tmp %>% 
  filter(Costanza == "missing combination")
# the mangrove and swamps are probably much too big. Need to check extent on spaial data.
# Use CIFOR Tropical and Subtropical Wetlands dataset?

