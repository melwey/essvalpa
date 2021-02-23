# ALL_Valuation_steps
library(dplyr)
library(googledrive)
library(googlesheets4)
googledrive::drive_auth()
sheets_auth(token = drive_token())
# Valuation steps, with values of ecosystem services
ALL_value_steps <- read_sheet("https://drive.google.com/file/d/1M8pJn8RpKWjTNrHYm0DQDsJ7BXUVdhR5/view?usp=sharing")

costanza_vals <- readr::read_csv("./legend/costanza.csv",skip = 2)
