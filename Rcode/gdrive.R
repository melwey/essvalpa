library(googledrive)
library(googlesheets4)

googledrive::drive_auth()
sheets_auth(token = drive_token())
tmp <- read_sheet("https://docs.google.com/spreadsheets/d/1Rnt6PjZFef5uyGXOHzDyv1jnxErRTmSn_WAHmZPsmiY")
names(tmp)[c(2,3)] <- c("class","response")

library(ggplot2)
p <- ggplot(data = tmp, aes(x = class, fill = response)) + 
  geom_bar()
p

# 1. Find OAuth settings for google:
#    https://developers.google.com/accounts/docs/OAuth2InstalledApp
oauth_endpoints("google")

# 2. Register an project at https://cloud.google.com/console#/project

# 3. Navigate to API Manager, then credentials. Create a new
#    "service account key". This will generate a JSON file that you need to
#    save in a secure location. This file is equivalent to a username +
#    password pair.

token <- oauth_service_token(
  oauth_endpoints("google"),
  jsonlite::fromJSON("demo/service-account.json"),
  "https://www.googleapis.com/auth/userinfo.profile"
)

# 4. Use API
tmp <- GET("https://docs.google.com/spreadsheets/d/1Rnt6PjZFef5uyGXOHzDyv1jnxErRTmSn_WAHmZPsmiY")

f <- google_form("1Rnt6PjZFef5uyGXOHzDyv1jnxErRTmSn_WAHmZPsmiY")
