setwd("~/New York State Office of Information Technology Services/BWAM - Lakes Database/Current")
source("new_database/Reading.LMAS.Data.R")

library(tidyverse)

locs <- newdata %>%
  select(LAKE_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>%
  distinct()

setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend")