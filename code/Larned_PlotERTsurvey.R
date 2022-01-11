## Larned_PlotERTsurvey.R

# load packages
library(tidyverse)
library(sf)
library(mapview) # needs to be dev version:    remotes::install_github("r-spatial/mapview")
mapviewOptions(fgb = FALSE)  # for pandoc export issue

# load data
survey <- read_csv(file.path("data", "ERT_Survey_Cleaned.csv"))

survey_sf <- st_as_sf(survey, coords = c("Longitude","Latitude"), crs=4326)

# mapview
m <-
  mapview(survey_sf, zcol='ElevationClean_m', label = "PointName")
m

# export
#Save map file
mapshot(m, file.path("docs", "ERT_Survey_Points.html"))
