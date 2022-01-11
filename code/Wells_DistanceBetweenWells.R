## Wells_DistanceBetweenWells.R

## prep workspace
source(file.path("code", "paths+packages.R"))

# load well info
well_info <- 
  file.path(dir_ghfiles, "data", "survey", "LarnedWellInfo.csv") %>% 
  readr::read_csv() %>% 
  subset(Status == "Active")

# convert to sf object
sf_wells <- sf::st_as_sf(well_info, coords = c("lon", "lat"), crs = 4326)
ggplot(sf_wells) + geom_sf()

# distance between all wells
dist <- sf::st_distance(sf_wells)
rownames(dist) <- sf_wells$Well
colnames(dist) <- sf_wells$Well
write.csv(dist, file = file.path(dir_ghfiles, "data", "survey", "processed", "DistanceBetweenWells.csv"))
