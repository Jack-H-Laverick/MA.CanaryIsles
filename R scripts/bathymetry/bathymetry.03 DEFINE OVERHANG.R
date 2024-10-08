
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster")                  # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

EEZ <- read_sf("./Data/eez")

GEBCO <- read_stars("../Shared data/GEBCO_2020.nc")
st_crs(GEBCO) <- st_crs(EEZ)
GFW <- raster("../Shared data/distance-from-shore.tif")

crop <- as(extent(-22, -9, 24, 30), "SpatialPolygons")
crs(crop) <- crs(GEBCO)

GFW <- crop(GFW, crop)

mask <- readRDS("./Objects/Domains.rds") %>%  filter(Shore == "Offshore")

#### Polygons based on depth ####

Depths <- GEBCO[EEZ] %>% 
  st_as_stars()

Depths[[1]][Depths[[1]] > units::set_units(0, "m") | Depths[[1]] < units::set_units(-600, "m")] <- NA

Depths[[1]][is.finite(Depths[[1]])] <- units::set_units(-600, "m")

Bottom <- st_as_stars(Depths) %>%
  st_as_sf(merge = TRUE) %>%
  st_make_valid() %>%
  group_by(GEBCO_2020.nc) %>%
  summarise(Depth = abs(mean(GEBCO_2020.nc))) %>%
  st_make_valid()

ggplot(Bottom) +
  geom_sf(aes(fill = Depth), alpha = 0.2) +
  theme_minimal()

#### Cut to domain ####

clipped <- st_difference(mask, st_transform(Bottom, crs = st_crs(mask)))

ggplot(clipped) +
  geom_sf(aes(fill = Depth), alpha = 0.5)

#### Format to domains object ####

overhang <- transmute(clipped, 
                      Shore = "Offshore",
                      area = as.numeric(st_area(clipped)),
                      Elevation = exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), clipped, "mean")) %>% 
  st_transform(crs = crs)

saveRDS(overhang, "./Objects/Overhang.rds")

#### Update elevations in domain polygon for extracting from NEMO-MEDUSA ####

## We need the volume calculations to be correct for exchanging water masses

Domains <- readRDS("./Objects/Domains.rds")  

GEBCO2 <- raster("../Shared data/GEBCO_2020.nc")

Elevation <- crop(GEBCO2, st_transform(mask, crs = st_crs(GEBCO2)))

Elevation_In <- Elevation ; Elevation_In[Elevation < -SDepth] <- -SDepth
Elevation_Off <- Elevation ; Elevation_Off[Elevation < -DDepth] <- -DDepth

Inshore_elevation <- exactextractr::exact_extract(Elevation_In, st_transform(filter(Domains, Shore == "Inshore"), st_crs(Elevation)), "mean")
Offshore_elevation <- exactextractr::exact_extract(Elevation_Off, st_transform(filter(Domains, Shore == "Offshore"), st_crs(Elevation)), "mean")

Domains <- mutate(Domains, Elevation = case_when(Shore == "Offshore" ~ Offshore_elevation,
                                                 Shore == "Inshore" ~ Inshore_elevation))
saveRDS(Domains, "./Objects/Domains.rds")


