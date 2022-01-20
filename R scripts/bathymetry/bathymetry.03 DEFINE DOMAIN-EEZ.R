
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

GEBCO <- crop(GEBCO, crop)
GFW <- crop(GFW, crop)

# #### Polygons based on depth ####

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

#### Polygons based on distance ####

Distance <- GFW
Distance[GFW == 0 | GFW > 3] <- NA  # Distance appears to be in KM not m as stated on the website.

Distance[is.finite(Distance)] <- 3  # Distance appears to be in KM not m as stated on the website.

Distance <- st_as_stars(Distance) %>% 
  st_as_sf(merge = TRUE) %>% 
  st_make_valid() %>% 
  group_by(distance.from.shore) %>% 
  summarise(Distance = (mean(distance.from.shore))) %>% 
  st_make_valid()

ggplot() +
  geom_sf(data = Distance) + 
#  geom_sf(data = Depths, aes(fill = Depth), alpha = 0.2) +
  theme_minimal() 

#### Expand inshore and cut offshore ####

# sf_use_s2(F)
# 
# inshore <- st_union(filter(Distance, Distance == 20), filter(Depths, Depth == 60)) %>% 
#   st_make_valid()
# 
# shrunk <- bind_rows(inshore, EEZ) %>%
#   st_make_valid() %>% 
#   st_difference()
# 
# ggplot() +
#   geom_sf(data = Region_mask, fill = "red") +
#   geom_sf(data = shrunk, aes(fill = Depth), alpha = 0.5)

#### Cut to region mask ####

contained <- st_cast(Distance, "POLYGON") %>%
  st_join(st_transform(Region_mask, st_crs(Distance)), st_within) %>% 
  drop_na() %>% 
  st_as_sf() %>% 
  st_union() %>% 
  #st_as_sf() %>% 
  st_sf(Shore = "Inshore") %>% 
  rename(geometry = ".")

  mutate(Shore = "Inshore")

exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), contained, "mean")


ggplot() +
  geom_sf(data = EEZ, fill = "white", size = 0.1) +
  geom_sf(data = Bottom, fill = "lightblue", size =0.1) +
  geom_sf(data = contained, fill = "red", size = 0.1) +
  theme_minimal()

ggsave("./Figures/bathymetry/EEZ.png", width = 18, height = 10, units = "cm", dpi = 700)

#### Format to domains object ####

Offshore <- st_difference(EEZ, st_transform(contained, st_crs(EEZ))) %>% 
  transmute(Shore = "Offshore") %>% 
  st_cast("POLYGON") %>% 
  mutate(area = as.numeric(st_area(.))) %>% 
  filter(area == max(area)) %>% 
  st_cast("MULTIPOLYGON")

Domains <- bind_rows(Offshore, contained) %>% 
  transmute(Shore = c("Offshore", "Inshore"),
            area = as.numeric(st_area(.)),
            Elevation = exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), ., "mean")) %>% 
  st_transform(crs = crs) 
  group_by(Shore) %>% 
  summarise(Elevation = weighted.mean(Elevation, area),
            area = sum(area))

saveRDS(Domains, "./Objects/Domains.rds")

map <- ggplot() + 
  geom_sf(data = Domains, aes(fill = Shore), colour = NA) +
#  geom_sf(data = Region_mask, colour = "red", fill = NA) + 
  geom_sf(data = world, size = 0.1, fill = "black") +
  scale_fill_manual(values = c(Inshore = "yellow", Offshore = "yellow3"), name = "Zone") +
#  zoom +
  coord_sf(xlim = st_bbox(EEZ)[c(1,3)], ylim = st_bbox(EEZ)[c(2,4)]) +
  theme_minimal() +
  #  theme(axis.text = element_blank()) +
  labs(caption = "Final model area") +
  NULL
ggsave_map("./Figures/bathymetry/Domains.png", map)

map_distance <- ggplot() +
  geom_stars(data = st_as_stars(GFW) %>% st_transform(crs)) +
  geom_sf(data = world, size = 0.1, fill = "white") +
#  zoom +
  coord_sf(xlim = st_bbox(EEZ)[c(1,3)], ylim = st_bbox(EEZ)[c(2,4)]) +
  theme_minimal() +
  NULL
ggsave_map("./Figures/bathymetry/Distance.png", map_distance)

