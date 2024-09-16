
#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "stars")                           # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

overhang <- readRDS("./Objects/Overhang.rds") %>%                # Import overhang to scale proportions
  st_transform(crs = 4326) %>% 
  mutate(Habitat = "Overhang")

translations <- read.csv("./Data/kml_translations.csv")          # Look up table for spanish categories to habitats

#### Combine data sets ####

sf_use_s2(FALSE)

sediment1 <- map(1:5, ~ {
  
  file <- str_glue("./Data/StrathE2E_data/Final_options/Sediments/Maps_substrate_types/Canary Islands/Canary government/kmz data /MORFOLOGIA-{.x}/doc.kml")

  layers <- st_layers(file)$name

    map <- map(layers, ~ {st_read(file, layer = .x)}) %>% 
           bind_rows() }) %>% 
  bind_rows() %>% 
  mutate(area = as.numeric(st_area(.))) %>% 
  left_join(translations) %>% 
  select(area, Habitat) %>% 
  group_by(Habitat) %>% 
  summarise(area = sum(area)) %>% 
  ungroup()

sf_use_s2(TRUE)
mask <- summarise(st_make_valid(sediment1), area = sum(area))

ggplot(sediment1) + geom_sf(aes(fill = Habitat))

sf_use_s2(FALSE)
sediment2 <- st_read("./Data/StrathE2E_data/Final_options/Sediments/Maps_substrate_types/Canary Islands/IEO/tipos-sustrato-Canarias-SIGIEO0522") %>% 
  mutate(Habitat = case_when(TIPO_FON_1 == "Arena" ~ "Sand",
                             TIPO_FON_1 == "Roca" ~ "Rock",
                             TIPO_FON_1 == "Fango" ~ "Mud",
                             TIPO_FON_1 == "Grava" ~ "Gravel"),
         area = as.numeric(st_area(.))) %>%
  st_make_valid() %>% 
  group_by(Habitat) %>% 
  summarise(area = sum(area)) %>% 
  select(Habitat) %>% 
  st_make_valid() %>% 
  st_difference(mask) 

sediment <- bind_rows(sediment1, sediment2) %>% 
#  bind_rows(sediment3) %>% 
  st_make_valid()

# rm(mask, sediment1, sediment2)
# 
# mask <- summarise(st_make_valid(sediment), area = sum(area)) %>% 
#         st_make_valid() #%>% 
# #        st_union(st_make_valid(st_read("./Data/StrathE2E_data/Final_options/Sediments/Maps_substrate_types/Canary Islands/IEO/tipos-sustrato-Canarias-SIGIEO0522")))
# 
# sediment3 <- st_read("./Data/StrathE2E_data/Final_options/Sediments/Maps_substrate_types/Canary Islands/EDMONET/EMODnet_Seabed_substrate_250k/EMODnet_Seabed_Substrate_250k_September2021/EMODnet_Seabed_substrate_250k_September2021.gdb/") %>% 
#   st_make_valid() %>% 
#   transmute(Habitat = case_when(Folk_5cl == 2 ~ "Sand",
#                                 Folk_5cl == 5 ~ "Rock",
#                                 Folk_5cl == 1 ~ "Mud",
#                                 Folk_5cl == 3 ~ "Gravel"))
# st_intersection(st_make_valid(domains)) %>% 
#   st_make_valid() %>%
#   mutate(area = as.numeric(st_area(.))) %>% 
#   group_by(Habitat) %>% 
#   summarise(area = sum(area)) %>% 
#   st_make_valid() %>% 
#   st_difference(mask) 
#
#ggplot(sediment3) + geom_sf(aes(fill = Habitat))
#ggplot(sediment3) + geom_sf(aes(fill = Folk_5cl_txt))

sf_use_s2(TRUE)

ggplot(sediment) + geom_sf(aes(fill = Habitat), size = 0)

#### Define geographic extent of each habitat type ####

polygons <- st_intersection(st_make_valid(st_transform(sediment, crs = crs)), # Split sediment polygons along model zones
                            st_transform(domains, crs = crs)) %>% 
  select(-c(Elevation, area)) %>%                                # Drop excess data
  st_transform(crs = 4326)                                       # Switch back to mercator

#### Accommodate overhang ####

ggplot() +                                                       # Check for overhang overlap with sediment data
#  geom_sf(data = sediment3, aes(fill = Folk_7cl_txt), colour = NA) +
  geom_sf(data = polygons, aes(fill = Habitat), colour = NA) +
  geom_sf(data = overhang, fill = "grey", size = 0.1, alpha = 1) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave("./Figures/saltless/Habitat gaps.png", width = 16, height = 8, units = "cm", bg = "white")

polygons <- st_difference(st_make_valid(polygons), overhang) %>% 
  select(Habitat, Shore) %>%                            
  bind_rows(overhang) %>%                                         # Add overhang polygon
  st_make_valid() %>% 
  mutate(area = as.numeric(st_area(.))) 

sf_use_s2(FALSE)
polygons2 <- st_make_valid(polygons) %>% 
  group_by(Habitat, Shore) %>%                             # Cheat way to return to "MULTIPOLYGON", and drop redundant columns
  summarise(area = sum(area)) %>%                          # Clean
  st_make_valid()
  
ggplot() +                                                       # Check ghots are removed
  geom_sf(data = polygons, aes(fill = Habitat), colour = NA) +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(polygons, "./Objects/Habitats.rds")


#### Calculate proportion of model zones in each habitat ####

proportions <- polygons %>% 
  mutate(Cover = as.numeric(st_area(.))) %>%                     # Measure the area of each habitat type
  st_drop_geometry() %>%                                         # Drop SF formatting
  mutate(Cover = Cover/sum(Cover)) %>%                           # Calculate the proportion of the model zone in each sediment polygon 
  rename(Bottom = Habitat)

saveRDS(proportions, "./Objects/Sediment area proportions.rds")

ggplot(proportions) +
  geom_col(aes(x = Shore, y = Cover*100, fill = Bottom), position = "Dodge") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top") +
  viridis::scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/saltless/Habitat types.png", width = 16, height = 8, units = "cm")

