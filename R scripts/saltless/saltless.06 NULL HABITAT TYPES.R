
# Create a vector of presumed habitat proportions, respecting the area of zones in the domain polygon

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "stars", "raster")                                             # List packages
lapply(Packages, library, character.only = TRUE)                                             # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                                              # Load SF polygons of model domain
  st_transform(crs = 4326)                                                                   # Transform to Lat/Lon to match other objects

overhang <- readRDS("./Objects/Overhang.rds") %>%                                            # Import overhang to scale proportions
  st_transform(crs = 4326) %>% 
  mutate(Habitat = "Overhang")

#### Calculate proportion of model zones in each habitat ####

Inshore <- as.numeric(st_area(filter(domains, Shore == "Inshore")))                          # Calculate the area of each zone
Overhang <- as.numeric(st_area(overhang))
Offshore <- as.numeric(st_area(filter(domains, Shore == "Offshore"))) - Overhang

total <- Inshore + Overhang + Offshore

Inshore <- Inshore / total                                                                   # Convert to proportion
Overhang <- Overhang / total
Offshore <- Offshore / total

proportions <- data.frame(Shore = c ("Inshore", "Inshore", "Inshore", "Inshore", "Offshore", "Offshore", "Offshore", "Offshore", "Offshore"),
                          Bottom = c ("Silt", "Sand", "Gravel", "Rock", "Silt", "Sand", "Gravel", "Rock", "Overhang"), 
                          Cover = c(0.1, 0.1, 0.1, 0.7, 0.7, 0.1, 0.1, 0.1, Overhang)) %>%           # Assumed weights for sediment types in each zone
  mutate(Cover = case_when(Shore == "Inshore" ~ Cover * Inshore,                             # Portion the area of each zone across habitat types
                           Shore == "Offshore" & Bottom != "Overhang" ~ Cover * Offshore,
                           Bottom == "Overhang" ~ Overhang))

saveRDS(proportions, "./Objects/Sediment area proportions.rds")
