
# Extract monthly significant wave height

#### Setup ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "exactextractr", "raster")                    # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/land.rds") %>%                                # Import land polygon
  st_transform(crs = 4326)

rain <- brick("../Shared data/Precipitation/precipitation.nc", varname = "tp") %>% # Import significant wave height time series
  rotate()

#### Calculate mean disturbance per habitat area, weighting by cell coverage ####

values <- exact_extract(rain, domains, fun = 'mean') %>%                    # Extract rain over land    
  pivot_longer(everything(), names_to = "Date", values_to = "rain") %>%     # Move time steps into a single column
  mutate(Date = str_remove(Date, "mean.X")) %>%                             # Fix date string
  mutate(Date = as.POSIXct(Date, format = "%Y.%m.%d")) %>%                  # Format as date
  mutate(Runoff = rain * domains$area) %>%                                  # Units are m (depth) so multiply by area to get m^3  
  replace_na(list(Runoff = 0))                                              # Correct pixels which didn't contribute water for some time steps

#### Plot ####

ggplot(data = values ) + 
  geom_area(aes(x = Date, y = Runoff), fill = "blue") +
  theme_minimal() +
  labs(y = expression("Freshwater input (M"^{3}*".D"^{-1}*")"), 
       subtitle = "ERA5 total precipiration over land",
       caption = "Did we generate a time series?") +
  NULL

ggsave("./Figures/saltless/check.04.4.png", dpi = 500, width = 18, height = 10 , units = "cm")

#### Save ####

transmute(values, Month = month(Date), Year = year(Date), Runoff) %>%       # Reformat and save to slot in as river input.
  saveRDS("./Objects/River volume input.rds")
