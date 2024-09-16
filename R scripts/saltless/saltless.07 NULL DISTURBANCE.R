
# Create User define habitat disturbance object

#### Set up ####

rm(list=ls())

library(tidyverse)

user_pdist <- expand.grid(Month = month.name, 
                          Habitat = c("Silt", "Sand", "Gravel"), 
                          Shore = c("Inshore", "Offshore")) %>% 
  mutate(Disturbance = 0.01)                                      # Same disturbance rate for all habitats

saveRDS(user_pdist, "./Objects/Habitat disturbance.rds")

