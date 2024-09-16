
# Create an object for the compiling scripts defining 0 riverine nutrient input

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

user_river_N <- expand.grid(Month = 1:12, 
                           Year = 1980:2099,
                           NO3 = 10,
                           NH4 = 1)                                         # No river nutrient input

saveRDS(user_river_N, "./Objects/River N.rds")

