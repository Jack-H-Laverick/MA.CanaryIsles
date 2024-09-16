
# Create an object for the compiling scripts defining an ammonia:nitrate ratio for NEMO-MEDUSA DIN 

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

user_DIN_fix <- data.frame(Depth_layer = c("Deep", "Shallow"), 
                           Proportion = c(0.165, 0.301),
                           Casts = c(24,29))                                # Values from the Brazillian shelf, why not?

saveRDS(user_DIN_fix, "./Objects/Ammonia to DIN.rds")