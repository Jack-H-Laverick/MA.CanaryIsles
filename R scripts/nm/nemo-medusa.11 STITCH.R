
# Average the data pulled from NEMO - MEDUSA for creating decadal maps
# readRDS("./Objects/vertical boundary NORTH/.")  # Marker so network script can see where the data is being read from, it's buried in a function
# readRDS("./Objects/vertical boundary SOUTH/.")  # Marker so network script can see where the data is being read from too, it's buried in a function
# saveRDS("./Objects/vertical boundary/.")  # Marker so network script can see where the data is being saved too, it's buried in a function
# readRDS("./Objects/Months NORTH/.")  # Marker so network script can see where the data is being saved too, it's buried in a function
# readRDS("./Objects/Months SOUTH/.")  # Marker so network script can see where the data is being saved too, it's buried in a function
# saveRDS("./Objects/Months/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                                # Wipe the brain

library(MiMeMo.tools)             
library(furrr)
plan(multisession)                                                           # Instructions for parallel processing

#### Combine North and South grids for volumes ####

North <- list.files("./Objects/Months NORTH/", full.names = T)
South <- list.files("./Objects/Months SOUTH/", full.names = T)

future_map2(North, South, ~{                                                 # For each paired set of results
  
south <- readRDS(.y)
 
  both <- anti_join(readRDS(.x), south, by = c("latitude", "longitude")) %>% # Find the non-duplictaed entries from the Norht
    mutate(Chunk = "North",                                                  # Label
           y = y + max(south$y) - min(y) + 1) %>%                            # Update the grid position for quick plotting
    bind_rows(mutate(south, Chunk = "South")) %>%                            # Add to labelled southern data
    select(-Chunk) %>%                                                       # Drop data label (comment this if you want to check on a test)
    saveRDS(str_remove(.x, " NORTH"))                                        # Save out spatial file in the folder above WD
}, .progress = TRUE) 
  
#### Combine North and South grids for vertical boundaries ####

North <- list.files("./Objects/vertical boundary NORTH/", full.names = T)
South <- list.files("./Objects/vertical boundary SOUTH/", full.names = T)

future_map2(North, South, ~{                                                 # For each paired set of results
  
  south <- readRDS(.y)
  
  both <- anti_join(readRDS(.x), south, by = c("latitude", "longitude")) %>% # Find the non-duplictaed entries from the Norht
    mutate(Chunk = "North",                                                  # Label
           y = y + max(south$y) - min(y) + 1) %>%                            # Update the grid position for quick plotting
    bind_rows(mutate(south, Chunk = "South")) %>%                            # Add to labelled southern data
    select(-Chunk) %>%                                                       # Drop data label (comment this if you want to check on a test)
    saveRDS(str_remove(.x, " NORTH"))                                        # Save out spatial file in the folder above WD
}, .progress = TRUE) 

#### Combine North and South grids for Overhang ####

North <- list.files("./Objects/overhang NORTH/", full.names = T)
South <- list.files("./Objects/overhang SOUTH/", full.names = T)

future_map2(North, South, ~{                                                 # For each paired set of results
  
  south <- readRDS(.y)
  
  both <- anti_join(readRDS(.x), south, by = c("latitude", "longitude")) %>% # Find the non-duplictaed entries from the Norht
    mutate(Chunk = "North",                                                  # Label
           y = y + max(south$y) - min(y) + 1) %>%                            # Update the grid position for quick plotting
    bind_rows(mutate(south, Chunk = "South")) %>%                            # Add to labelled southern data
    select(-Chunk) %>%                                                       # Drop data label (comment this if you want to check on a test)
    saveRDS(str_remove(.x, " NORTH"))                                        # Save out spatial file in the folder above WD
}, .progress = TRUE) 
