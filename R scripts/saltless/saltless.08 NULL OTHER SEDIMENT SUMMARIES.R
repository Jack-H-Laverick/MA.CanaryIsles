
# Create a vector of mid value D50s for use when data is lacking

#### Set up ####

rm(list=ls())

result <- data.frame(Shore = c ("Inshore", "Inshore", "Inshore", "Offshore", "Offshore", "Offshore"),
                     Habitat = c ("Silt", "Sand", "Gravel", "Silt", "Sand", "Gravel"), 
                     D50 = c(0.007826238, 0.3535534, 11.31371, 0.007826238, 0.3535534, 11.31371)) # Mid-values of sediment classes, borrowed from MiMeMo sediments.

saveRDS(result, "./Objects/Other habitat parameters.rds")
