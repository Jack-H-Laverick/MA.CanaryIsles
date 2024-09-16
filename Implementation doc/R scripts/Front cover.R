
##**## Copy rayshader low script
##**## Change render_snapshot to render_snapshot("./Implementation doc/img/Rayshade_1.png")
##**## Change window size to 1161
##**## add background = "black", shadowcolor = "grey50" to plot_3D

#### Copied script ####

rm(list=ls())                                                                       # Wipe the brain

packages <- c("tidyverse", "data.table", "ncdf4", "stars", "rayshader", "tictoc")   # List packages
lapply(packages, library, character.only = TRUE)                                    # Load packages

nc_raw <- nc_open("../Shared data/GEBCO_2020.nc")                                   # Access GEBCO bathymetry
nc_lat <- ncvar_get(nc_raw, "lat")                                                  # Extract the latitudes
nc_lon <- ncvar_get(nc_raw, "lon")                                                  # Extract the longitudes
nc_close(nc_raw)                                                                    # You must close an open netcdf file when finished to avoid data loss
rm(nc_raw)                                                                          # Drop the file

#### Extract Area ####

S <- nrow(nc_lat)*(90-39)/180 ; N <- nrow(nc_lat)*(90-28)/180
W <- length(nc_lon)*(180+12)/360 ; E <- length(nc_lon)*(180+30)/360     # For Mercatore

Bathymetry <- read_ncdf("../Shared data/GEBCO_2020.nc", ncsub = cbind(
  start = c(W, S), count =c((E-W+1), (N-S+1)))) 

plot(Bathymetry)

matrix <- Bathymetry$elevation %>% as.numeric() %>% 
  matrix(nrow = nrow(Bathymetry$elevation), ncol= ncol(Bathymetry$elevation))

# 8192 x 8192 maximum textured syrface allowed by RGL
mat <- matrix[seq(nrow(matrix), 1, by = -1),]                               # Use for full resolution, divide zscales by 10                
#mat <- matrix                               # Use for full resolution, divide zscales by 10                

#### Plot area ####

montshadow = ray_shade(mat, zscale = 0.1, lambert = FALSE, multicore = TRUE)
montamb = ambient_shade(mat, zscale = 5, multicore = TRUE)

mat_col <- mat %>%
  sphere_shade(zscale = 1, texture = "imhof2") %>%
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb)
  
plot_3d(mat_col, mat, zscale = 20, fov = 0, theta = 150, phi = 30, 
        windowsize = 1161, zoom = 0.9,    #3720 0.55   # original values when resolution was somehow higher
        water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
        waterlinecolor = "white", waterlinealpha = 0.5,
        background = "black", shadowcolor = "grey50", 
          asp = 1/cospi(-35/180)) 
#Sys.sleep(30)                                                                        # Pause for RGL to open
render_snapshot("./Implementation doc/img/Rayshade_1.png")                            # Save the current view in the RGL window

#### Set up ####

library(magick)

image_read("./Implementation doc/img/Rayshade_1.png") %>% 
  image_border("#000000", "150x520") %>%                                           # Extend the black border
  image_crop("1061x1482", gravity = "NorthWest") %>%                                # Lop off the bottom to shift the render down
  image_write("./Implementation doc/img/Front.png")
