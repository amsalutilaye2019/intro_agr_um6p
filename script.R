#-------------------------------------------------------------------------------
# This script is used to extract the farm characteristics regarding soil, climate, 
# and topography. The data is accessed through the geodata package using respective 
# functions from open accessible databases.
#-------------------------------------------------------------------------------

#install and load libraries
packages_required <- c("terra", "geodata", "tidyverse")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#read the farm area
farm <- terra::vect("D:/UM6P/assignment/Intro_Agriculture/shp/farm2.shp")
admin1 <- terra::vect("D:/UM6P/assignment/Intro_Agriculture/shp/morocco_adm0/morocco_adm0.shp")
plot(farm)

#climate extraction
vars <- c('tmin', 'tmax', 'prec')
clim_vars <- c()
for(i in 1:length(vars)){
  print(i)
  clim <- geodata::worldclim_country("MAR", var= vars[i], path=tempdir()) |> 
    terra::extract(farm, fun = c('mean'), na.rm = T, ID = F) 
  if(vars[i] == 'prec'){
    clim_mean <- sum(clim)
  }else{
    clim_mean <- rowMeans(clim)
  }
  clim_vars <- append(clim_vars, clim_mean)
}

#soil extraction
vars <- c("phh2o",'bdod', 'soc', 'clay','silt', 'sand')
soil_vars <- c()
for(i in 1:length(vars)){
  print(i)
  soil <- soil_world(var =  vars[i], depth = 5, stat = 'mean', path=tempdir()) |> 
    terra::extract(farm, fun = 'mean', ID = F) 
  soil_vars <- append(soil_vars, soil)
}

#elevation extraction
elev <- geodata::elevation_30s(country = 'MAR', path = tempdir(), mask = TRUE) |>
  terra::extract(farm, ID = F) 
