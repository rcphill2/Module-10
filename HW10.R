########################################################################################
# Summary: Module 10 HW
# Date: November 4, 2019
# Ryan Phillips
######################################################################################
#Part 1: How many potential shelter sites are located in urban areas in NC? ----

# rm(list=ls(all=TRUE))
# install.packages("sf")
# install.packages("ggpspatial")
library(tidyverse)
library(sf)
library(ggspatial)

#Read in shelter sites
shelter_sites <- read_sf("spatial_data/Potential_Emergency_Shelters.shp")
str(shelter_sites)
head(shelter_sites)
st_crs(shelter_sites)
st_transform(shelter_sites, 32119) -> shelter_sites
#Read in urban areas
urban_areas <- read_sf("spatial_data/NCDOT_Smoothed_Urban_Boundaries_simple.shp")
str(urban_areas)
head(urban_areas)
# View(urban_areas)



st_crs(urban_areas)
urban_areas <- st_transform(urban_areas, 32119)

#Use intersect to find shelters in urban areas
st_intersection(shelter_sites, urban_areas) -> urban_shelters
urban_shelters %>%
  group_by(TYPE) %>%
  count()

#Answer 1: ----
#There are 1201 shelters in urban areas across NC

#Part 2: ----- 
#How many shelters are located within 50 km of the Capital Area Metropolitan Organization? 

#Extract the capital area from urban areas 
capital <- urban_areas %>%
  filter(NAME == "Capital Area Metropolitan Organization")

#Check the area that was extracted
capital %>%
  ggplot() +
  geom_sf()

#Create buffer around CAMO
capital_buff <- st_buffer(capital, 50000)

#Find Shelters within the buffer
shelter_within <- st_intersection(shelter_sites, capital_buff)

#Find shelters within CAMO
shelter_camo <- st_intersection(shelter_sites, capital)

#Join created shelters
shelter_within %>%
  st_join(shelter_camo) -> shelters_aoi

capital_buff %>%
  ggplot() +
  geom_sf(fill = "red", alpha = 0.4, color = "red") +
  geom_sf(data = capital) +
  geom_sf(data = shelters_aoi, alpha = 0.5) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering())+
  theme_bw()

part2 <- as.numeric(nrow(shelters_aoi))
# Answer 2: -----
# 556 Shelters are located within 50 km of the CAMO


#Part 3: ------
#How many potential sheters are there per county?


#Join to boundaries

# Read in boundaries
boundaries <- st_read("spatial_data/NCDOT_County_Boundaries.shp")
str(boundaries)
st_crs(boundaries)
boundaries <- st_transform(boundaries, 32119)

county_shelters <- boundaries %>%
  st_join(shelter_sites)

View(county_shelters)

#Get number of shelters per county
shelter_counties <- county_shelters %>%
  group_by(CountyName) %>%
  count()

View(shelter_counties)

shelter_counties %>%
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c()+
  labs(color = "No. of shelters") +
  annotation_scale(location = "bl") +
  theme_bw()
  


# Extra Credit ----

View(urban_areas)
shelters_urban <- urban_areas %>%
  st_join(shelter_sites) %>%
  group_by(POP_EST) %>%
  count() %>%
  mutate(ratio = POP_EST / n)
  
View(shelters_urban)

nc_geom %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = shelters_urban, aes(fill = ratio))+
  scale_fill_viridis_c() +
  labs(fill = "Populaiton/shelter") +
  annotation_scale(location = "bl")































