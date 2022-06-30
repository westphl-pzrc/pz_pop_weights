library(tidyverse, quietly = T)
library(tidycensus, quietly = T)
library(acs, quietly = T)
library(sf, quietly = T)
library(stringr, quietly = T) #For mutating tract_names
library(tmap, quietly = T)

#I want to first figure out which of the blocks in a tract fall in the PZ,
#then take their total population, and then divide that by the tot_pop of their tract.

vars20 = load_variables(2020, "sf3", cache = T)

#Steps:
#import phl, pz as sf
phl_tracts <- read_sf("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/General Boundaries/Shapefiles/Census_Tracts",
                      "Census_Tracts", stringsAsFactors = FALSE)|>
  st_transform(crs = st_crs("EPSG:4326"))

pz <- read_sf("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/General Boundaries/Shapefiles/PZ_Shapefile",
              "PZ_Boundaries", stringsAsFactors = FALSE) |>
  st_transform(crs = st_crs("EPSG:4326"))

pz_tracts = phl_tracts[pz, ]

vars = c("P001001", #Total population
         "H013001", #Total households
         "P016002", #Total population under 18
         "P020002" #Total!!Households with one or more people under 18 years
)

#import 2020 census data by block; filter for pz
phl_block_demos = get_decennial(geography = "block", 
                                year = 2020,
                                variables = vars,
                                geometry = T, #we want this as a .shp
                                sumfile = "sf1",
                                state = "PA",
                                county = "Philadelphia",
                                output = "wide") |>
  st_transform(crs = st_crs("EPSG:4326")) |>
  mutate(tract_num = substr(GEOID, 1, 11))

pz_block_demos = phl_block_demos[pz, ]

#summarize total pop of blocks in pz by tract
pz_tract_demos_agg = pz_block_demos |>
  group_by(tract_num) |>
  summarize(blocks_tot_pop = sum(P001001),
            blocks_tot_hh = sum(H013001),
            blocks_tot_und18_pop = sum(P016002),
            blocks_tot_hh_w_kids = sum(P020002)) |>
  as.data.frame() |>
  dplyr::select(-geometry)

#pull phl demos by tract; filter for pz
phl_tract_demos = get_decennial(geography = "tract", 
                                year = 2020,
                                variables = vars,
                                geometry = T, #we want this as a .shp
                                state = "PA", # What state?
                                county = "Philadelphia", # What County?
                                output = "wide") |>
  st_transform(crs = st_crs("EPSG:4326"))

pz_tract_demos = phl_tract_demos[pz, ]

#join block tot pop by tract to tot pop per tract
pz_tract_agg_demos = left_join(pz_tract_demos_agg, 
                               pz_tract_demos,
                               by = c("tract_num" = "GEOID")) |>
  rename(tract_tot_pop = P001001,
         tract_tot_hh = H013001,
         tract_tot_und18_pop = P016002,
         tract_tot_hh_w_kids = P020002) |>
  st_as_sf(crs = st_crs("EPSG:4326"))

#calculate weights
pz_tract_agg_demos = pz_tract_agg_demos|>
  mutate(tot_pop_weight = blocks_tot_pop / tract_tot_pop,
         tot_hh_weight = blocks_tot_hh / tract_tot_hh,
         tot_und18_weight = blocks_tot_und18_pop / tract_tot_und18_pop,
         tot_hh_w_kids_weight = blocks_tot_hh_w_kids / tract_tot_hh_w_kids) 

#Currently there are two extra tracts being counted in the PZ that should not be. 
#The easiest way to do this is create a vector of tracts actually in the PZ
#and use that to filter. Since pop weights are calculated on a tract basis,
#this shouldn't cause any issues.

tm_shape(pz_tract_agg_demos) +
  tm_polygons(col = "tot_pop_weight",
              style = "cont",
              palette = "viridis") +
  tm_shape(pz) +
  tm_borders(col = "black",
             lty = "dashed")
