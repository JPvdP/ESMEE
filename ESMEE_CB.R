#  This script prepares the Crunchbase data to search for patents and publications

# Packages used
library(tidyverse) # general data stuff
library(splitstackshape) # splitter for dataframes
library(rjson) # for the retrieval of webdata
library(mapview) # make a map

# data was extracted on the 31st of may 2023
# Filter applied: Industry - Energy (all 17)
#                 Industry - Agriculture and farming (all 10)
#                 Time [founded] - 01/01/2010 ~ 31/05/2023
#                 Location - Netherlands, Europe

load("~/Desktop/Projects/ESMEE/ESMEE_CB_Energy_Agri_2010_2023_NL_31052023.rdata")

#####
##### 1. Geolocalisation of the companies
#####

# The CB data contains the city, region and country of the headquarters of the company. 
# We need to extract - the region for grouped statistics per region
#                    - the city for an more precise mapping of the companies and other elements

##### 1.1 extract the city and the region

CB_geoloc = ESMEE_CB_Energy_Agri_2010_2023_NL_31052023[,c(3,13)] # we use the name as identifier
CB_geoloc = splitstackshape::cSplit(CB_geoloc, 2, sep = ", ")

##### 1.2 geolocate the cities, i.e find longitude and latitude to make maps easier to make

# no need to search for one city twice, let's save some computation time 
# we first make a list of cities to search for
CB_cities = as.matrix(unique(CB_geoloc[,2]))
CB_cities = cbind(CB_cities, "Longitude", "Latitude")
colnames(CB_cities) = c("City", "Lon", "Lat")

# some cities have a space in the name which creates an issue for the creation of the URL
# we can replace this with a "-" so that openstreetmaps still knows which city it is
CB_cities[,1] <- gsub(" ", "-", CB_cities[,1])
# we do the same to the initial dataframe so we can match the information quickly
CB_geoloc$`Headquarters Location_1_adjusted` = gsub(" ", "-", CB_geoloc$`Headquarters Location_1` )

# Loop over all the cities and searc for the coordinates on openstreetmap
for(i in 53:dim(CB_geoloc)[1]){
  # build the URl to search for
  query = paste0("https://nominatim.openstreetmap.org/search?q=", CB_cities[i,1],"&format=json&polygon_geojson=1&addressdetails=1")
  # retrieve the data
  plop = fromJSON(file=query)
  # Extract relevant information 
  CB_cities[i,2] = plop[[1]][["lon"]]
  CB_cities[i,3] = plop[[1]][["lat"]]
}

# Save the data for good measure
save(CB_cities, file="ESMEE_CB_cities_geocoordinates.rdata")

# Now we match the city coordinates back to the initial dataframe
CB_geoloc$Longitude = CB_cities[,2][match(CB_geoloc$`Headquarters Location_1_adjusted`, CB_cities[,1])]
CB_geoloc$Latitude = CB_cities[,3][match(CB_geoloc$`Headquarters Location_1_adjusted`, CB_cities[,1])]

# add to the initial complete dataframe for safeguarding
ESMEE_CB_Energy_Agri_2010_2023_NL_31052023 = left_join(ESMEE_CB_Energy_Agri_2010_2023_NL_31052023, CB_geoloc, by = "Organization Name")
# export the data for safeguarding
save(ESMEE_CB_Energy_Agri_2010_2023_NL_31052023, file ="~/Desktop/Projects/ESMEE/ESMEE_CB_Energy_Agri_2010_2023_NL_31052023_geolocated.rdata")


#### 1.3 Compute information per region



#####
##### 2. Make a nice map
#####


CB_geoloc$Longitude = as.numeric(CB_geoloc$Longitude)
CB_geoloc$Latitude = as.numeric(CB_geoloc$Latitude)

NLD <- read_sf("NLD_shapefile/NLD_adm1.shp")
mapview(NLD)
mapview(CB_geoloc, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)

 