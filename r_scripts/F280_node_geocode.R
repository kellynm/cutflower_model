library(data.table)
library(plyr)
library(tidyr)
library(gtools)
library(rjson)
library(purrr)
library(ggmap)
register_google("AIzaSyAkc2RtCVd1PYSEuTU8rh3T5Tamlk_JSrE", "standard")

setwd("../data")
#setwd("G:/Team Drives/APHIS  Private Data/Pathways")
#setwd("I:/Ongoing Team Projects/Exclusion Support/AQI Cooperative Agreement")
F280 <- fread("/media/Kellyn/F20E17B40E177139/kpmontgo@ncsu.edu/Research/APHIS_Pathways/analysis/F280_CF_FY2014_to_FY2018.csv")


#Node list for Gephi network graph:
origin_geocoded <- fread("origin_geocoded.csv")
origin_geocoded <- origin_geocoded[, c(2:4)]
origin_geocoded$id <- c(1:162)
origin_geocoded$group <- "source"
names(origin_geocoded) <- c("name", "latitude", "longitude", "id", "group")

poe <- read.csv("POE_geocoded.csv")
poe_state <- unique(poe$State)
poe_state_geocode <- geocode(as.character(poe_state))
poe_state <- as.data.frame(poe_state)
poe_state$latitude_poe <- poe_state_geocode$lat
poe_state$longitude_poe <- poe_state_geocode$lon
poe_state$id <- c(163:197)
poe_state$group <- "target"
names(poe_state) <- c("name", "latitude", "longitude", "id", "group")

nodes <- rbind(origin_geocoded, poe_state)
write.csv(nodes, "Q:/My Drive/Coursework/GIS 714/Project/Network/nodes_state_country.csv")

# Count edges for Gephi network

poe_state <- poe_state[,c(1,4)]
names(poe_state) <- c("name", "target_id")
origin_geocoded <- origin_geocoded[,c(1,4)]
names(origin_geocoded) <- c("name", "source_id")

F280_network <- merge(F280_clean, poe_state, by.x = "State", by.y = "name")
F280_network <- merge(F280_network, origin_geocoded, by.x = "ORIGIN_NM", by.y = "name")

edges_quant_2018 <- F280_network[FY==2018, .(total = sum(QUANTITY)),by=.(target_id, source_id)]
edges_count_2018 <- F280_network[FY==2018, .(.N), by=.(target_id, source_id)]

write.csv(edges_quant_2018,"Q:/My Drive/Coursework/GIS 714/Project/Network/edges_quant_2018.csv" )
write.csv(edges_count_2018,"Q:/My Drive/Coursework/GIS 714/Project/Network/edges_count_2018.csv" )

# Code used to geocode origin countries
# origin_countries <- as.data.frame(unique(F280$ORIGIN_NM))
# names(origin_countries) <- "ORIGIN_NM"
# origin_geocode <- geocode(as.character(origin_countries))
# origin_countries$latitude_origin <- origin_geocode$lat
# origin_countries$longitude_origin <- origin_geocode$lon
# write.csv(origin_countries, "origin_geocoded.csv")

# Code used to geocode subregions (Subregion grouping source: https://www.crwflags.com/fotw/flags/cou_reg.html)
# regions_lookup <- fread("/media/Kellyn/F20E17B40E177139/kpmontgo@ncsu.edu/Research/APHIS_Pathways/analysis/region_lookup.csv")
# subregion_geocode <- geocode(as.character(regions_lookup$Subregion))
# regions_lookup$latitude_region <- subregion_geocode$lat
# regions_lookup$longitude_region <- subregion_geocode$lon
# write.csv(regions_lookup, "/media/Kellyn/F20E17B40E177139/kpmontgo@ncsu.edu/Research/APHIS_Pathways/analysis/subregions_geocoded.csv")

regions_lookup <- read.csv("subregions_geocoded.csv")
subregions <- as.data.frame(unique(regions_lookup$Subregion))
names(subregions) <- "Subregion"
subregions$source_id <- c(1:23)
regions_lookup <- join(regions_lookup, subregions, by = "Subregion")

# Code used to geocode POE cities
#poe <-unite(poe, geocode, c('City', 'State'), sep = ", ", remove = F)
#poe_geocode <- geocode(poe$geocode)
#poe$latitude_poe <- poe_geocode$lat
#poe$longitude_poe <- poe_geocode$lon
#write.csv(poe, "G:/My Drive/Coursework/GIS 714/Project/POE_geocoded.csv")

# If grouping POE by city:
poe <- read.csv("POE_geocoded.csv")
poe_cities <- as.data.frame(unique(poe$geocode))
names(poe_cities) <- "poe_city"
poe_cities$target_id <- c(23:114)
poe <- merge(poe, poe_cities, by.x = "geocode", by.y="poe_city")


# Code for node visualizations
# origin_outcome_quant <- F280_clean[,.(sum(QUANTITY)),by=.(ORIGIN_NM, Outcome)]
# poe_state_outcome_quant <- F280_clean[,.(sum(QUANTITY)),by=.(State, Outcome)]
# names(poe_state_outcome_quant) <- c("node", "outcome","Stems")
# names(origin_outcome_quant) <- c("node", "outcome","Stems")
# nodes_outcome_quant <- rbind(poe_state_outcome_quant, origin_outcome_quant)
# write.csv(nodes_outcome_quant, "G:/My Drive/Coursework/GIS 714/Project/nodes_outcome_quant.csv")

# If grouping POE by state:
# F280_clean$State <- unlist(map(strsplit(F280_clean$LOCATION, " "), 1))
# State <- as.character(unique(F280_clean$State))
# states_geocode <- geocode(State)
# State <- as.data.frame(State)
# State$latitude_poe <- states_geocode$lat
# State$longitude_poe <- states_geocode$lon
# State$target_id <- c(23:57)
# F280_clean <- join(F280_clean, State, by = "State")
# State$group <- "target"
# 
# names(State) <- c("Name", "lat", "lon", "id", "group")
# node_poe <- State


# Create node table with lat/lon
node_regions <- join(subregions,regions_lookup, by="source_id", match = "first")
node_regions <- node_regions[, c(2,7, 8, 1)]
names(node_regions) <- c("Name", "lat", "lon", "id")
node_regions$group <- "source"

#If grouping POE by city:
node_poe <- join(poe_cities,poe, by="target_id", match = "first")
node_poe <- node_poe[, c(2,8, 9, 1)]
names(node_poe) <- c("Name", "lat", "lon", "id")
node_poe$group <- "target"

nodes <- rbind(node_regions, node_poe)
write.csv(nodes, "Q:/My Drive/Coursework/GIS 714/Project/nodes.csv")

#If grouping POE by city:
F280_clean <- join(F280_clean, poe, by = "LOCATION")
names(F280_clean)[39] <- "poe_city"
F280_2018_network <- F280_clean[FY == 2018, c(17, 29,34, 36, 37, 38, 39, 43, 44, 45)][Pest_found != "?"][Pest_found != "n/a"]
sample_2018 <- F280_2018_network[sample(nrow(F280_2018_network), 10000), ]

#If grouping POE by state:
F280_2018_network <- F280_clean[FY == 2018, c(17, 29,34, 36, 37, 38, 39, 40, 41, 42)][Pest_found != "?"][Pest_found != "n/a"]


F280_2018_quant <- F280_2018_network[,.(total = sum(QUANTITY)),by=.(target_id, source_id, Pest_found)]
F280_2018_count <- F280_2018_network[,.(.N),by=.(target_id, source_id, Pest_found)]

write.csv(F280_2018_quant, "Q:/My Drive/Coursework/GIS 714/Project/F280_2018_quant_edges.csv")
write.csv(F280_2018_count, "Q:/My Drive/Coursework/GIS 714/Project/F280_2018_count_edges.csv")