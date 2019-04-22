library(ggmap)
register_google("AIzaSyAkc2RtCVd1PYSEuTU8rh3T5Tamlk_JSrE", "standard")


#F280_clean[FY==2017]
#poe <- unique(F280$LOCATION)
#write.csv(poe, "G:/My Drive/Coursework/GIS 714/Project/POE.csv")

# If grouping POE by city:
# poe <- read.csv("Q:/My Drive/Coursework/GIS 714/Project/POE_geocoded.csv")
# poe_cities <- as.data.frame(unique(poe$geocode))
# names(poe_cities) <- "poe_city"
# poe_cities$target_id <- c(23:114)
# poe <- merge(poe, poe_cities, by.x = "geocode", by.y="poe_city")
#poe <-unite(poe, geocode, c('City', 'State'), sep = ", ", remove = F)
#poe_geocode <- geocode(poe$geocode)
#poe$latitude_poe <- poe_geocode$lat
#poe$longitude_poe <- poe_geocode$lon
#write.csv(poe, "G:/My Drive/Coursework/GIS 714/Project/POE_geocoded.csv")

origin_outcome_quant <- F280_clean[,.(sum(QUANTITY)),by=.(ORIGIN_NM, Outcome)]
poe_state_outcome_quant <- F280_clean[,.(sum(QUANTITY)),by=.(State, Outcome)]
names(poe_state_outcome_quant) <- c("node", "outcome","Stems")
names(origin_outcome_quant) <- c("node", "outcome","Stems")
nodes_outcome_quant <- rbind(poe_state_outcome_quant, origin_outcome_quant)
write.csv(nodes_outcome_quant, "G:/My Drive/Coursework/GIS 714/Project/nodes_outcome_quant.csv")

# If grouping POE by state:
F280_clean$State <- unlist(map(strsplit(F280_clean$LOCATION, " "), 1))
State <- as.character(unique(F280_clean$State))
states_geocode <- geocode(State)
State <- as.data.frame(State)
State$latitude_poe <- states_geocode$lat
State$longitude_poe <- states_geocode$lon
State$target_id <- c(23:57)
F280_clean <- join(F280_clean, State, by = "State")
State$group <- "target"


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

#If grouping POE by state:
names(State) <- c("Name", "lat", "lon", "id", "group")
node_poe <- State

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