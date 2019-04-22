library(data.table)
library(plyr)
library(tidyr)
library(gtools)
library(rjson)
library(purrr)


setwd("../data")
#setwd("G:/Team Drives/APHIS  Private Data/Pathways")
#setwd("I:/Ongoing Team Projects/Exclusion Support/AQI Cooperative Agreement")
F280 <- fread("F280_CF_FY2014_to_FY2018.csv")

# Add column for data cleaning (FALSE/TRUE), FALSE - do not use, TRUE - use
F280$CLEAN <- "TRUE"

# Add notes column to document reason for omitting records, or what has been changed
F280$NOTES <- ""

# Import Plantae Kingdom taxonomy from ITIS (https://www.itis.gov/hierarchy.html)
jsonFamily <- fromJSON(file="Summaries/familyDict.json")
jsonOrder <- fromJSON(file="Summaries/orderDict.json")
jsonClass <- fromJSON(file="Summaries/classDict.json")

familyDF <- ldply(jsonFamily, data.frame, stringsAsFactors = F)
names(familyDF) <- c("Family", "Genus")

# Omit duplicate genus synonym names
genusCt <- count(familyDF$Genus)
genusCt$x <- as.character(genusCt$x)
dupGenus <- as.data.table(genusCt)[freq > 1,]

familyDF_noDup <- familyDF[!familyDF$Genus %in% dupGenus$x,]

# Merge taxonomic family data to F280 records
F280_merge <- merge(F280, familyDF_noDup, by.x="COMMODITY", by.y = "Genus", all.x = TRUE)

# Export list of genuses without family match with at least 300 records and manually specify family
missingGenus <- F280_merge[is.na(Family)][CLEAN=="TRUE"]
countGenus <- count(missingGenus$COMMODITY)
addGenus <- countGenus[countGenus$freq > 300,]
names(addGenus) <- c("Genus", "freq")
#write.csv(addGenus, "addGenus.csv")

addGenus$Family <- c("Orchidaceae","Orchidaceae","Asparagaceae","Asparagaceae","Bruniaceae","Alstroemeriaceae","Caryophyllaceae",
                     "Caryophyllaceae","Liliaceae","Mixed", "Asteraceae", "Rosaceae", "Acanthaceae", "Bruniaceae","Plumbaginaceae", "Asteraceae","Asteraceae",
                     "Asparagaceae","Caryophyllaceae","Asphodelaceae", "Asparagaceae",  "Onagraceae", "Hydrangeaceae","Proteaceae", "Amaryllidaceae", "Gentianaceae","Brassicaceae",
                     "Orchidaceae", "Apiaceae", "Asteraceae","Arecaceae", "Cyperaceae", "Orchidaceae", "Asparagaceae","Colchicaceae",
                     "Proteaceae","", "Asteraceae", "Stemonaceae", "Apocynaceae", "Campanulaceae", "Asphodelaceae")

addGenus$Genus <- as.character(addGenus$Genus)
addGenus <- addGenus[,c(1,3)]

familyDF_noDup <- rbind(familyDF_noDup, addGenus)

F280_merge <- merge(F280, familyDF_noDup, by.x = "COMMODITY", by.y = "Genus", all.x = TRUE)
missingGenus <- F280_merge[is.na(Family)][CLEAN=="TRUE"]

# Add order and class 

orderDF <- ldply(jsonOrder, data.frame, stringsAsFactors = F)
names(orderDF) <- c("Order", "Family")

classDF <- ldply(jsonClass, data.frame, stringsAsFactors = F)
names(classDF) <- c("Class", "Order")

taxoDF <- merge(orderDF, classDF, by = "Order", all.x = T)

F280_merge <- merge(F280_merge, taxoDF, by = "Family", all.x = T)
F280_merge[Family == "Mixed",]$Order <- "Mixed"
F280_merge[Family == "Mixed",]$Class <- "Mixed"

F280 <- F280_merge
F280[is.na(Family)]$Family <- ""
F280[Family == ""][CLEAN=="TRUE"]$NOTES <- "Unmatched Genus"
F280[Family == ""]$CLEAN <- "FALSE"

orderCount <- count(F280$Order)
names(orderCount) <- c("Order", "Count")
orderCount[order(-orderCount$Count),]

# Add columns for grouped disposition codes
disp_lookup <- read.csv("Summaries/disp_group_lookup.csv")
F280 <- join(F280, disp_lookup, by = "DISP_CD")

# Exclude disposition codes from analysis
F280[Include != "Y"]$CLEAN <- "FALSE"

# Which records have CFRP disposition codes but should not?
CFRP_DISP <- c("REAR", "IRAR", "DEAR", "FUAR", "OTAR", "RXAR")
CFRP_countries <- c("Colombia", "Ecuador", "Dominican Republic", "Costa Rica")
CFRP_POE <- c("TX Houston Air CBP", "GA Atlanta CBP", "NY JFK Air Cargo CBP", "NY JFK CBP", 
              "CA Los Angeles CBP", "FL Miami Air CBP", "FL Miami Air Cargo CBP", "PR San Juan Air CBP")
CFRP_commodoties <- c("Dianthus", "Liatris", "Lilium", "Rosa", "Bouquet, Rose", "Zantedeschia")
CFRP_omit <- F280[DISP_CD %in% CFRP_DISP][!(ORIGIN_NM %in% CFRP_countries)][!(LOCATION %in% CFRP_POE)][!(COMMODITY %in% CFRP_commodoties)]
F280$CLEAN[F280$F280_ID %in% CFRP_omit$F280_ID] <- "FALSE"
F280$NOTES[F280$F280_ID %in% CFRP_omit$F280_ID] <- "CFRP disp code but not in CFRP"

#CFRP_omit_summary <- CFRP_omit[,.N, by = .(ORIGIN_NM, COMMODITY)]
#write.csv(CFRP_omit_summary, "CFRP_omit_summary.csv")

# Which records have preclearance disp code but should not?
Preclear_DISP <- c("PCIR", "PCNA")
Preclear_countries <- c("Jamaica", "Chile")
# All commodities from Chile are precleared. Need to filter out some commodities from Jamaica.See CF manual pg 50
Preclear_commodities <- c("Alpinia", "Anthurium", "Croton", "Cordyline", "Cyperus","Dracaena",  "Gerbera",
                          "Gladiolus", "Heliconia", "Pandanus", "Phaeomeria", "Rosa", "Rumohra", "Strelitzia reginae")
Preclear_families <- c("Orchidaceae")
Preclear_omit <-  F280[DISP_CD %in% Preclear_DISP][!(ORIGIN_NM %in% Preclear_countries)][!(COMMODITY %in% Preclear_commodities | Family == "Orchidaceae")]
F280$CLEAN[F280$F280_ID %in% Preclear_omit$F280_ID] <- "FALSE"
F280$NOTES[F280$F280_ID %in% Preclear_omit$F280_ID] <- "Not preclearance"

#Preclear_omit_summary <- Preclear_omit[,.N, by = .(ORIGIN_NM, COMMODITY)]
#write.csv(Preclear_omit_summary, "Preclear_omit_summary.csv")

# Remove records without disposition code (n=5), pathway (35), location (1299), commodity (4), or origin (150).
# 1,423 records removed in total (some records had missing values in multiple columns)
F280[DISP_CD==""]$CLEAN <- "FALSE"
F280[PATHWAY==""]$CLEAN <- "FALSE"
F280[LOCATION==""]$CLEAN <- "FALSE"
F280[COMMODITY==""]$CLEAN <- "FALSE"
F280[ORIGIN_NM==""]$CLEAN <- "FALSE"
F280[DISP_CD==""]$NOTES <- "Missing DISP_CD"
F280[PATHWAY==""]$NOTES <- "Missing PATHWAY"
F280[LOCATION==""]$NOTES <- "Missing LOCATION"
F280[COMMODITY==""]$NOTES <- "Missing COMMODITY"
F280[ORIGIN_NM==""]$NOTES <- "Missing ORIGIN_NM"



# Change US origin to correct origin (based on guidance from APHIS). All remaining USA origin changed to clean=F
F280[ORIGIN_NM=="United States of America"][LOCATION=="MI Port Huron CBP"]$NOTES <- "Changed origin from US to Canada. Port Huron POE."
F280[ORIGIN_NM=="United States of America"][COMMODITY=="Aspidistra"]$NOTES <- "Changed origin from US to Netherlands. Aspidistra."
F280[ORIGIN_NM=="United States of America"][LOCATION=="MI Port Huron CBP"]$ORIGIN_NM <- "Canada"
F280[ORIGIN_NM=="United States of America"][COMMODITY=="Aspidistra"]$ORIGIN_NM <- "Netherlands"
F280[ORIGIN_NM=="United States of America"]$NOTES <- "USA origin"
F280[ORIGIN_NM=="United States of America"]$CLEAN <- "FALSE"

# Remove records with origin=destination, suspicious
F280[ORIGIN_NM == DEST_NM]$NOTES <- "Origin=destination"
F280[ORIGIN_NM == DEST_NM]$CLEAN <- "FALSE"

F280_clean <- droplevels(F280[CLEAN == "TRUE"])
F280_clean$PATHWAY <- as.factor(F280_clean$PATHWAY)
F280_clean$Order <- as.factor(F280_clean$Order)
F280_clean$Class <- as.factor(F280_clean$Class)

origin_ct <- F280_clean[, .(.N), by = .(ORIGIN_NM)][order(-N)]
origin_lowfreq <- unique(origin_ct[N < 500]$ORIGIN_NM)
#write.csv(origin_ct, "Q:/My Drive/Coursework/GIS 714/Project/origin_ct.csv")

regions_lookup <- read.csv("Q:/My Drive/Coursework/GIS 714/Project/subregions_geocoded.csv")
subregions <- as.data.frame(unique(regions_lookup$Subregion))
names(subregions) <- "Subregion"
subregions$source_id <- c(1:22)
regions_lookup <- join(regions_lookup, subregions, by = "Subregion")
#subregion_geocode <- geocode(as.character(regions_lookup$Subregion))
#regions_lookup$latitude_region <- subregion_geocode$lat
#regions_lookup$longitude_region <- subregion_geocode$lon
#write.csv(regions_lookup, "G:/My Drive/Coursework/GIS 714/Project/subregions_geocoded.csv")
F280_clean <- join(F280_clean, regions_lookup, by = "ORIGIN_NM")


# Sample data
# F280_sample <- sample_n(F280_clean, 100000)
# F280_sample <- separate(data = F280_sample, col = REPORT_DT, into = c("DATE", "TIME"), sep = " ")
# F280_sample$DATE <- as.Date(gsub('-', '/', F280_sample$DATE))
# F280_sample <-F280_sample %>%
#   mutate(DATE = as_datetime(format(DATE,"2017-%m-%d")))
# F280_sample$FY <- as.character(F280_sample$FY)
# 
# write.csv(F280_sample, "/home/kellyn/Desktop/pathways/F280_sample.csv")
