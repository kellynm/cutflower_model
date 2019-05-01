library(data.table)
library(plyr)
library(tidyr)
library(gtools)
library(rjson)
library(purrr)


setwd("../data")

F280 <- fread("Q:/Team Drives/APHIS  Private Data/Pathways/F280_CF_FY2014_to_FY2018.csv")

# Add column for data cleaning (FALSE/TRUE), FALSE - do not use, TRUE - use
F280$CLEAN <- "TRUE"

# Add notes column to document reason for omitting records, or what has been changed
F280$NOTES <- ""

# Change US origin to correct origin (based on guidance from APHIS). All remaining USA origin changed to clean=F
F280[ORIGIN_NM=="United States of America"][LOCATION=="MI Port Huron CBP"]$NOTES <- "Changed origin from US to Canada. Port Huron POE."
F280[ORIGIN_NM=="United States of America"][COMMODITY=="Aspidistra"]$NOTES <- "Changed origin from US to Netherlands. Aspidistra."
F280[ORIGIN_NM=="United States of America"][LOCATION=="MI Port Huron CBP"]$ORIGIN_NM <- "Canada"
F280[ORIGIN_NM=="United States of America"][COMMODITY=="Aspidistra"]$ORIGIN_NM <- "Netherlands"
F280[ORIGIN_NM=="United States of America"]$NOTES <- "USA origin"
F280[ORIGIN_NM=="United States of America"]$CLEAN <- "FALSE"

# Add POE City and State
poe <- read.csv("POE_geocoded.csv")
poe <- poe[,c(2,4,5,6,7)]
F280 <- merge(F280, poe, by='LOCATION', all.x = T)

# Add origin country lat/lon $ hemispher
origin_geocoded <- fread("origin_geocoded.csv")
origin_geocoded <- origin_geocoded[, c(2:4)]
origin_geocoded$hemisphere <- ""
origin_geocoded[latitude_origin < 0]$hemisphere <- "southern"
origin_geocoded[latitude_origin > 0]$hemisphere <- "northern"
F280 <- merge(F280, origin_geocoded, by='ORIGIN_NM', all.x = T)

# Add subregion groups for ORIGIN
regions <- read.csv("subregions_geocoded.csv")
regions <- regions[, c(2:6)]
F280 <- merge(F280, regions, by='ORIGIN_NM', all.x = T)

# For low frequency origin countries, use subregion as origin for model
origin_ct <- as.data.table(count(F280, ORIGIN_NM))
origin_low_freq <- origin_ct[n<1200]$ORIGIN_NM # This results in 52 categories for Random Forest

F280$Origin_Model <- ""
F280[ORIGIN_NM %in% origin_low_freq]$Origin_Model <- F280[ORIGIN_NM %in% origin_low_freq]$Subregion
F280[!ORIGIN_NM %in% origin_low_freq]$Origin_Model <- F280[!ORIGIN_NM %in% origin_low_freq]$ORIGIN_NM

# Add season
F280$season <- ""
F280[MON >= 3 & MON <= 5 & hemisphere == "northern"]$season <- "spring"
F280[MON >= 3 & MON <= 5 & hemisphere == "southern"]$season <- "fall"
F280[MON >= 6 & MON <= 8 & hemisphere == "northern"]$season <- "summer"
F280[MON >= 6 & MON <= 8 & hemisphere == "southern"]$season <- "winter"
F280[MON >= 9 & MON <= 11 & hemisphere == "northern"]$season <- "fall"
F280[MON >= 9 & MON <= 11 & hemisphere == "southern"]$season <- "spring"
F280[((MON == 12) | (MON >= 1 & MON <= 2)) & (hemisphere == "northern")]$season <- "winter"
F280[((MON == 12) | (MON >= 1 & MON <= 2)) & (hemisphere == "southern")]$season <- "summer"

# Add average precip and temp by month for ORIGIN
precip <- fread("country_precip_91_16.csv")
temp <- fread("country_temp_91_16.csv")

precip <- precip[,.(avg_precip=mean(`Rainfall - (MM)`)), by = .(Month, Country)]
temp <- temp[,.(avg_temp=mean(`Temperature - (Celsius)`)), by = .(Month, Country)]
climate <- merge(temp, precip, by = c("Month", "Country"), all = T)
month <- as.data.frame(unique(climate$Month))
names(month) <- 'Month'
tmp <- c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9)
month$MON <- tmp
climate <- merge(climate, month, by = 'Month', all.x = T)

# Find countries without match in climate data, edit spelling where possible to make match
#unmatched_climate <- F280[is.na(Month)]
#write.csv((unique(unmatched_climate$ORIGIN_NM)), "/media/Kellyn/F20E17B40E177139/kpmontgo@ncsu.edu/Research/APHIS_Pathways/analysis/unmatched_climate_origin.csv")
#write.csv((unique(climate$Country)), "/media/Kellyn/F20E17B40E177139/kpmontgo@ncsu.edu/Research/APHIS_Pathways/analysis/climate_countries.csv")
climate[Country=="United Kingdom"]$Country <- "United Kingdom of Great Britain and N. Ireland"
climate[Country=="United Kingdom"]$Country <- "United Kingdom of Great Britain and N. Ireland"
climate[Country=="Cote d'Ivoire"]$Country <- "Cote D`Ivoire"
climate[Country=="United States"]$Country <- "United States of America"
climate[Country=="Vietnam"]$Country <- "Viet Nam"
climate[Country=="Cook Is."]$Country <- "Cook Islands"
climate[Country=="The Gambia"]$Country <- "Gambia"
climate[Country=="St. Kitts &amp; Nevis"]$Country <- "St. Kitts and Nevis"
climate[Country=="Trinidad &amp; Tobago"]$Country <- "Trinidad and Tobago"
climate[Country=="The Bahamas"]$Country <- "Bahamas"
climate[Country=="Cayman Is."]$Country <- "Cayman Islands"
climate[Country=="Northern Mariana Is."]$Country <- "Northern Mariana Islands"
climate[Country=="Virgin Is."]$Country <- "US Virgin Islands"
climate[Country=="Solomon Is."]$Country <- "Solomon Islands"

F280 <- merge(F280, climate, by.x = c("MON", "ORIGIN_NM"), by.y = c("MON", "Country"), all.x = T)
unmatched_climate <- F280[is.na(Month)] # 2,157 records without climate match

# Import Plantae Kingdom taxonomy from ITIS (https://www.itis.gov/hierarchy.html)
jsonFamily <- fromJSON(file="familyDict.json")
jsonOrder <- fromJSON(file="orderDict.json")
jsonClass <- fromJSON(file="classDict.json")

familyDF <- ldply(jsonFamily, data.frame, stringsAsFactors = F)
names(familyDF) <- c("Family", "Genus")

# Omit duplicate genus synonym names
genusCt <- count(familyDF, Genus)
dupGenus <- as.data.table(genusCt)[n > 1,]

familyDF_noDup <- familyDF[!familyDF$Genus %in% dupGenus$x,]

# Merge taxonomic family data to F280 records
F280_merge <- merge(F280, familyDF_noDup, by.x="COMMODITY", by.y = "Genus", all.x = TRUE)

# Export list of genuses without family match with at least 300 records and manually specify family
missingGenus <- F280_merge[is.na(Family)][CLEAN=="TRUE"]
countGenus <- count(missingGenus, COMMODITY)
addGenus <- countGenus[countGenus$n > 300,]
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

orderCount <- as.data.table(count(F280, Order))
names(orderCount) <- c("Order", "Count")
orderCount[order(-orderCount$Count),]
order_low_freq <- orderCount[Count<50]$Order # This results in 49 categories for Random Forest

F280[Order %in% order_low_freq]$CLEAN <- "FALSE"
F280[Order %in% order_low_freq]$NOTES <- "Low freq order"

# Add columns for grouped disposition codes
disp_lookup <- read.csv("disp_group_lookup.csv")
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

# Remove records with origin=destination, suspicious
F280[ORIGIN_NM == DEST_NM]$NOTES <- "Origin=destination"
F280[ORIGIN_NM == DEST_NM]$CLEAN <- "FALSE"

F280_clean <- droplevels(F280[CLEAN == "TRUE"])
F280_clean$PATHWAY <- as.factor(F280_clean$PATHWAY)
F280_clean$Order <- as.factor(F280_clean$Order)
F280_clean$Class <- as.factor(F280_clean$Class)

write.csv(F280_clean, "Q:/Team Drives/APHIS  Private Data/Pathways/F280_clean.csv")

# Sample data
F280_clean <- fread("Q:/Team Drives/APHIS  Private Data/Pathways/F280_clean.csv")

#F280_2018 <- F280_clean[FY=="2018"]
F280_clean[MON == 7,sum(as.numeric(QUANTITY))]

library(lubridate)

F280_clean <- separate(data = F280_clean, col = REPORT_DT, into = c("DATE", "TIME"), sep = " ")
F280_clean$DATE <- as.Date(gsub('-', '/', F280_clean$DATE))
F280_clean <-F280_clean %>%
  mutate(DATE = as_datetime(format(DATE,"2017-%m-%d")))
F280_clean$FY <- as.character(F280_clean$FY)

F280_clean$DATE <- as.Date(F280_clean$DATE)

outcome_order <- c("AP", "PA", "PC", "CC", "PP", "NP")

require(gdata)
F280_clean$Outcome <- reorder.factor(F280_clean$Outcome, new.order=outcome_order)

#write.csv(F280_2018, "G:/Team Drives/APHIS  Private Data/Pathways/F280_2018.csv")

library(dplyr)

library(ggplot2)

# Bubble chart
set.seed(89)
F280_sample <- sample_n(F280_clean, 100000)
F280_sample <- separate(data = F280_sample, col = REPORT_DT, into = c("DATE", "TIME"), sep = " ")
F280_sample$DATE <- as.Date(gsub('-', '/', F280_sample$DATE))
F280_sample <-F280_sample %>%
  mutate(DATE = as_datetime(format(DATE,"2017-%m-%d")))
F280_sample$FY <- as.character(F280_sample$FY)

#write.csv(F280_sample, "/home/kellyn/Desktop/pathways/F280_sample.csv")

F280_clean$DATE <- as.Date(F280_clean$DATE)
F280_clean$DATE <- as.Date(F280_clean$DATE)

plot_template_F280 <- ggplot(F280_sample, aes(y=as.numeric(FY))) +
  geom_hline(yintercept = seq(2014, 2018, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2018,2014)) +
  xlab("") +
  ylab("")

F280_bubble<- plot_template_F280 +
  geom_point(aes(size = QUANTITY, x = DATE, color = Outcome),alpha=0.5)+theme(legend.position = "bottom",
  legend.box = "vertical") +guides(size = guide_legend(order = 4)) + scale_color_hue(labels = c("Actionable Pest", 
  "No Pest","Precautionary Action", "Product Contaminated", "Carrier Contaminated"))

F280_bubble

F280_clean <- as.data.table(F280_clean)[QUANTITY < 10000000,]
F280_2018 <- F280_clean[FY==2018]
F280_2018 <- droplevels(F280_2018)

# 2018 inspection outcomes quantity
ggplot(data=F280_2018, aes(x=DATE, y=QUANTITY, fill=Outcome)) + geom_col() + scale_x_date(date_breaks = "months", date_labels = "%b") +theme(legend.position = "bottom",
    legend.box = "vertical") +scale_fill_manual(values=c("red", "pink", "dodgerblue", "yellow", "lightgreen"),  labels = c("Actionable Pest","Precautionary Action", "Product Contaminated", "Product
          Prohibited", "No Pest"))+ xlab("Date") + ylab("Quantity (stems)") + ggtitle("2018 Cut Flower Shipment Inspection Outcomes")

# Inspection outcomes by order
F280_clean <- transform( F280_clean,
                       Order = ordered(Order, levels = names( sort(-table(Order)))))

ggplot(data=F280_clean, aes(x=Order, fill=Outcome)) + geom_bar(stat="count")+theme(legend.position = "bottom", legend.box = "vertical") +
    scale_fill_manual(values=c("red", "pink", "dodgerblue", "yellow", "lightgreen"),  labels = c("Actionable Pest","Precautionary Action", "Product Contaminated", "Product
    Prohibited", "No Pest"))+ xlab("Order") + ylab("Count") + ggtitle("2014-2018 Cut Flower Shipment Inspection Outcomes by Flower Order")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
