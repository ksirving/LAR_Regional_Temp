
library(tidyverse)
library(sf)
library(mapview)
library(nhdplusTools)
# install.packages("devtools")
library(lubridate)
library(tidylog)



# Bio Data ----------------------------------------------------------------

bioData <- read.csv("ignore/Bio/SMC_bioassessment2.csv")
head(bioData)
# CSCI

csci <- bioData %>%
  select(masterid:comid, csci:csci_fieldreplicate) %>% ## only columns needed
  distinct() %>% ## remove duplicates
  separate_wider_delim(csci_sampledate, delim = "-", names = c("Year", "Month", "Day")) %>% ## separate date
  select(-Day, -Month) %>% ## remove month and day
  mutate(BioMetric = "CSCI") %>%
  rename(fieldreplicate = csci_fieldreplicate, Score = csci)
  
head(csci)
length(unique(csci$masterid)) ## 6269

# ASCI

asci <- bioData %>%
  select(masterid:comid, assemblage:asci_replicate, asci_result) %>%
  filter(assemblage =="Hybrid", metric == "ASCI") %>%
  distinct() %>%
  separate_wider_delim(asci_sampledate, delim = "-", names = c("Year", "Month", "Day")) %>% ## separate date
  select(-Day, -Month, -metric_type, -assemblage) %>% ## remove month and day
  rename(BioMetric = metric, fieldreplicate = asci_replicate, Score = asci_result)

head(asci)

BioData <- bind_rows(csci, asci)

## save 
write.csv(BioData, "ignore/Bio/00_all_bio_data.csv")

## take max score from replicate

BioData <- BioData %>%
  group_by(masterid, Year, BioMetric) %>% ## group by site and year and ffm
  mutate(MaxScore = max(Score)) %>%
  mutate(Match = ifelse(MaxScore == Score, "Yes", "No")) %>%
  filter(Match == "Yes" ) %>%
  select(-Match, -fieldreplicate)

# Temperature Data --------------------------------------------------------

TempData <- read.csv("ignore/Temp/biosites_TempMetrics.csv") 
head(TempData)

## separate column "id" to master id and comid
## format data
TempData2 <- TempData %>%
  select(id:lon, year:tmax_ab30count) %>%
  separate_wider_delim(id, delim = "_", names = c("B", "masterid", "comid"), too_many = "debug") %>% ## separate by _ 
  # - some masterids include a _ 
  separate_wider_delim(comid, delim = "-", names = c("C", "comid"), too_few = "align_start", too_many = "merge") %>% ## separate comid and COMID
  filter(!comid %in% c("Not", "-9999", "0")) %>% # remove comids that aren't comids
  mutate(masterid2 = ifelse(id_pieces == 4, paste0(masterid, "_", C), masterid)) %>% ## manually add masterids with _
  separate_wider_delim(id_remainder, delim = "-", names = c("C2","comid2"), too_few = "align_start") %>% ## separate comid-xxxx
  mutate(comid = ifelse(is.na(comid), comid2, comid)) %>% # add comids taken above to OG comid col
  select(-c(id_pieces, B, id,C, C2, comid2, masterid,id_ok, lat, lon)) %>% ## remove columns from formatting
  rename(masterid = masterid2, Year = year) %>% #rename
  mutate(Year = as.character(Year)) %>%
  pivot_longer(lst_max7rav:tmax_ab30count, names_to = "Metric", values_to = "Value") ## make metrics longer


length(unique(TempData2$masterid)) ## 1509
length(unique(TempData2$comid)) ## 1013

# Join data  --------------------------------------------------------------

head(BioData)
head(TempData2)

## match by masterid and year

allData <- left_join(TempData2, BioData, by =c("Year", "masterid")) %>%
  drop_na(Score)

## NAs are where years don't match
## remove NAs for csci

## check comids match
allData <- allData %>%
  mutate(match = ifelse(comid.x == comid.y, "Yes", "No")) %>% ## do they match
  mutate(Test = ifelse(match == "No" & comid.x == "Not recorded", comid.y, comid.x)) %>% ## if not add comid from one column when not recorded
  rename(comid = Test) %>%
  select(-c(comid.x, comid.y))

head(allData)

## save 
save(allData, file = "ignore/00_all_data.RData")


# Plot Bio--------------------------------------------------------------------

## Spatial data

## ca state
ca_sf <- st_read("ignore/SpatialData/California/Ca_State_poly.shp")
## upload ca counties

counties_sf <- st_read("ignore/SpatialData/Counties/Counties.shp")

## get only socal
counties_socal_sf<-counties_sf %>%
  filter(NAME_PCASE %in% c("Ventura","Los Angeles", "Orange","San Bernardino", "Riverside", "San Diego"))

## upload watersheds

sheds_sf<- st_read("ignore/SpatialData/SMCSheds2009/SMCSheds2009.shp")

## create beige palet

beige_pal<-c("#f2dbb7","#eed9c4","#fff0db","#e4d5b7","#d9b99b",
             "#d9c2ba","#9c8481","#e2cbb0","#a69279","#f2dbb7",
             "#f6e6bf","#a69279","#f2dbb7","#9c8481","#e4d5b7")


bio2 <- allData %>%
  dplyr::select(masterid, longitude, latitude, comid, BioMetric) %>%
  # filter(!Threshold %in% c("NATMed", "NATLow", "NATHigh")) %>%
  drop_na() %>%
  distinct()

length(unique(bio2$masterid)) ## 1482
length(unique(bio2$comid)) ## 1002

## make spatial and format channel type
bio2_sf <- bio2%>%
  st_as_sf(coords=c( "longitude", "latitude"), crs=4326, remove=F) 

## map
m1 <- ggplot()+
  geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none")+ ## beige colour for watersheds
  geom_sf(data=counties_socal_sf, fill=NA)+ ## counties
  geom_sf(data=bio2_sf,size = 1, aes(group = BioMetric, color = BioMetric)) + ## results
  # scale_colour_manual(values=c("chartreuse4", "dodgerblue2", "darkblue", "mediumpurple2", "firebrick3"))+ ## colour of points
  coord_sf(xlim=c(-119.41,-116.4), ## axis limits
           ylim=c(32.5, 34.8),
           crs=4326) +
  theme_bw()+
  # ggtitle(paste0(IndName,": ", metName)) +
  theme(legend.text=element_text(size=15),
        plot.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15)) +
  guides(col= guide_legend(title= ""))
# facet_grid(rows = vars(BioResult), cols = vars(FlowResult)) ## facet by categories

m1

file.name1 <- paste0("Figures/00_all_sites_map.jpg")
ggsave(m1, filename=file.name1, dpi=600, height=7, width=10)


