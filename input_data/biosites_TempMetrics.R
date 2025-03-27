#####################################################################################################
### Packages ###
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(fs)
#install.packages("Microsoft365R")
library(Microsoft365R)
#####################################################################################################
### Data ###
##filenames<- list.files(path = "C:/Users/gisuser/SCCWRP/Staff - P Drive/Data/PartTimers/Megan Warren/temp metrics/biosites_output")
##numfiles<- length(filenames)

#option to pull directly from sharepoint, requires approval
##sp<- get_sharepoint_site("https://sccwrp.sharepoint.com/sites/TempModelling/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FTempModelling%2FShared%20Documents%2FGeneral%2FData&p=true&ga=1")

#note directory where all csvs are stored
directory_for_files <- "C:/Users/gisuser/SCCWRP/Staff - P Drive/Data/PartTimers/Megan Warren/temp metrics/biosites_output/"

#create list from directory for all files, may need to specify column type if there are empty cells within dataset
biosite_list <- directory_for_files %>% 
  dir_ls() %>% 
  map(
    .f = function(path) {
      read_csv(path)
    }
  )

#create dataframe of all sites
biosite_tbl <- biosite_list %>% 
  set_names(dir_ls(directory_for_files)) %>% 
  bind_rows(.id = "file.path")

#####################################################################################################
### Rolling Temp Metrics ###

#Calculating rolling values
biosite_tbl_2<- biosite_tbl %>% 
  mutate(temp.mod_rav = rollmean(temp.mod, k = 7, fill = NA)) %>% 
  mutate(temp.mod_rmx = rollmax(temp.mod, k = 7, fill = NA)) %>% 
  mutate(temp.mod_rmn = -rollmax(-temp.mod, k = 7, fill = NA)) %>%       #-rollmax = rolling min
  mutate(temp.max_rav = rollmean(temp.max, k = 7, fill = NA)) %>% 
  mutate(temp.max_rmx = rollmax(temp.max, k = 7, fill = NA)) %>% 
  mutate(temp.max_rmn = -rollmax(-temp.max, k = 7, fill = NA)) %>% 
  mutate(lst_rav = rollmean(lst, k = 7, fill = NA),
         humidity.rav = rollmean(humidity, k = 7, fill = NA),
         temp.anom.rav = rollmean(temp.anom, k = 7, fill = NA),
         temp.plus.rav = rollmean(temp.plus, k = 7, fill = NA),
         temp.doy.rav = rollmean(temp.doy, k = 7, fill = NA))

rm(biosite_list)
rm(directory_for_files)

#Calculating metrics using rolling values

 #table 3:metric values for each season within each year
biosite_tbl_3<- biosite_tbl_2 %>% 
  mutate(year = year(date),                                              #extract year/month from date to group by year/season
         season = month(date)) %>% 
  mutate(season = case_when(season <= 2 | season == 12 ~ "winter",       #convert month numbers into seasons
                            season >= 3 & season <= 5 ~ "spring",
                            season >= 6 & season <= 8 ~ "summer",
                            season >= 9 & season <= 11 ~ "fall")) %>%  
  group_by(id, year, season) %>% 
  mutate(lst_max7rav = max(lst_rav, na.rm = T),                          #max value of rolling 7-day average
         humidity_max7rav = max(humidity.rav, na.rm = T),
         temp.anom_max7rav = max(temp.anom.rav, na.rm = T),
         temp.plus_max7rav = max(temp.plus.rav, na.rm = T),
         temp.doy_max7rav = max(temp.doy.rav, na.rm = T),               #temp.mod and temp.max metrics below
         tmod_min7rmn = min(temp.mod_rmn, na.rm = T),                    #the minimum value of a rolling 7-day minimum
         tmax_min7rmn = min(temp.max_rmn, na.rm = T),
         tmod_max7rmx = max(temp.mod_rmx, na.rm = T),                    #the maximum value of a rolling 7-day maximum
         tmax_max7rmx = max(temp.max_rmx, na.rm = T),
         tmod_max7rav = max(temp.mod_rav, na.rm = T),                    #the max value of rolling 7-day average
         tmax_max7rav = max(temp.max_rav, na.rm = T),
         tmod_diff = temp.mod_rmx - temp.mod_rmn,                        #difference between rolling 7-day min and max
         tmax_diff = temp.max_rmx - temp.max_rmn, 
         tmod_maxdiff = max(tmod_diff, na.rm = T),                       #max difference between rolling 7-day min and max
         tmax_maxdiff = max(tmax_diff, na.rm = T),
         tmod_avdiff = mean(tmod_diff, na.rm = T),                       #average difference between rolling 7-day min and max
         tmax_avdiff = mean(tmax_diff, na.rm = T),
         tmod_above30 = if_else(temp.mod_rmx > 30, 1, 0),                #the number of 7-day rolling averages that are over 30
         tmod_ab30count = sum(tmod_above30, na.rm = T),
         tmax_above30 = if_else(temp.max_rmx > 30, 1, 0),
         tmax_ab30count = sum(tmax_above30, na.rm = T))

rm(biosite_tbl)

 #table 4:metric values for each year over all seasons
biosite_tbl_4<- biosite_tbl_2 %>% 
  mutate(year = year(date),                                              #extract year from date to group by year
         season = "all") %>%  
  group_by(id, year) %>% 
  mutate(lst_max7rav = max(lst_rav, na.rm = T),
         humidity_max7rav = max(humidity.rav, na.rm = T),
         temp.anom_max7rav = max(temp.anom.rav, na.rm = T),
         temp.plus_max7rav = max(temp.plus.rav, na.rm = T),
         temp.doy_max7rav = max(temp.doy.rav, na.rm = T),               #temp.mod and temp.max metrics below
         tmod_min7rmn = min(temp.mod_rmn, na.rm = T),                    #the minimum value of a rolling 7-day minimum
         tmax_min7rmn = min(temp.max_rmn, na.rm = T),
         tmod_max7rmx = max(temp.mod_rmx, na.rm = T),                    #the maximum value of a rolling 7-day maximum
         tmax_max7rmx = max(temp.max_rmx, na.rm = T),
         tmod_max7rav = max(temp.mod_rav, na.rm = T),                    #the max value of rolling 7-day average
         tmax_max7rav = max(temp.max_rav, na.rm = T),
         tmod_diff = temp.mod_rmx - temp.mod_rmn,                        #difference between rolling 7-day min and max
         tmax_diff = temp.max_rmx - temp.max_rmn, 
         tmod_maxdiff = max(tmod_diff, na.rm = T),                       #max difference between rolling 7-day min and max
         tmax_maxdiff = max(tmax_diff, na.rm = T),
         tmod_avdiff = mean(tmod_diff, na.rm = T),                       #average difference between rolling 7-day min and max
         tmax_avdiff = mean(tmax_diff, na.rm = T),
         tmod_above30 = if_else(temp.mod_rmx > 30, 1, 0),                #the number of 7-day rolling averages that are over 30
         tmod_ab30count = sum(tmod_above30, na.rm = T),
         tmax_above30 = if_else(temp.max_rmx > 30, 1, 0),
         tmax_ab30count = sum(tmax_above30, na.rm = T))

 #add table 3 and 4 together and clean
biosite_tbl_unique <- biosite_tbl_3 %>%                                
  bind_rows(biosite_tbl_4) %>% 
  select(-file.path,-date, -lst, -humidity, -day:-temp.doy.rav, -tmod_diff, -tmax_diff, -tmod_above30, -tmax_above30) %>%
  distinct()
  
#####################################################################################################
### Exporting Files ###
write.csv(biosite_tbl_unique, "C:/Users/gisuser/SCCWRP/Staff - P Drive/Data/PartTimers/Megan Warren/temp metrics/temp_metric_output/biosites_temp.csv")

