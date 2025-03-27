# LAR_Regional_Temp

## Script - biosites_TempMetrics.R

calculation of temperature metrics

## Script 00_format_data.R

Formats and joins bio and temp data

Data needed - ignore/Bio/SMC_bioassessment2.csv
Temp metrics - ignore/Temp/biosites_TempMetrics.csv

## Temp metrics descriptions

lst_max7rav	      the max value of rolling 7-day average for lst
humidity_max7rav	the max value of rolling 7-day average for humidity
temp.anom_max7rav	the max value of rolling 7-day average for temp.anom
temp.plus_max7rav	the max value of rolling 7-day average for temp.plus
temp.doy_max7rav	the max value of rolling 7-day average for temp.doy
tmod_min7rmn	    the minimum value of a rolling 7-day minimum for temp.mod
tmax_min7rmn	    the minimum value of a rolling 7-day minimum for temp.max
tmod_max7rmx	    the maximum value of a rolling 7-day maximum for temp.mod
tmax_max7rmx	    the maximum value of a rolling 7-day maximum for temp.max
tmod_max7rav	    the max value of rolling 7-day average for temp.mod
tmax_max7rav	    the max value of rolling 7-day average for temp.max
tmod_maxdiff	    max difference between rolling 7-day min and max for temp.mod
tmax_maxdiff	    max difference between rolling 7-day min and max for temp.max
tmod_avdiff	      average difference between rolling 7-day min and max for temp.mod
tmax_avdiff	      average difference between rolling 7-day min and max for temp.max
tmod_ab30count	  the number of 7-day rolling averages that are over 30 for temp.mod
tmax_ab30count	  the number of 7-day rolling averages that are over 30 for temp.max

## creates
map of sites - 00_all_sites_map.jpg
all data df - ignore/00_all_data.RData

## Script 01_relative_importance.R

Boosted regression trees to evaluate relative importance

