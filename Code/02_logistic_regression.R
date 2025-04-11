### glm on csci and asci
## probability based on index threshold,
### temp metrics are weekly metrics in relation to 80 (-&/)

library(tidylog)
library(tidyverse)
library(sf)

out.dir <- "Figures/"


# Temp data ---------------------------------------------------------------

load(file = "ignore/00_all_data.RData")
head(allData)

## get comids
TempSites <- unique(allData$comid)

## POR
range(allData$Year) ## 2001 2023

## seasonal metrics
allData <- allData %>%
  mutate(Metric2 = paste0(Metric, "_", season))

## channel engineering data

# BioEng <- read.csv("ignore/02_chan_eng.csv") %>%
#   select(-c(X,channel_engineering_classification_date, channel_engineering_personnel, channel_engineering_comments)) %>%
#   mutate(Class2 = ifelse(channel_engineering_class =="NAT", "Natural", "Modified"))

# Models: ------------------------------------------------------------------


## bio 
biol.endpoints<-c("CSCI", "ASCI")

## temp
temp.endpoints<- unique(na.omit(allData$Metric))
temp.endpoints

# Thresholds for index
index.thresholds <- c(0.86, 0.79) ## can change/add modified thresholds

## Seasons
seasons <- unique(allData$season)
seasons

bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,temp.endpoints=temp.endpoints, 
                             index.thresholds= index.thresholds, seasons = seasons,stringsAsFactors = F)

# bio_h_summary <- bio_h_summary[-c(1:9),]
bio_h_summary
i=1
## model of each configuration
log.lm <-lapply(1:nrow(bio_h_summary), function(i)
{

  tmet<-as.character(bio_h_summary[i,"temp.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  # imet<-as.character(bio_h_summary[i,"index.thresholds"])
  smet<-as.character(bio_h_summary[i,"seasons"])

  mydat<-allData %>%
    filter(BioMetric == bmet,
           Metric == tmet, season == smet) %>%
    select(Score, Value, comid, season) %>% ## only metrics needed
    drop_na(Score, Value) %>%
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct()
  
  ## use different threshold for each index
  if(bmet == "CSCI") {
    
    mydat$Condition<-ifelse(mydat$MetricValue < 0.79 ,0, 1) ## convert to binary
    
  } else {
    
    mydat$Condition<-ifelse(mydat$MetricValue < 0.86 ,0, 1) ## convert to binary
    
  }

  
  mydat<-mydat[order(mydat$Score),] ## order by csci value
  glm(Condition~Value, family=binomial(link="logit"), data=mydat) ### glm
  
  
})

## save models
save(log.lm, file = "ignore/02_glms_csci_asci_all_temp_metrics.RData")

### get rsqds and pvals
for(i in 1:length(log.lm)) {
  
  mod <- summary(log.lm[[i]])
  bio_h_summary$AIC[i] <- mod$aic ##1-mod$deviance/mod$null.deviance ## mcfaddens r2
  bio_h_summary$PValue[i] <- mod$coefficients[8]
  bio_h_summary$McFaddensR2[i] <- 1-mod$deviance/mod$null.deviance
  bio_h_summary$n[i] <- mod$df[2]+1
}
## save configs and r sqds
save(bio_h_summary, file="output_data/02_glm_rsqds.RData")
bio_h_summary

csci_coefs <- bio_h_summary

## make df of predicted values to predict on - need to be different for each temp metric

## blank df
DF <- NULL
DF <- as.data.frame(DF)

### get predictions and fitted values
for(i in 1:length(log.lm)) {
  
  bio <- bio_h_summary[i,"biol.endpoints"]
  temp <- bio_h_summary[i,"temp.endpoints"]
  ind <- bio_h_summary[i,"index.thresholds"]
  seas <- bio_h_summary[i,"seasons"]
  
  data<-allData %>%
    filter(BioMetric == bmet,
           Metric == tmet,
           season == seas) %>%
    select(Score, Value, comid, season) %>% ## only metrics needed
    drop_na(Score, Value) %>%
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct()
  
  
  ## new data - plus and minus 10% as a start
  tempvalues <- seq(range(data$Value)[1]-10,range(data$Value)[2]+10,0.05)
  
  ## get model, predict, extract all data and categories
  mod <- log.lm[[i]]
  predictedVals <- predict.glm(mod,list(Value = tempvalues),  type = "response")
  DFX <- as.data.frame(predictedVals)
  DFX$Value <- tempvalues
  DFX$Bio <- bio
  DFX$Variable <- temp
  DFX$BioThreshold <- ind
  DFX$Season <- seas
  DFX$MinVal <-  range(data$Value)[1]
  DFX$MaxVal <-  range(data$Value)[2]
  
  DF <- bind_rows(DF, DFX)
  
}
DF
# plot(DF$predictedVals, DF$BioVals)

DF$BioThreshold <- as.factor(DF$BioThreshold)

### predicted figures
mets <- unique(DF$BioThreshold)
mets
m=1
## facet labels 
supp.labs <-  c("tmod_min7rmn" = "Weekly Min of Daily Mean Temp",
                 "tmax_min7rmn" = "Weekly Min of Max Daily Temp (+10%)",
                  "tmax_max7rmx" = "Weekly Max of Max Daily Temp (+10%)",
                   "tmod_avdiff" = "Weekly Range of Daily Mean Temp",
                    "tmax_max7rav" = "Weekly Max of Average Daily Temp (+10%)",
                      "tmod_max7rmx" = "Weekly Max of Daily Max Temp",
                        "tmax_ab30count" =  "Number of Days over 30 Degrees (+10%)",
                          "tmod_max7rav" = "Weekly Max of Daily Mean Temp",
                            "tmax_avdiff" = "Weekly Mean Range (+10%)",
                              "tmax_maxdiff" = "Weekly Max Range (+10%)",
                                "tmod_maxdiff" = "Weekly Max Range",
                                  "tmod_ab30count" = "Number of Days over 30 Degrees")

for(m in 1:length(mets)) {
  
  T1 <- (ggplot(subset(DF, BioThreshold == mets[m]), aes(y=predictedVals, x=Value, group = Variable, color = Variable)) +
           # geom_point(size=0.2) +
           geom_line( linewidth = 1)+
           # stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
           # geom_hline(yintercept = 0.6,  linetype="dashed", linewidth=0.5, color = "grey50") +
           facet_wrap(~Variable, scales = "free") +
           # geom_vline(data=filter(DF, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
           #            aes(xintercept = 86), linetype="dashed", color = "red", linewidth=0.5, show.legend = T) +
           # geom_vline(data=filter(DF, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
           #            aes(xintercept = 80), linetype="dashed", color = "blue", linewidth=0.5, show.legend = T) +
           scale_x_continuous(name="Water Temp (°C)") +
           scale_y_continuous(name = paste0("Probability of ", mets[m], " CSCI")) +
           theme(legend.position = "none"))
  
  T1
  
  file.name1 <- paste0(out.dir, "02_", mets[m], "_csci_temp_response_predicted_glm_simple.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

### predicted figures with index thresholds on one figure
mets <- unique(DF$Variable)

for(m in 1:length(mets)) {
  
  T1 <- (ggplot(subset(DF, Variable == mets[m]), aes(y=predictedVals, x=Value, group = BioThreshold, color = BioThreshold)) +
           # geom_point(size=0.2) +
           geom_line(linewidth = 1)+
           # scale_color_manual(values=c('blue','red')) +
           # stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
           # geom_hline(yintercept = 0.6,  linetype="dashed", linewidth=0.5, color = "grey50") +
           facet_wrap(~Bio, scales = "free") +
           # geom_vline(data=filter(DF, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
           #            aes(xintercept = 86), linetype="dashed", color = "red", linewidth=0.5, show.legend = T) +
           # geom_vline(data=filter(DF, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")),
           #            aes(xintercept = 80), linetype="dashed", color = "blue", linewidth=0.5) +
           scale_x_continuous(name="Water Temp (°C)") +
           scale_y_continuous(name = paste0("Probability of Good Score")))
  
  T1
  
  file.name1 <- paste0(out.dir, "02_", mets[m], "_temp_response_predicted_glm.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=6, width=6)
}

## get temps for different probabilities

## plot seasonal curves on same plot

### predicted figures with index thresholds on one figure
mets <- unique(DF$Variable)


for(m in 1:length(mets)) {
  
  T1 <- (ggplot(subset(DF, Variable == mets[m]), aes(y=predictedVals, x=Value, group = Season, color = Season)) +
           # geom_point(size=0.2) +
           geom_line(linewidth = 1)+
           # scale_color_manual(values=c('blue','red')) +
           # stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
           # geom_hline(yintercept = 0.6,  linetype="dashed", linewidth=0.5, color = "grey50") +
           facet_grid(BioThreshold~Bio, scales = "free") +
           # geom_vline(data=filter(DF, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
           #            aes(xintercept = 86), linetype="dashed", color = "red", linewidth=0.5, show.legend = T) +
           # geom_vline(data=filter(DF, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")),
           #            aes(xintercept = 80), linetype="dashed", color = "blue", linewidth=0.5) +
           scale_x_continuous(name="Water Temp (°C)") +
           scale_y_continuous(name = paste0("Probability of Good Score")))
  
  T1
  
  file.name1 <- paste0(out.dir, "02_", mets[m], "_temp_response_predicted_glm.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=6, width=6)
}

?facet_grid
