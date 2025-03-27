### glm on csci and asci
## probability based on index threshold,
### temp metrics are weekly metrics in relation to 80 (-&/)

library(qgam); library(MASS)
library(tidyverse)
library(sf)
library(mgcv)

library(tidylog)

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
head(allData)
## channel engineering data
## can add this for mixed models if needed

BioEng <- read.csv("ignore/Chan_eng_all_SMC.csv") %>% ## upload data
  # select(-c(X,channel_engineering_classification_date, channel_engineering_personnel, channel_engineering_comments)) %>% ## remove redundant columns
  mutate(Class2 = ifelse(channel_engineering_class =="NAT", "Natural", "Modified")) %>% ## add overall modification class
  mutate(comid = as.character(comid))

## join

allData2 <- right_join(allData, BioEng, by = c("masterid", "comid"), relationship = "many-to-many") %>%
  drop_na(Score, season, channel_engineering_class)
head(allData2)

# Models: ------------------------------------------------------------------
## bio 
biol.endpoints<-c("CSCI", "ASCI")

## temp
temp.endpoints<- unique(na.omit(allData2$Metric))
temp.endpoints

## smoothing functions - testing smoothness
smooth_funcs <- c(3,6)

## Seasons
seasons <- unique(allData2$season)
seasons

bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,temp.endpoints=temp.endpoints, 
                             seasons = seasons,
                             smooth_funcs = smooth_funcs, stringsAsFactors = F)

## blank df
DF <- NULL
DF <- as.data.frame(DF)

## blank coefs 
coefs <- NULL
coefs <- as.data.frame(coefs)

i=1
## blank mods
gam_lme <- NULL

# Loop over FFMs: Mixed Model Gams ----------------------------------------------------------


for(i in 1:nrow(bio_h_summary)) {
  
  ## take each model element
  tmet<-as.character(bio_h_summary[i,"temp.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  smet<-as.numeric(bio_h_summary[i,"smooth_funcs"])
  seas<-as.character(bio_h_summary[i,"seasons"]) ## seasons could also be a random effect??

  ## subset from data
  mydat<-allData2 %>%
    filter(BioMetric == bmet, ## bio metric
           Metric == tmet, season == seas) %>% ## flow metric
    dplyr::select(Score, Value, season, channel_engineering_class) %>% ## remove redendant columns
    # drop_na(deltah_final) %>% ## drop na from flow
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct() %>%
    mutate(channel_engineering_class = as.factor(channel_engineering_class)) %>%
    drop_na()
  
  # unique(mydat$channel_engineering_class)
  
  ## format data
  mydat<-mydat[order(mydat$Score),] ## order by csci value
  # head(mydat)
  ## run models
  
  ### random intercept
  gamm_intercept <- mgcv::gam(Score ~ s(Value, k=smet) + 
                                s(channel_engineering_class, bs = "re"), 
                              data = mydat,
                              method = "REML")
  
  ### random slopes
  gamm_slope <- mgcv::gam(Score ~ s(Value, k=smet) + 
                            s(Value, channel_engineering_class, bs = "re"), 
                          data = mydat,
                          method = "REML")
  
  # Extract standard deviations
  # random_std_dev <- vcomp["s(deltah_final,channel_engineering_class)", "std.dev"]  # Random effect for channel_engineering_class
  # residual_std_dev <- vcomp["scale", "std.dev"]  # Residual standard deviation
  
  # Compute variances
  # random_variance <- random_std_dev^2
  # residual_variance <- residual_std_dev^2
  # 
  # # Compute ICC
  # icc <- random_variance / (random_variance + residual_variance)
  # print(paste("ICC:", round(icc, digits = 2)))
  
  ### random slope & intercept
  gamm_int_slope <- mgcv::gam(Score ~ s(Value, k=smet) + 
                                s(channel_engineering_class, bs = "re") + ## intercept
                                s(channel_engineering_class, Value, bs = "re"), ## slope
                              data = mydat, 
                              method = "REML")
  
  
  # vcomp <- gam.vcomp(gamm_slope)
  # 
  # icc <- vcomp[["fac"]] / (vcomp[["fac"]] + vcomp[["residual"]])
  # 
  # print(paste("ICC:", icc))
  # print(vcomp)
  
  ## mods 
  
  # modsx <- c(gamm_intercept, gamm_slope, gamm_int_slope)
  ## coefs
  coefsx <- AIC(gamm_intercept, gamm_slope, gamm_int_slope) %>% ## get AIC for comparison
    mutate(DevianceExplained = c(summary(gamm_intercept)$dev.expl, summary(gamm_slope)$dev.expl, summary(gamm_int_slope)$dev.expl)) %>% ## deviance explained
    mutate(RSquared = c(summary(gamm_intercept)$r.sq, summary(gamm_slope)$r.sq, summary(gamm_int_slope)$r.sq)) %>%
    mutate(Variable = tmet,  Metric = bmet, Smooths = smet, Season = seas) %>%
    mutate(Model = c("gamm_intercept", "gamm_slope", "gamm_int_slope"))
  
  ## predict for figure
  mydat_long <- mydat %>%
    mutate(Intercept = predict(gamm_intercept),
           Slope = predict(gamm_slope),
           SlopeAndIntercept = predict(gamm_int_slope)) %>%
    pivot_longer(Intercept:SlopeAndIntercept, names_to = "ModelType", values_to = "predictedVals") %>%
    mutate(ModelType = factor(ModelType, levels = c("Intercept", "Slope", "SlopeAndIntercept"), 
                              labels = c("Intercept", "Slope", "Slope and Intercept"))) %>%
    mutate(channel_engineering_class = factor(channel_engineering_class, 
                                              levels = c("NAT", "SB0", "SB1", "SB2", "HB"), 
                                              labels = c("NAT", "SB0", "SB1", "SB2", "HB"))) 
  
  # head(mydat_long)
  
  T1 <- ggplot(data = mydat_long, aes(y=Score, x=Value, group = channel_engineering_class, col = channel_engineering_class)) +
    geom_smooth( aes(y=predictedVals, x=Value), linewidth = 1)+
    # geom_point(data = mydat, aes(x=deltah_final, y = MetricValue,
    #                                col = channel_engineering_class)) +
    geom_hline(yintercept = 0.79,  linetype="dashed", linewidth=0.5, color = "grey50") +
    geom_hline(yintercept = 0.67,  linetype="dotted", linewidth=0.5, color = "grey50") +
    geom_vline(xintercept = 0) +
    facet_grid(~ModelType, scales = "free") +
    scale_colour_manual(values=c("chartreuse4", "dodgerblue2", "darkblue", "mediumpurple2", "firebrick3"))+ ## colour of points
    scale_x_continuous(name="Delta (CFS)") +
    scale_y_continuous(name = paste0("CSCI Score")) +
    theme_classic() +
    theme(legend.title = element_blank(), 
          # legend.position = "bottom",
          legend.text=element_text(size=12),
          axis.text.x = element_text(size = 12, angle = 20, vjust = 0.5,hjust = 0.3),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 15),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))
  
  T1
  
  file.name1 <- paste0(out.dir, "03_",bmet,"_", tmet, "_", smet, "_",seas, "_mixed_effects_GAM_raw.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
  
  
  ## increments for newdata
  incr <- mean(mydat$Value)/500
  # head(newdata)
  ## get new data from range of input data, incluyding channel class for random effects
  newdata <- as.data.frame(seq(min(mydat$Value), max(mydat$Value), abs(incr))) %>%
    rename(Value = 1) %>% ## change name to match original
    mutate(channel_engineering_class1 = "NAT", ## add channel class as columns
           channel_engineering_class2 = "SB0",
           channel_engineering_class4 = "SB1",
           channel_engineering_class5 = "SB2",
           channel_engineering_class6 = "HB") %>%
    pivot_longer(channel_engineering_class1:channel_engineering_class6,  ## make channel class longer
                 names_to = "deletethiscolumn", values_to = "channel_engineering_class") %>%
    dplyr::select(-deletethiscolumn) ## remove unwanted column
  
  
  # head(newdata)
  
  ## predict on all 3 models
  predictedValsI <- as.data.frame(predict(gamm_intercept, newdata, type = "response")) #%>% ## add newdata 
  predictedValsS = as.data.frame(predict(gamm_slope, newdata, type = "response"))
  predictedValsIS  = as.data.frame(predict(gamm_int_slope, newdata, type = "response"))
  
  ## join all predictions
  predictedVals <- cbind(predictedValsI,predictedValsS,predictedValsIS)
  
  ## change names
  colnames(predictedVals) = c("Intercept", "Slope", "SlopeAndIntercept")
  
  # head(predictedVals)
  ## join 
  DFX <- cbind(newdata, as.data.frame(predictedVals)) %>%
    # rename(Value = "s(deltah_final)") %>%
    mutate(Variable = tmet,  Metric = bmet, Smooths = smet, Season = seas) %>%
    distinct() 
  
  
  # head(DFX1)
  
  DFX1 <- DFX %>%
    pivot_longer(Intercept:SlopeAndIntercept, names_to = "ModelType", values_to = "predictedVals") %>%
    mutate(ModelType = factor(ModelType, levels = c("Intercept", "Slope", "SlopeAndIntercept"), 
                              labels = c("Intercept", "Slope", "Slope and Intercept"))) %>%
    mutate(channel_engineering_class = factor(channel_engineering_class, 
                                              levels = c("NAT", "SB0", "SB1", "SB2", "HB"), 
                                              labels = c("NAT", "SB0", "SB1", "SB2", "HB"))) 
  
  
  
  T2 <- ggplot() +
    geom_smooth(data = DFX1, aes(y=predictedVals, x=Value, col = channel_engineering_class), linewidth = 1)+
    geom_hline(yintercept = 0.79,  linetype="dashed", linewidth=0.5, color = "grey50") +
    geom_hline(yintercept = 0.67,  linetype="dotted", linewidth=0.5, color = "grey50") +
    geom_vline(xintercept = 0) +
    scale_colour_manual(values=c("chartreuse4", "dodgerblue2", "darkblue", "mediumpurple2", "firebrick3"))+ ## colour of points
    scale_x_continuous(name="Temp") +
    facet_wrap(~ModelType, scales = "free") +
    scale_y_continuous(name = paste0("Bioassessment Score")) +
    theme_classic() +
    theme(legend.title = element_blank(), 
          # legend.position = "bottom",
          legend.text=element_text(size=12),
          axis.text.x = element_text(size = 12, angle = 20, vjust = 0.5,hjust = 0.3),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 15),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))
  
  
  
  # theme(legend.position = "none"))
  
  T2
  
  file.name1 <- paste0(out.dir, "03_",bmet,"_", tmet, "_", smet, "_",seas,  "_mixed_effects_GAM_predicted.jpg")
  ggsave(T2, filename=file.name1, dpi=300, height=5, width=7.5)
  
  
  # head(DFX)
  ## combine 
  DF <- bind_rows(DF, DFX)
  coefs <- bind_rows(coefs, coefsx)
  gam_lme <- c(gam_lme, list(gamm_intercept, gamm_slope, gamm_int_slope))
  # sapply(gam_lme, class)
}

gam_lme
## remove blank model
# gam_lme <- gam_lme[-1]

## save models
save(gam_lme, file = "ignore/03_mixed_models.RData")

## save coefs
write.csv(coefs, "ignore/03_coefs_mixed_effects_model.csv")
## without SB1 r2 ~ 0.4-0.45
## save predictions

save(DF, file= "ignore/03_mixed_effects_model_predictions.RData")
load(file= "ignore/03_mixed_effects_model_predictions.RData")
head(DF)
