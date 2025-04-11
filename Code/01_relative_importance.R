## BRTs for relative importance

###  brts

library(gbm)
library(dismo)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)
library(tidylog)

set.seed(321) # reproducibility

## brt function - Ryan Peek
source("Code/functions/My.gbm.step.R")

## upload data

load(file = "ignore/00_all_data.RData")
head(allData)

# CSCI BRTs ---------------------------------------------------------------

## format for model
unique(allData$Metric)

data <- allData %>%
  filter(BioMetric == "CSCI") %>%
  dplyr::select(masterid, Year,  Score, season, Metric, Value) %>%
  distinct() %>%
  mutate(season = as.factor(season)) %>%
  pivot_wider(names_from = Metric, values_from = Value, values_fn = mean) %>% ## take mean of duplicates for now
  drop_na() %>%
  dplyr::select(-c(starts_with("humidity"), starts_with("lst"), contains("temp.plus"), contains("anom"), contains("doy")))

## could also just create seasonal metric names

## as df
data <- as.data.frame(data)
sum(is.na(data))
head(data)
class(data)
names(data)

## gbm functions in brt.functions script

## CSCI

## define criteria

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.0001, 0.0003, 0.0005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8)
)


# double check and view
hyper_grid
names(data)
# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
    shrinkage, interaction.depth, n.minobsinnode, bag.fraction,n.trees, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 3, # response in training data
    gbm.x = 4:16, # temp dat
    family = "gaussian",
    data = data,
    max.trees = 50000, 
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# use PURRR: this part can take a looong time
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data) # CHECK AND CHANGE!!
)

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# based on above, run final BRT and save:
gbm_final_step <- function(
    shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 3, # response in training data
    gbm.x = 4:16, # temp dat
    family = "gaussian",
    data = data,
    max.trees = 50000,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
}


# set up filename for best model outputs
(gbm_best_file <- paste0("output_data/01_gbm_final_csci_model_output.txt"))

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

 #strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write.csv(hyper_best, "output_data/01_best_model_csci_output.csv")

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 
# 0.45


# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_csci")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_csci")))

# save to RDS
write_rds(x = get(fileToSave), path = paste0("output_data/01_",fileToSave, "_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("output_data/01_",fileToSave,"_model_data.rda")))

gbm_final <- read_rds("ignore/01_gbm_final_csci_model.rds")
class(gbm_final)

gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) 
gbm_fin_RI  


# tmod_max7rmx_spring     tmod_max7rmx_spring 17.80983249
# tmax_max7rmx_spring     tmax_max7rmx_spring 12.66149828
# tmod_max7rav_spring     tmod_max7rav_spring  9.33974666
# tmod_min7rmn_winter     tmod_min7rmn_winter  9.17141917
# tmax_max7rav_spring     tmax_max7rav_spring  5.34999521
# tmod_avdiff_winter       tmod_avdiff_winter  2.69028255
# tmod_min7rmn_summer     tmod_min7rmn_summer  2.67484631
# tmax_min7rmn_fall         tmax_min7rmn_fall  1.98606564

# Plots and metrics-------------------------------------------------------------------


write.csv(gbm_fin_RI, "output_data/01_rel_imp_csci_labels.csv")


ggplot(data=gbm_fin_RI, aes(x=reorder(var,-rel.inf), y=rel.inf, fill = var)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  # scale_x_continuous(limits = c(0, 35)) +
  labs(title = "Relative Importance of Temp on CSCI",
       x = "Temperature Metric",
       y = "Relative Importance (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")




gbm_fin_RI



# ASCI --------------------------------------------------------------------

set.seed(321) # reproducibility

## upload data

## fiormat for models
names(data)
data <- allData %>%
  filter(BioMetric == "ASCI") %>%
  dplyr::select(masterid, Year,  Score, season, Metric, Value) %>%
  distinct() %>%
  mutate(season = as.factor(season)) %>%
  pivot_wider(names_from = Metric, values_from = Value, values_fn = mean) %>% ## take mean of duplicates for now
  drop_na() %>%
  dplyr::select(-c(starts_with("humidity"), starts_with("lst"), contains("temp.plus"), contains("anom"), contains("doy")))

# redo with seasonal metrics

data <- as.data.frame(data)
## gbm functions in brt.functions script

## ASCI

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
hyper_grid

names(data)

# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
    shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 3, # response in training data
    gbm.x = 4:16, # temp dat
    family = "gaussian",
    data = data,
    max.trees = 50000,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data) 
)

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))


# based on above, run final BRT and save:
gbm_final_step <- function(
    shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 3, # response in training data
    gbm.x = 4:16, # temp dat
    family = "gaussian",
    data = data,
    max.trees = 50000,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

# set up filename for best model outputs
(gbm_best_file <- paste0("output_data/01_gbm_final_asci_model_output.txt"))

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write.csv(hyper_best, "output_data/01_best_model_asci_output.csv")

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 
# 0.39


# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_asci")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_asci")))

# save to RDS
write_rds(x = get(fileToSave), path = paste0("output_data/01_",fileToSave, "_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("output_data/01_",fileToSave,"_model_data.rda")))

gbm_final <- read_rds("output_data/01_gbm_final_asci_model.rds")
class(gbm_final)

gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) 
gbm_fin_RI  


# Plots and metrics-------------------------------------------------------------------


## combine with rel importance

write.csv(gbm_fin_RI, "output_data/01_rel_imp_asci_labels.csv")

## plot
ggplot(data=gbm_fin_RI, aes(x=reorder(var,-rel.inf), y=rel.inf, fill = var)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  labs(title = "Relative Importance of Temp on ASCI",
       x = "Temperature Metric",
       y = "Relative Importance (%)") #+ theme_bw(base_size = 15)

gbm_fin_RI

### brt figure

library(tidyverse)

out.dir <- "/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic_v2/Figures/"

### upload rel importance
gbm_fin_RI_csci <- read.csv("output_data/01_rel_imp_csci_labels.csv")
gbm_fin_RI_asci <- read.csv("output_data/01_rel_imp_asci_labels.csv")

gbm_fin_RI_csci <- gbm_fin_RI_csci %>%
  mutate(Index = "CSCI")

gbm_fin_RI_asci <- gbm_fin_RI_asci %>%
  mutate(Index = "ASCI")

gbm_fin_RI <- rbind(gbm_fin_RI_csci, gbm_fin_RI_asci) %>%
  rename(Var = var) #%>%
# mutate(Index = recode_factor(Index, CSCI="CSCI", ASCI="ASCI"))

gbm_fin_RI 

# gbm_fin_RI <- gbm_fin_RI %>%
#   mutate(TempMetric = case_when(Var ==  "tmod_min7rmn" ~ "Weekly Min of Daily Mean Temp",
#                                 Var == "tmax_min7rmn" ~ "Weekly Min of Max Daily Temp (+10%)",
#                                 Var ==  "tmax_max7rmx" ~ "Weekly Max of Max Daily Temp (+10%)",
#                                 Var == "tmod_avdiff" ~ "Weekly Range of Daily Mean Temp",
#                                 Var == "tmax_max7rav" ~ "Weekly Max of Average Daily Temp (+10%)",
#                                 Var == "tmod_max7rmx" ~ "Weekly Max of Daily Max Temp",
#                                 Var == "tmax_ab30count" ~  "Number of Days over 30 Degrees (+10%)",
#                                 Var ==  "tmod_max7rav" ~ "Weekly Max of Daily Mean Temp",
#                                 Var == "tmax_avdiff" ~ "Weekly Mean Range (+10%)",
#                                 Var == "tmax_maxdiff" ~ "Weekly Max Range (+10%)",
#                                 Var == "tmod_maxdiff" ~ "Weekly Max Range",
#                                 Var == "tmod_ab30count" ~ "Number of Days over 30 Degrees"))
# gbm_fin_RI

c1 <- ggplot(data=gbm_fin_RI, aes(x=reorder(Var,-rel.inf), y=rel.inf, fill = Var)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  # scale_x_continuous(limits = c(0, 35)) +
  facet_wrap(~Index) +
  labs(title = "",
       x = "",
       y = "Relative Importance (%)")+
  theme(legend.position = "none")

c1

out.filename <- paste0(out.dir,"01_rel_imp_csci_asci_bar_plot.jpg")
ggsave(c1, file = out.filename, dpi=300, height=4, width=6)



all_rf <- gbm_fin_RI %>%
  select(Var, TempMetric, rel.inf, Index) %>%
  pivot_wider(names_from = Index, values_from = rel.inf)

head(all_rf)


write.csv(all_rf, "output_data/01_relative_imp_table.csv")








