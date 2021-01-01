library(tidyverse)
library(readr)
library(randomForest)
library(Metrics)

# Data prep
#-----------------------------------------------------------------------------------
data <- read_csv('./0_Data/Lab5_PredictionChallenge_holdout.csv')

plz_einwohner <- read_csv('./0_Data/plz_einwohner.csv')
plz_einwohner$plz <- as.numeric(plz_einwohner$plz)

zuordnung_plz_ort <- read_csv('./0_Data/zuordnung_plz_ort.csv')
zuordnung_plz_ort$plz <- as.numeric(zuordnung_plz_ort$plz)
drop.cols <- c('osm_id')
zuordnung_plz_ort <- zuordnung_plz_ort %>% select(-one_of(drop.cols))

as.data.frame(data)
as.data.frame(plz_einwohner)
as.data.frame(zuordnung_plz_ort)

data <- merge(data,plz_einwohner, by.x="postcode",by.y="plz",all.x=TRUE) 
data <- merge(data,zuordnung_plz_ort, by.x="postcode",by.y="plz",all.x=TRUE)

data %<>% arrange(einwohner)
data <- as_tibble(data)

data_onehot <- data %>% mutate(
  ost = ifelse(bundesland %in% c("Sachsen-Anhalt","Brandenburg", "Sachsen", "Thüringen", "Mecklenburg-Vorpommern"),1,0 ),
  sachsenanhalt = ifelse(bundesland == "Sachsen-Anhalt",1,0),
  brandenburg = ifelse(bundesland == "Brandenburg",1,0),
  sachsen = ifelse(bundesland == "Sachsen",1,0),
  thueringen = ifelse(bundesland == "Thüringen",1,0),
  mecklenburg = ifelse(bundesland == "Mecklenburg-Vorpommern",1,0),
  baden = ifelse(bundesland == "Baden-Württemberg",1,0),
  bayern = ifelse(bundesland == "Bayern",1,0),
  berlin = ifelse(bundesland == "Berlin",1,0),
  bremen = ifelse(bundesland == "Bremen",1,0),
  hamburg = ifelse(bundesland == "Hamburg",1,0),
  niedersachsen = ifelse(bundesland == "Niedersachsen",1,0),
  nrw = ifelse(bundesland == "Nordrhein-Westfalen",1,0),
  rheinland = ifelse(bundesland == "Rheinland-Pfalz",1,0),
  saarland = ifelse(bundesland == "Saarland",1,0),
  schleswig = ifelse(bundesland == "Schleswig-Holstein",1,0)
  # Skip Schleswig-Holstein because it is true when all others are 0
) 

data_einwohner <- data %>%
  group_by(ort) %>%
  summarize(
    einwohner_total = sum(einwohner)
  )


data_relevant <- data_onehot %>%
  left_join(data_einwohner, by="ort") %>%
  select('id', 'livingspace', 'NoOfRooms', 'lat', 'lon', 'distanceShop',
         'dum_balcony', 'dum_builtinkitchen', 'dum_floorplan', 'dum_garden',
         'dum_privateoffer', 'ost', 'sachsenanhalt', 'brandenburg', 'sachsen',
         'thueringen', 'mecklenburg', 'baden', 'bayern', 'berlin', 'bremen',
         'hamburg', 'niedersachsen', 'nrw', 'rheinland', 'saarland', 'schleswig',
         'einwohner', 'einwohner_total') %>%
  na.omit()
#-----------------------------------------------------------------------------------

# Prediction
#-----------------------------------------------------------------------------------
load(file = "./model_rf_fulldata.rda")

predictions = predict(rf_fulldata, data_relevant %>% select(-id))

results = tibble(data_relevant[["id"]], predictions) %>%
  rename(id = `data_relevant[["id"]]`) %>%
  rename(prediction = predictions)

write_csv(results, "holdout_predictions.csv")
#-----------------------------------------------------------------------------------