library(tidyverse)
library(readr)
library(dplyr)


<<<<<<< HEAD
# test
# test hannah git
# test hannah 2 
=======
data <- read_csv('./Lab5_PredictionChallenge_training.csv')
>>>>>>> eb04e2f7168418e8dd7706c74a79c2b4adcc8b40

plz_einwohner <- read_csv('./plz_einwohner.csv')
plz_einwohner$plz <- as.numeric(plz_einwohner$plz)

zuordnung_plz_ort <- read_csv('./zuordnung_plz_ort.csv')
zuordnung_plz_ort$plz <- as.numeric(zuordnung_plz_ort$plz)
drop.cols <- c('osm_id', 'bundesland')
zuordnung_plz_ort <- zuordnung_plz_ort %>% select(-one_of(drop.cols))

as.data.frame(data)
as.data.frame(plz_einwohner)
as.data.frame(zuordnung_plz_ort)

data <- merge(data,plz_einwohner, by.x="postcode",by.y="plz")
data <- merge(data,zuordnung_plz_ort, by.x="postcode",by.y="plz")

