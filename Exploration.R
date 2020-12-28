library(tidyverse)
library(readr)
library(corrplot)
library(dplyr)

data <- read_csv('./Lab5_PredictionChallenge_training.csv')

plz_einwohner <- read_csv('./plz_einwohner.csv')
plz_einwohner$plz <- as.numeric(plz_einwohner$plz)

zuordnung_plz_ort <- read_csv('./zuordnung_plz_ort.csv')
zuordnung_plz_ort$plz <- as.numeric(zuordnung_plz_ort$plz)
drop.cols <- c('osm_id')
zuordnung_plz_ort <- zuordnung_plz_ort %>% select(-one_of(drop.cols))

as.data.frame(data)
as.data.frame(plz_einwohner)
as.data.frame(zuordnung_plz_ort)

data <- merge(data,plz_einwohner, by.x="postcode",by.y="plz",all.x=TRUE) 
data <- merge(data,zuordnung_plz_ort, by.x="postcode",by.y="plz",all.x=TRUE)





data_relevant <- data %>%
  select('Rent', 'livingspace', 'NoOfRooms', 'lat', 'lon', 'distanceShop', 'dum_balcony', 'dum_builtinkitchen', 'dum_floorplan', 'dum_garden', 'dum_privateoffer')

# Distribution of target variable
#-----------------------------------

ggplot(data=data_relevant, aes(x=Rent))+
  geom_histogram(binwidth=10) + # Using binwidth 10 because most rents are divisible by 10
  geom_vline(xintercept = mean(data_relevant[["Rent"]]), color="blue")+
  geom_vline(xintercept = median(data_relevant[["Rent"]]), color="orange") +
  scale_x_continuous(breaks=seq(0, 2200, 100))

#-----------------------------------


# Correlation analysis
#-----------------------------------

get_correlation <- function(data, target){
  # As tibble
  correlations <- as_tibble(cor(data)) %>%
    add_column(regressor = colnames(.), .before= 1) %>%
    arrange(desc(abs(.[[!!target]])))
  
  # As plot
  corrplot(cor(data))
  
  return(correlations)
}

correlations_original <- get_correlation(data_relevant, "Rent")

#----------------------------------



#Identify relevant regressors
#-----------------------------

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']])
residuals <- simple_model[['residuals']]

data_res <- data_relevant %>% add_column(res1=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res1") 
# Pick dum_buildinkitchen next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['dum_builtinkitchen']])
residuals <- simple_model[['residuals']]

data_res <- data_res  %>% add_column(res2=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res2")
# Pick lat next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['dum_builtinkitchen']] +
                     data_relevant[['lat']])
residuals <- simple_model[['residuals']]

data_res <- data_res  %>% add_column(res3=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res3")
# Pick dum_balcony next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['dum_builtinkitchen']] +
                     data_relevant[['lat']] +
                     data_relevant[['dum_balcony']])
residuals <- simple_model[['residuals']]

data_res <- data_res  %>% add_column(res4=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res4")
# Skip NoOfRooms because of high correlation to already included livingspace -> avoid multicollinearity
# Pick lon next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['dum_builtinkitchen']] +
                     data_relevant[['lat']] +
                     data_relevant[['dum_balcony']] +
                     data_relevant[['lon']])
residuals <- simple_model[['residuals']]

data_res <- data_res  %>% add_column(res5=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res5")
# No regressor left with >0.1 or <-0.1 correlation to the residual

# Plot correlation of res5 with remaining top 3 regressors
ggplot(data=data_res, aes(x=NoOfRooms, y=res5)) +
  geom_point()

ggplot(data=data_res, aes(x=dum_floorplan, y=res5, group=dum_floorplan)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=dum_garden, y=res5, group=dum_garden)) +
  geom_boxplot()


# Plot correlation of Rent with the regressors we included
ggplot(data=data_res, aes(x=livingspace, y=Rent)) +
  geom_point()

ggplot(data=data_res, aes(x=dum_builtinkitchen, y=Rent, group=dum_builtinkitchen)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=lat, y=Rent)) +
  geom_point()

ggplot(data=data_res, aes(x=dum_balcony, y=Rent, group=dum_balcony)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=lon, y=Rent)) +
  geom_point()

#-----------------------------
