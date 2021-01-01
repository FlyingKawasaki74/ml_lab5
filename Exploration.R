library(tidyverse)
library(readr)
library(corrplot)
library(dplyr)
library(magrittr)

data <- read_csv('./Lab5_PredictionChallenge_training.csv')

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

data1 <- data %>% arrange(einwohner)
data1 <- as_tibble(data1)
write_csv(data1, "data_extended.csv")


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
  select('Rent', 'livingspace', 'NoOfRooms', 'lat', 'lon', 'distanceShop',
         'dum_balcony', 'dum_builtinkitchen', 'dum_floorplan', 'dum_garden',
         'dum_privateoffer', 'ost', 'sachsenanhalt', 'brandenburg', 'sachsen',
         'thueringen', 'mecklenburg', 'baden', 'bayern', 'berlin', 'bremen',
         'hamburg', 'niedersachsen', 'nrw', 'rheinland', 'saarland', 'schleswig',
         'einwohner', 'einwohner_total') %>%
  na.omit()

write_csv(data_relevant, "model_input_data.csv")

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
write_csv(correlations_original, "correlations.csv")

#----------------------------------



#Identify relevant regressors (without one hot encoding)
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


#Identify relevant regressors (with one hot encoding)
#-----------------------------

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']])
residuals <- simple_model[['residuals']]

data_res <- data_relevant %>% add_column(res1=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res1") 
# Pick ost next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['ost']])
residuals <- simple_model[['residuals']]

data_res <- data_res %>% add_column(res2=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res2") 
# Pick dum_buildinkitchen next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['ost']] +
                     data_relevant[['dum_builtinkitchen']])
residuals <- simple_model[['residuals']]

data_res <- data_res %>% add_column(res3=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res3")
# Pick berlin next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['ost']] +
                     data_relevant[['dum_builtinkitchen']] +
                     data_relevant[['berlin']])
residuals <- simple_model[['residuals']]

data_res <- data_res %>% add_column(res4=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res4")
# Pick bayern next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['ost']] +
                     data_relevant[['dum_builtinkitchen']] +
                     data_relevant[['berlin']] +
                     data_relevant[['bayern']])
residuals <- simple_model[['residuals']]

data_res <- data_res %>% add_column(res5=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res5")
# Pick hamburg next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['ost']] +
                     data_relevant[['dum_builtinkitchen']] +
                     data_relevant[['berlin']] +
                     data_relevant[['bayern']] +
                     data_relevant[['hamburg']])
residuals <- simple_model[['residuals']]

data_res <- data_res %>% add_column(res6=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res6")
# Pick baden next

simple_model <- lm(data_relevant[['Rent']] ~ data_relevant[['livingspace']] +
                     data_relevant[['ost']] +
                     data_relevant[['dum_builtinkitchen']] +
                     data_relevant[['berlin']] +
                     data_relevant[['bayern']] +
                     data_relevant[['hamburg']] +
                     data_relevant[['baden']])
residuals <- simple_model[['residuals']]

data_res <- data_res %>% add_column(res7=residuals, .before='Rent')
correlations <- get_correlation(data_res, "res7")
# Skip NoOfRooms because of high correlation to already included livingspace -> avoid multicollinearity
# Skip dum_floorplan because see the following plot
# No regressor left with >0.1 or <-0.1 correlation to the residual

# Plot correlation of res7 with remaining top 4 regressors
ggplot(data=data_res, aes(x=NoOfRooms, y=res7)) +
  geom_point()

ggplot(data=data_res, aes(x=dum_floorplan, y=res7, group=dum_floorplan)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=dum_balcony, y=res7, group=dum_balcony)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=lat, y=res7)) +
  geom_point()

# Plot correlation of Rent with the regressors we included
ggplot(data=data_res, aes(x=livingspace, y=Rent)) +
  geom_point()

ggplot(data=data_res, aes(x=ost, y=Rent, group=ost)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=dum_builtinkitchen, y=Rent, group=dum_builtinkitchen)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=berlin, y=Rent, group=berlin)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=bayern, y=Rent, group=bayern)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=hamburg, y=Rent, group=hamburg)) +
  geom_boxplot()

ggplot(data=data_res, aes(x=baden, y=Rent, group=baden)) +
  geom_boxplot()

#-----------------------------