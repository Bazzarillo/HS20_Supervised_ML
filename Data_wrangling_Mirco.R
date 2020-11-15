library(readr)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(caret)


training <- read_csv("Data/training.csv")

str(training)

summary(training)

#resizing and formatting sample, most of the variables consist of up to 99% NA's. I'll throw these ones out of the Data froma.
#I am keeping: 
#GDENAMK,GDENR,KTKZ,area,balcony,cabletv,date,descr,elevator,floors,kids_friendly,
#msregion,parking_indoor,parking_outside,quarter_general,quarter_specific,rooms,rent_full,year_built
#Micro_rating,Micro_rating_NoiseAndEmission,Micro_rating_Accessibility,Micro_rating_DistrictAndArea
#Micro_rating_SunAndView,Micro_rating_ServicesAndNature,wgh_avg_sonnenklasse_per_egid,Anteil_auslaend
#Avg_age,Avg_size_household,Noise_max,anteil_efh,apoth_pix_count_km2,avg_anzhl_geschosse,dist_to_4G
#dist_to_haltst,dist_to_highway,dist_to_lake,dist_to_main_stat,dist_to_school_1,dist_to_train_stat
#geb_wohnnutz_total,restaur_pix_count_km2,superm_pix_count_km2,dist_to_river

training %>% select(GDENAMK,GDENR,KTKZ,area,balcony,cabletv,date,elevator,floors,kids_friendly,
                    msregion,parking_indoor,parking_outside,quarter_general,quarter_specific,rooms,rent_full,year_built,
                    Micro_rating,Micro_rating_NoiseAndEmission,Micro_rating_Accessibility,Micro_rating_DistrictAndArea,
                    Micro_rating_SunAndView,Micro_rating_ServicesAndNature,wgh_avg_sonnenklasse_per_egid,Anteil_auslaend,
                    Avg_age,Avg_size_household,Noise_max,anteil_efh,apoth_pix_count_km2,avg_anzhl_geschosse,dist_to_4G,
                    dist_to_haltst,dist_to_highway,dist_to_lake,dist_to_main_stat,dist_to_school_1,dist_to_train_stat,
                    geb_wohnnutz_total,restaur_pix_count_km2,superm_pix_count_km2,dist_to_river) -> training_s_1

#The first seleciton condensed the data frame from 100 to 44 variables. We still need to mutate some of them.

training_s_1 %>% mutate(KTKZ = as.factor(KTKZ)) %>% 
  mutate_at(c("balcony","cabletv","elevator","kids_friendly",
              "parking_indoor","parking_outside"), as.logical) %>%
  mutate_at(c("GDENAMK","year_built","floors"), as.factor) %>% 
  mutate(date = as.Date(date, format ="%d.%m.%Y")) -> training_s_1


summary(training_s_1)

#One big problem: the logical values do not contain any "False" or "0"-variables. We either have 1 and NA.
#We therefore have 2 options left: categorizing every NA as FALSE, or leving them out.
#I'll only work with numerical data as a first step.

#Selecting numerical variables and year_built

training_s_1 %>% select(which(sapply(.,class)=="numeric")) -> training_num

#Creating a correlatoon map
correlations_num <- cor(training_num)

#Visualising the correlation
ggcorrplot(correlations_num)

#Many of the values do not correalte at all. I'll kick them out. Furthermor, I'll kick out the general micro rating, as it leads to too much
#Multi-Colinearity.

training_num %>% select(rent_full, msregion,Micro_rating_NoiseAndEmission, Micro_rating_Accessibility, Micro_rating_DistrictAndArea,
                         Micro_rating_SunAndView, Micro_rating_ServicesAndNature, apoth_pix_count_km2, dist_to_4G, dist_to_highway, dist_to_train_stat,
                         restaur_pix_count_km2, superm_pix_count_km2, dist_to_river) -> training_short

#New visualisation:

short_corr <- cor(training_short)
ggcorrplot(short_corr,hc.order = TRUE)

#This is highly optimized, but I still ned to normalize the data. MinMax-Scaling

preprocess_num <- preProcess(training_short, method = "range")
minMax_num <- predict(preprocess_num,training_short )
summary(minMax_num)

short_corr_minmax <- cor(minMax_num)
ggcorrplot(short_corr_minmax,hc.order = TRUE, type="upper", lab = TRUE)

#Did not change that much, but I think it is better to work with normalized data.

lm_1 <- lm(data=minMax_num, rent_full ~ ., )
summary(lm_1)
#Both Micro_rating_ServicesAndNature and Micro_rating_NoiseAndEmission are not significant -> I'll kick them out.

#Create new dataset, this time containing factors too:

training_s_1 %>% 