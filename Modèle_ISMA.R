# BENCHMARK : MODELE ISMA

#Forme : données de 1990 à fin 2024, régression linéaire de 1990 à 2010 puis nowcasting du PIB de 2010 à fin 2024

rm(list = ls())
library(dplyr)

#####################
#INITIALISATION
######################

data_pib <- read_xlsx("Data_PIB_ENQ.xlsx", sheet = "PIB")
data_enq <- read_xlsx("Data_PIB_ENQ.xlsx", sheet = "ENQ_BDF")

data_pib <- data_pib |>
  select(dates, PIB, PIB_PR)


data_enq <- data_enq |>
  mutate(dates = `...1`, .keep = "all") |> 
  select(!...1)

#Convertir en date 
data_enq <- data_enq |>
  mutate(year = year(dates),
         month = month(dates),
         quarter = quarter(dates))


#####################
#Agréger en 1 dataset
#####################

#On associe trois valeurs mensuelles à chaque trimestre de prévision du PIB
data_pib <- data_pib |>
  mutate(year = year(dates),
         month = month(dates),
         quarter = case_when(
           month == 2 ~ 4,   
           month == 5 ~ 1,   
           month == 8 ~ 2,   
           month == 11 ~ 3   ),
         year = if_else(month == 2, year - 1, year)) # Décaler année pour T4

# Fusion des deux bases
final_data <- data_enq |>
  left_join(data_pib, by = c("year", "quarter"))


########################
# MODELE ISMA
######################

# TRAINING #
#Estimation du modèle sur la période 1990-2010
final_data <- final_data |> 
  select(EVLIV, PREVPRO, month.x, dates.x, year, quarter, PIB) |> 
  mutate(month = month.x, dates = dates.x, .keep = "unused") |>
  filter(year < 2025)



##Trois régressions une mois 1, une mois 2 et une mois 3

final_data <- final_data |>
  mutate(
    PIB = PIB, 
    PIB_lag = lag(PIB)) |>
  group_by(year, quarter) |>
  mutate(
    EVLIV_m1 = EVLIV[month %% 3 == 1],   
    EVLIV_m2 = EVLIV[month %% 3 == 2],   
    EVLIV_m3 = EVLIV[month %% 3 == 0],   
    PREVPRO_m1 = PREVPRO[month %% 3 == 1],
    PREVPRO_m2 = PREVPRO[month %% 3 == 2],
    PREVPRO_m3 = PREVPRO[month %% 3 == 0],
  ) |>
  select(year, quarter, month, PIB, PIB_lag, EVLIV_m1:PREVPRO_m3)
  


test_data <- final_data |>
  filter(year < 2010)

  # M1 
reg_M1 <- lm(PIB ~ PIB_lag + EVLIV_m1 + PREVPRO_m1, data = test_data)
summary(reg_M1)


# M2 
reg_M2 <- lm(PIB ~ PIB_lag + EVLIV_m1 + EVLIV_m2 + PREVPRO_m2, data = test_data)
summary(reg_M2)

# M3 
reg_M3 <- lm(PIB ~ PIB_lag + EVLIV_m3 + EVLIV_m2 + EVLIV_m1, data = test_data)
summary(reg_M3)

# TESTING #

#Test du modèle sur la période 2010-2024

pred_data <- final_data |>
  ungroup() |>           
  arrange(year, quarter, month) |>
  filter(year >= 2010)      # période de prédiction

n <- nrow(pred_data)

# vecteurs de sortie
pred_data$forecast_M1 <- NA_real_
pred_data$forecast_M2 <- NA_real_
pred_data$forecast_M3 <- NA_real_
pred_data$forecast    <- NA_real_  # colonne d'agrégation des trois forecasts

# position dans le trimestre de chaque mois
month_position <- ((pred_data$month - 1) %% 3) + 1

# séparation des prédictions par mois : on veut 3 prédictions par trimestre et donc 1 par type de modèle selon le mois
months_1 <- which(month_position == 1)

  pred_vec1 <- predict(reg_M1, newdata = pred_data[months_1, , drop = FALSE]) # on ne drop pas les NA pour conserver un df de la même structure
  pred_data$forecast_M1[months_1] <- pred_vec1
  pred_data$forecast[months_1]    <- pred_vec1


months_2 <- which(month_position == 2)

  pred_vec2 <- predict(reg_M2, newdata = pred_data[months_2, , drop = FALSE])
  pred_data$forecast_M2[months_2] <- pred_vec2
  pred_data$forecast[months_2]    <- pred_vec2


months_3 <- which(month_position == 3)

  pred_vec3 <- predict(reg_M3, newdata = pred_data[months_3, , drop = FALSE])
  pred_data$forecast_M3[months_3] <- pred_vec3
  pred_data$forecast[months_3]    <- pred_vec3


# Message de confirmation
cat("Prévisions réalisées :", sum(!is.na(pred_data$forecast)), "/", n) #178/180 car 2025 enlevé (on pourrait enelever les rows si besoin)

# Training dataset avec les forecasts
results_forecasts <- pred_data |>
  select(year, quarter, month, PIB, PIB_lag, forecast_M1, forecast_M2, forecast_M3, forecast)

# Dataset de toutes les observations + le forecast
total_dataset <- final_data |>
  ungroup() |>
  left_join(results_forecasts |>  select(year, quarter, month, forecast), 
            by = c("year", "quarter", "month"))







############# Notes/Reflexions ####################

    #selon le mois du trimestre actuel doit renvoyer un format de prevision different (1 evliv et 1 PP ou 2-1 ou 3-0)
    #Réunir les résultats 
 

#a noter : fevrier-avril T1 mai_juillet T2 aout octobre T3 novembre janvier  T4 >>> EMC 1 est en février de l'année


#Pas de variations au sein d'une même date : RMSE et MAE varie seulement lorsque 'lon prend plusieurs date #  prédictions des LLM qui varie entre plusieurs itérations




























