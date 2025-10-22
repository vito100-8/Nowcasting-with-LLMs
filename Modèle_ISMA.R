# BENCHMARK : MODELE ISMA

#Forme : données de 1990 à fin 2024, régression linéaire de 1990 à 2010 puis nowcasting du PIB de 2010 à fin 2024


rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")



######################
#PARAMETRES UTILISATEUR
#######################

#Taille fenêtre
window <- 80
#ROLLING WINDOW OU RECURSIF
rolling <- TRUE 



#####################
#INITIALISATION
######################
df_PIB_ENQ <- read_xlsx("Data_PIB_ENQ_2.xlsx", sheet = "data_Q")

df_PIB_ENQ <-  df_PIB_ENQ |>
  mutate(PIB_lag = lag(PIB_PR)) |>
  mutate(
    PIB_lag = ifelse(row_number() == 1 & is.na(PIB_lag),
                     mean(PIB_PR),
                     lag(PIB_PR)), 
         dates = as.Date((dates), format = "%Y-%m-%d")) 

####################################
# Choix méthode pour trimestres covid
###################################

covid_treatment <- 1 #  0 pour méthode dummy, 1 pour méthode où l'on intègre pas les données covid dans le training dataset

#Dummy de 2020Q1 à 2021Q4
if (covid_treatment == 0 ){
  df_PIB_ENQ <- df_PIB_ENQ |>
    mutate(COVID = ifelse(dates > as.Date("2020-01-01") & dates < as.Date("2022-01-01"), 1, 0))
  
}


###############################
# Boucle prévision
###############################

#Date de début de training
start_forecast_date <- as.Date("2010-02-01") #Peut être automatisé car là nécessaire de rentrer une date comprise dans df_PIB_Q$dates
## Poistion dans le dataset
first_forecast_row <- which(df_PIB_ENQ$dates >= start_forecast_date)[1]

# Verif pour la date
if (is.na(first_forecast_row)) {
  stop("Date de début de prévision non trouvée dans le dataset.")
}

# Stockage des résultats (dates, PIB_PR et prévisions)
forecast_results <- tibble(
  dates = as.Date(character()), 
  PIB_PR = double(),     
  forecast_M1 = double(),       
  forecast_M2 = double(),       
  forecast_M3 = double())



# Boucle de prévision : itérer jusqu'à la fin du dataset depuis la date de training choisie
for (i in first_forecast_row:nrow(df_PIB_ENQ)) {
 
  # Date du forecast
  current_forecast_date <- df_PIB_ENQ$dates[i]
  
  # Sélection dataset d'entrainement
  if (covid_treatment == 1){
    
    if (current_forecast_date >=  "2020-02-01" && current_forecast_date < "2022-01-01"){
      # Dataset d'entrainement
      i_2019_T4 <- which(df_PIB_ENQ$dates == "2019-11-01")[1]
      training_data <- df_PIB_ENQ[(ifelse(rolling == TRUE, i-window, 1)):(i_2019_T4),  ]
    }else{
      training_data <- df_PIB_ENQ[(ifelse(rolling == TRUE, i-window, 1)):(i-1),  ]
    }
    
  }else{training_data <- df_PIB_ENQ[(ifelse(rolling == TRUE, i-window, 1)):(i-1),  ]
  }
  
  # Estimation du modèle (réestimé à chaque boucle)
  reg_M1 <- lm(PIB_PR ~ PIB_lag + EVLIV_M1 + PREVPRO_M1 + INDIC09, data = training_data)
  reg_M2 <- lm(PIB_PR ~ PIB_lag + EVLIV_M2 + EVLIV_M1 + PREVPRO_M2 + INDIC09, data = training_data)
  reg_M3 <- lm(PIB_PR ~ PIB_lag + EVLIV_M3 + EVLIV_M2 + EVLIV_M1 + INDIC09, data = training_data)
  
  #Newdata dans predict (on utilise les variables en t pour prévoir le PIB de t)
  forecast_data_current_quarter <- df_PIB_ENQ[i, ]
  
  # Prévision
    forecast_M1_val <- predict(reg_M1, newdata = forecast_data_current_quarter)
    forecast_M2_val <- predict(reg_M2, newdata = forecast_data_current_quarter)
    forecast_M3_val <- predict(reg_M3, newdata = forecast_data_current_quarter)
 
  
  #Stockage des résultats
  forecast_results <- forecast_results |>
    add_row(
      dates = current_forecast_date,
      PIB_PR = df_PIB_ENQ$PIB_PR[i],
      forecast_M1 = forecast_M1_val,
      forecast_M2 = forecast_M2_val,
      forecast_M3 = forecast_M3_val
    ) 
  
}



#Df pour comparaison des résultats

df_ISMA <- forecast_results
 


#########################
# Analyse des résultats
#########################

#MAE

M1 <- mean(abs(df_ISMA$forecast_M1 - df_ISMA$PIB_PR))
M2 <- mean(abs(df_ISMA$forecast_M2 - df_ISMA$PIB_PR))
M3 <- mean(abs(df_ISMA$forecast_M3 - df_ISMA$PIB_PR))


