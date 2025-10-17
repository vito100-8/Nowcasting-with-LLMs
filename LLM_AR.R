# BENCHMARK AR(2)


######################
#Paramètres
##################

#Taille fenêtre
window <- 80
#ROLLING WINDOW OU RECURSIF
rolling <- TRUE 

###############################
#Ouverture et traitement données
##################################

df_PIB_ENQ <- read_xlsx("Data_PIB_ENQ_2.xlsx", sheet = "data_Q")

df_PIB_ENQ_AR <-  df_PIB_ENQ |>
  select(!EVLIV_M1:PREVPRO_M3) |>
  mutate(PIB_lag = lag(PIB_PR),
         PIB_lag_2 = lag(PIB_lag)) |>
  mutate(
    PIB_lag = ifelse(row_number() == 1 & is.na(PIB_lag),
                     mean(PIB_PR),
                     lag(PIB_PR)), 
    PIB_lag_2 = ifelse(row_number() %in% c(1,2) & is.na(PIB_lag_2),
                       mean(PIB_lag),
                       lag(PIB_lag)), 
    dates = as.Date((dates), format = "%Y-%m-%d")) 

####################################
# Choix méthode pour trimestres covid
###################################

covid_treatment <- 1 #  0 pour méthode dummy, 1 pour méthode où l'on intègre pas les données covid dans le training dataset

#Dummy de 2020Q1 à 2021Q4
if (covid_treatment == 0 ){
  df_PIB_ENQ_AR <- df_PIB_ENQ_AR |>
    mutate(COVID = ifelse(dates > as.Date("2020-01-01") & dates < as.Date("2022-01-01"), 1, 0))
  
}



#######
#Modèle
########

#Date de début de training
start_forecast_date <- as.Date("2010-02-01") #Peut être automatisé car là nécessaire de rentrer une date comprise dans df_PIB_Q$dates
## Poistion dans le dataset
first_forecast_row <- which(df_PIB_ENQ_AR$dates >= start_forecast_date)[1]

# Verif pour la date
if (is.na(first_forecast_row)) {
  stop("Date de début de prévision non trouvée dans le dataset.")
}

# Stockage des résultats (dates, PIB_PR et prévisions)
AR_forecast_results <- tibble(
  dates = as.Date(character()), 
  PIB_PR = double(),     
  forecast_AR = double(),       
)



# Boucle de prévision : itérer jusqu'à la fin du dataset depuis la date de training choisie
for (i in first_forecast_row:nrow(df_PIB_ENQ_AR)) {
  
  # Date du forecast
  current_forecast_date <- df_PIB_ENQ_AR$dates[i]
  
  # Sélection dataset d'entrainement
  if (covid_treatment == 1){
    
    if (current_forecast_date >=  "2020-02-01" && current_forecast_date < "2022-01-01"){
      # Dataset d'entrainement
      i_2019_T4 <- which(df_PIB_ENQ_AR$dates == "2019-11-01")[1]
      training_data <- df_PIB_ENQ_AR[(ifelse(rolling == TRUE, i-window, 1)):(i_2019_T4),  ]
    }else{
      training_data <- df_PIB_ENQ_AR[(ifelse(rolling == TRUE, i-window, 1)):(i-1),  ]
    }
    
  }else{training_data <- df_PIB_ENQ_AR[(ifelse(rolling == TRUE, i-window, 1)):(i-1),  ]
  }
  
  # Estimation du modèle (réestimé à chaque boucle)
  reg_AR <- lm(PIB_PR ~ PIB_lag + PIB_lag_2 + INDIC09, data = training_data)

  
  #Newdata dans predict (on utilise les variables en t pour prévoir le PIB de t)
  forecast_data_current_quarter <- df_PIB_ENQ_AR[i, ]
  
  # Prévision
  forecast_AR_val <- predict(reg_AR, newdata = forecast_data_current_quarter)

  
  
  #Stockage des résultats
  AR_forecast_results <- AR_forecast_results |>
    add_row(
      dates = current_forecast_date,
      PIB_PR = df_PIB_ENQ_AR$PIB_PR[i],
      forecast_AR = forecast_AR_val,
    ) 
  
}



#Df pour comparaison des résultats

df_AR <- AR_forecast_results



#########################
# Analyse des résultats
#########################

#MAE

MAE_AR <- mean(abs(df_AR$forecast_AR - df_AR$PIB_PR))



