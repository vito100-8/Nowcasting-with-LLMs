# BENCHMARK : MODELE ISMA

#Forme : données de 1990 à fin 2024, régression linéaire de 1990 à 2010 puis nowcasting du PIB de 2010 à fin 2024


rm(list = ls())

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

#Dummy de 2020Q1 à 2021Q4

df_PIB_ENQ <- df_PIB_ENQ |>
  mutate(COVID = ifelse(dates > as.Date("2020-01-01") & dates < as.Date("2022-01-01"), 1, 0))


#Estimation du modèle en dehors de la période 

df_PIB_ENQ <- df_PIB_ENQ |>
  filter(dates < as.Date("2020-01-01") | dates > as.Date("2022-01-01"))


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
  
  # Dataset d'entrainement
  training_data <- df_PIB_ENQ[(ifelse(rolling == TRUE, i-window, 1)):(i-1),  ]
  
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

df_ISMA <- forecast_results |>
  mutate(PIB = PIB_PR) 


#########################
# Analyse des résultats
#########################

qqnorm(na.omit(df_ISMA$residuals_M1), main = "QQ-Plot des Résidus M1")
qqline(na.omit(df_ISMA$residuals_M1))








###################################

#jpg pour envoyer résultats de régression
jpeg("reg_M3_summary.jpg", width = 800, height = 600, quality = 100)
plot.new()
text(0, 1, paste(capture.output(summary(reg_M1)), collapse = "\n"), adj = c(0,1), cex = 0.8)
dev.off()


#covid quelle méthode ? : dummy ou suppression observation
#MAE et RMSE direct ? Ou juste erreur directe



