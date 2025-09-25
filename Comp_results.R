########## COMPARAISON DES RESULTATS ENTRE LES DIFFERENTS MODELES ####################

############################
# Telechargement du PIB réel
###########################

pib_reel <- read_xlsx("Data_PIB_ENQ.xlsx", sheet = "PIB")
pib_reel <- pib_reel |>
  select(dates:PIB_PR)

###########################
# CALCUL RMSE ET MAE
########################

res_noText <- df_results_BDF 
res_Text <- df_results_text
res_ts<- df_results_TS


# Prendre le trimestre observé


pib_reel <- pib_reel |>
  mutate(Date    = as.Date(dates),             
    Year = year(Date),               
    Month = month(Date),                 
    Quarter = case_when(                  
      Month == 2  ~ 1,
      Month == 5  ~ 2,
      Month == 8  ~ 3,
      Month == 11 ~ 4
    ) )


# Fonction pour appareiller la date de prévision au PIB du trimestre adéquate
map_forecast_to_quarter <- function(date_forecast) {
  m <- month(date_forecast)
  y <- year(date_forecast)
  
  if (m %in% c(11, 12, 1)) {
    q <- 4
    y <- ifelse(m == 1, y - 1, y)  
  } else if (m %in% 2:4) {
    q <- 1
  } else if (m %in% 5:7) {
    q <- 2
  } else {
    q <- 3
  }
  return(c(y, q))
}

# Fonction pour calculer les erreurs avec prise en compte du trimestre
compute_errors <- function(df_model, df_obs) {
  df_model2 <- df_model |>
    mutate(Date_forecast = as.Date(Date)) |>
    rowwise() |>
    mutate(tmp = list(map_forecast_to_quarter(Date_forecast))) |>
    mutate(Year = tmp[1], Quarter = tmp[2]) |>
    select(-tmp)
  
  df_model2 |>
    left_join(df_obs, by = c("Year", "Quarter")) |>
    mutate(across(starts_with("forecast_"), 
                  ~ .x - PIB, 
                  .names = "error_{.col}")) 
}

# Appliquer aux trois modèles
errors_noText <- compute_errors(res_noText, pib_reel)|>
  select(dates, error_forecast_1:last_col() )
errors_Text   <- compute_errors(res_Text, pib_reel)|>
  select(dates, error_forecast_1: last_col())
errors_ts     <- compute_errors(res_ts, pib_reel)|>
  select(dates, error_forecast_1: last_col())


#Agréger en un tableau
errors <- errors_ts |>
  left_join(errors_Text, by = "dates") |>
  left_join(errors_noText, by = "dates")



