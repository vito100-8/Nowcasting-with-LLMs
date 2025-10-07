########## COMPARAISON DES RESULTATS ENTRE LES DIFFERENTS MODELES ####################

############################
# Telechargement du PIB réel
###########################

pib_reel <- read_xlsx("Data_PIB_ENQ_2.xlsx", sheet = "data_Q")
pib_reel <- pib_reel |>
  select(dates:PIB_PR)

###########################
# CALCUL RMSE ET MAE
########################

res_noText <- df_results_BDF 
res_Text <- df_results_text
res_ts<- df_results_TS
res_ISMA <- df_ISMA

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
                  ~ .x - PIB_PR, 
                  .names = "error_{.col}")) 
}

# Appliquer aux trois modèles
errors_noText <- compute_errors(res_noText, pib_reel)|>
  select(dates, error_forecast_1:last_col() )
errors_Text   <- compute_errors(res_Text, pib_reel)|>
  select(dates, error_forecast_1: last_col())
errors_ts     <- compute_errors(res_ts, pib_reel)|>
  select(dates, error_forecast_1: last_col())

#Erreur pour chaque prévision ISMA
error_ISMA <- res_ISMA |>
  mutate( "dates" = dates, 
          Error_M1 = PIB_PR - forecast_M1,
          Error_M2 =  PIB_PR -forecast_M2,
          Error_M3 = PIB_PR -forecast_M3,
          .keep = "none")



#Regrouper les erreurs de chaque modèle
errors <- errors_ts |>
  rename_with(~ paste0(.x, "_TS"), starts_with("error_forecast_"))  |>
  left_join(
    errors_Text |> rename_with(~ paste0(.x, "_Text"), starts_with("error_forecast_")),
    by = "dates"
  )  |>
  left_join(
    errors_noText |> rename_with(~ paste0(.x, "_noText"), starts_with("error_forecast_")),
    by = "dates"
  )


#################################
#Comparaison erreurs modèles LLMs
#######################

#Moyenne des erreurs à chaque date
Avg_error_LLM <- errors |>
  rowwise()|>
  mutate(
    TS = mean(c_across(starts_with("error_forecast_") & ends_with("_TS")), na.rm = TRUE),
    Text = mean(c_across(starts_with("error_forecast_") & ends_with("_Text")), na.rm = TRUE),
    noText = mean(c_across(starts_with("error_forecast_") & ends_with("_noText")), na.rm = TRUE),
    .keep = "none"
  ) |>
  ungroup()

#Test pour comparer la précision des forecasts de LLMs : Diebold Mariano (autre test plus pertinent ?)
dm.test(TS, Text, power = 2)
dm.test(TS, noText, power = 2)
dm.test(Text, noText, power = 2) # WIP A VOIR EN ESSAYANT AVEC UN NOMBRE HARMONISER DE FORECAST -> faire un MCS PLUTOT ?

###################################
#RMSE et MAE ISMA et autres modèles 
####################################

## Méthode : moyenne des erreurs de forecast à chaque dates des LLM 
### MFE = Mean forecast error FE = Forecast error

avg_error_all <- errors |> 
  inner_join(error_ISMA, by = "dates") |>
  rowwise() |>
  mutate(
    "noText MFE" = mean(c_across(starts_with("error_forecast_") & ends_with("_noText"))),
    "Text MFE" = mean(c_across(starts_with("error_forecast_") & ends_with("_Text"))),
    "TS MFE" = mean(c_across(starts_with("error_forecast_") & ends_with("_TS"))),
    "ISMA_FE" = Error #revoir avec nv df
            ) |>
  ungroup() |>
  select(dates, noText_mFE, Text_mFE, TS_mFE, ISMA_FE)

# RMSE
RMSE_df <- avg_error_all |>
  summarise(
    RMSE_noText = sqrt(mean(noText_MFE^2, na.rm = TRUE)),
    RMSE_Text   = sqrt(mean(Text_MFE^2,   na.rm = TRUE)),
    RMSE_TS     = sqrt(mean(TS_MFE^2,     na.rm = TRUE)),
    RMSE_ISMA   = sqrt(mean(ISMA_FE^2,    na.rm = TRUE))
  )

# MAE
MAE_df <- avg_error |>
  summarise(
    MAE_noText = mean(abs(noText_mFE)),
    MAE_Text   = mean(abs(Text_mFE)),
    MAE_TS     = mean(abs(TS_mFE)),
    MAE_ISMA   = mean(abs(ISMA_FE))
  )

# RAJOUTER UN MCS AVEC ISMA AUSSI ?

#prendre une mediane plutot
#prendre une prévision moyenne des forecasts LLM ET Econometrique
#Forme simple : simple moy arithmétique
# Sinon : moyenne qui pondère selon les performances récentes(Bate-Granger)/ poids inverse de l'erreur carré
