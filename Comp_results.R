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
res_Text <- df_results_text_BDF # à modifier avec INSEE en plus ensuite
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


####################################
# Comparaison résultats (ISMA et Text)
#####################################
res_Text <- res_Text |>
  mutate(
    dates = Date,
    forecast_mean = rowMeans(across(starts_with("forecast_")), na.rm = TRUE)
  )


res_Text <- res_Text |>
  arrange(dates) |>
  mutate(
    trimestre_id = rep(1:ceiling(n()/3), each = 3, length.out = n()),
    mois_in_quarter = rep(1:3, times = ceiling(n()/3))[1:n()]
  )


res_Text_wide <- res_Text |>
  select(trimestre_id, mois_in_quarter, starts_with("forecast_")) |>
  pivot_wider(
    id_cols = trimestre_id,
    names_from = mois_in_quarter,
    values_from = starts_with("forecast_"),
    names_glue = "{.value}_month{mois_in_quarter}"
  )


res_Text_wide <- res_Text_wide |>
  mutate(
    year = rep(year(res_Text$dates[seq(1, nrow(res_Text), by = 3)]), each = 1, length.out = nrow(res_Text_wide)),
    month_median = rep(c(2,5,8,11), length.out = nrow(res_Text_wide)),
    dates = as.Date(paste(year, month_median, 1, sep = "-"))
  ) |>
  select(dates, forecast_mean_month1:forecast_mean_month3, -year, -month_median)

final_df <- res_Text_wide |>
  left_join(
    res_ISMA |>
      mutate(dates = floor_date(dates, unit = "month")) |>
      select(dates, forecast_M1:forecast_M3, PIB),
    by = "dates"
  )

final_df <- final_df |>
  select(dates, everything())


#### CALCUL MAE et RMSE
MAE_M1 <- mean(abs(final_df$forecast_mean_month1 - final_df$PIB))
MAE_M2 <- mean(abs(final_df$forecast_mean_month2 - final_df$PIB))
MAE_M3 <- mean(abs(final_df$forecast_mean_month3 - final_df$PIB))

MAE_i_M1 <- mean(abs(final_df$forecast_M1 - final_df$PIB))
MAE_i_M2 <- mean(abs(final_df$forecast_M2 - final_df$PIB))
MAE_i_M3 <- mean(abs(final_df$forecast_M3 - final_df$PIB))


RMSE_M1 <- sqrt(mean((final_df$forecast_mean_month1 - final_df$PIB)^2))
RMSE_M2 <- sqrt(mean((final_df$forecast_mean_month2 - final_df$PIB)^2))
RMSE_M3 <- sqrt(mean((final_df$forecast_mean_month3 - final_df$PIB)^2))

RMSE_i_M1 <- sqrt(mean((final_df$forecast_M1 - final_df$PIB)^2))
RMSE_i_M2 <- sqrt(mean((final_df$forecast_M2 - final_df$PIB)^2))
RMSE_i_M3 <- sqrt(mean((final_df$forecast_M3 - final_df$PIB)^2))

recap_df <- data.frame(MAE_M1, MAE_M2, MAE_M3,MAE_i_M1,MAE_i_M2,MAE_i_M3, RMSE_M1, RMSE_M2, RMSE_M3, RMSE_i_M1, RMSE_i_M2, RMSE_i_M3)
recap_df



