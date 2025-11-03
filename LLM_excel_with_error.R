#INSERTION D'UN EXCEL ITERATIF AVEC ERREUR DU LLM SELON LA PERIODE DE PREVISION AU LLM#


rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")

#######################
#Paramètres spécifiques
#######################

#Systeme prompt
sys_prompt <- ifelse(english == 1,
                     "You will act as the economic agent you are told to be. Answer based on your knowledge and the document provided in less than 200 words. You will use only the information available as of the forecast date, do not invent facts." ,
                     "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aide de vos connaissances et du document fourni. Vous n'utiliserez que l'information disponible à la date du jour de la prévision, n'inventez pas de faits."
)

#Initialisation LLM
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")
chat_gemini <- chat_google_gemini( system_prompt = sys_prompt,
                                   base_url = "https://generativelanguage.googleapis.com/v1beta/", 
                                   api_key = cle_API, 
                                   model = "gemini-2.5-pro", 
                                   params(temperature = temp_LLM, max_tokens = 5000)
)


#################################
#Initialisation variables/données
#################################

#Téléchargement données
df_PIB<- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")
df_enq_BDF <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_BDF")
df_enq_INSEE <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_INSEE")

#Bien transformer les dates en un vecteur
dates <- if (is.data.frame(dates)) as.Date(dates[[1]]) else as.Date(dates)

# Variables contenant les futures erreurs de prévision
errors_BDF <- rep(NA_real_, length(dates_vector))
errors_INSEE <- rep(NA_real_, length(dates_vector))

# variables pour associer les erreurs ux lignes correspondantes dans df_enq_XXX
errors_by_data_BDF <- list()
errors_by_data_INSEE <- list()

# Forecast regex pattern qui sera appelé dans la boucle pour parse
forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"


#################
#Nettoyage données
##################


#Nettoyage de df_enq_INSEE

df_enq_INSEE <- df_enq_INSEE |>
  mutate(
    dates = str_replace_all(dates, 
                            c("janv\\." = "jan", "févr\\." = "feb", "mars" = "mar",
                              "avr\\." = "apr", "mai" = "may", "juin" = "jun",
                              "juil\\." = "jul", "août" = "aug", "sept\\." = "sep",
                              "oct\\." = "oct", "nov\\." = "nov", "déc\\." = "dec"))
  ) |>
  mutate(dates_temp = as.Date(parse_date_time(dates, orders = "b Y"), origin = "1970-01-01")) 


v_dates <- rollforward(df_enq_INSEE$dates_temp)
df_enq_INSEE <- df_enq_INSEE |>
  mutate(dates = v_dates) |>
  select(!dates_temp)


# colonnes de dates en classe Date
if ("dates" %in% names(df_enq_BDF)) df_enq_BDF$dates <- as.Date(df_enq_BDF$dates)
if ("dates" %in% names(df_enq_INSEE)) df_enq_INSEE$dates <- as.Date(df_enq_INSEE$dates)
if ("dates" %in% names(df_PIB)) df_PIB$dates <- as.Date(df_PIB$dates)

####################################
# Prompts
###################################

if (english == 1) {
  try(Sys.setlocale("LC_TIME", "English"), silent = TRUE)
  
  #Renvoie le bon dirigeant
  current_boss <- function(type, d) {
    if (type == "BDF") return(BDF_current_boss(d))
    if (type == "INSEE") return(INSEE_current_boss(d))
  }
  
  # 
  prompt_template <- function(type, d, q_trim, y_prev) {
    boss <- current_boss(type, d)
    position <- ifelse(type == "BDF", "Governor of the Banque de France" , "Director General of INSEE")
    current_quarter <- if (q_trim == 1){
      "first"}
    else if (q_trim == 2){
      "second"
    }else if (q_trim == 3){
      "third"}else{
        "fourth"}
    
    
    paste0(
      "Forget the previous instructions and answers. You are ", boss, ", ", position, 
      ", giving a speech about the economic outlook of France. Today is ",
      format(d, "%d %B %Y"), ". ",
      "You will be provided with a document with information about the current state and recent past of the French economy. ",
      "Using only the information in that document and information that was available on or before ", format(d, "%d %B %Y"),
      ", provide a numeric forecast (decimal percent with sign, e.g., +0.3) for French real GDP growth in the ", current_quarter, " quarter of ", y_prev,
      " and a confidence level (integer 0–100). Output EXACTLY in this format on a single line (no extra text):\n",
      "<forecast> (<confidence>)\nExample: +0.3 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), "."
    )
  }
  
} else {
  try(Sys.setlocale("LC_TIME", "French"), silent = TRUE)
  
  current_boss <- function(type, d) {
    if (type == "BDF") return(BDF_current_boss(d))
    if (type == "INSEE") return(INSEE_current_boss(d))
  }
  
  prompt_template <- function(type, d, q_trim, y_prev) {
    boss <- current_boss(type, d)
    position <- ifelse(type == "BDF","Gouverneur de la Banque de France", "Directeur Général de l'INSEE")  
    trimestre_actuel <- if (q_trim == 1){
      "premier"}
    else if (q_trim == 2){
      "second"
    }else if (q_trim == 3){
      "troisième"}else{
        "quatrième"}
    
    
    paste0(
      "Oubliez les instructions et les réponses précédentes. Vous êtes ", boss, ", ", position,
      ", qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "Vous recevrez un document concernant la situation actuelle et passée de l'économie française. ",
      "En utilisant UNIQUEMENT les informations contenues dans ce document et celles disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) de la croissance du PIB réel français pour le ",
      trimestre_actuel, " trimestre ", y_prev,
      " et un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) :\n",
      "<prévision> (<confiance>)\nExemple : +0.3 (80)\n",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}


###########################################
# BOUCLES ITERATIVES SELON LA DATE
#############################################


# BDF #
dir.create("Indicateurs_BDF_error", showWarnings = FALSE)

results_uploads_BDF <- list()
row_id_BDF <- 1

for (idx in seq_along(dates)) {
  current_date <- as.Date(dates[idx])
  
  # prendre les données les plus récentes
    avail_idx <- which(as.Date(df_enq_BDF$dates) <= current_date)
    if (length(avail_idx) >= 1) {
      data_date <- as.Date(df_enq_BDF$dates[max(avail_idx)])   #date où l'on tronque les données trouvée
      df_temp <- df_enq_BDF |> filter(dates <= data_date)
    } else {
      #si pas de données alors on prend juste la première ligne pour ne pas avoir un df vide
      data_date <- as.Date(df_enq_BDF$dates[which.min(as.Date(df_enq_BDF$dates))])
      df_temp <- df_enq_BDF |> filter(dates <= data_date)
    }
  
  
  # imposer type Date
  if ("dates" %in% names(df_temp)) df_temp$dates <- as.Date(df_temp$dates)
  
  # remplir LLM_prev_error avec l'erreur d'une date si erreur existante
  df_temp$LLM_prev_error <- sapply(df_temp$dates, function(d) {
    key <- as.character(d)
    if (!is.null(errors_by_data_BDF[[key]])) as.character(errors_by_data_BDF[[key]]) else NA_character_
  })
  
  # Lag pour avoir l'erreur de t-1 en t-1
  lagged_errors <- sapply(seq_along(errors_BDF), function(i) {
    if (i > 1) as.character(errors_BDF[i - 1]) else NA_character_
  })
  
  #Construction de history_rows (appel à la fonction pour créer un df paralèle à df_temp)
  tmpl <- if (nrow(df_temp) > 0) df_temp else df_enq_BDF
  hist_rows <- make_hist_rows(tmpl, dates_vector, lagged_errors, prev_col = "LLM_prev_error")
  
  # Concaténation des deux df
  export_df <- bind_rows(df_temp, hist_rows)
  
  #Supprimer les lignes en trop créées
  n_remove <- length(dates_vector)
  if (nrow(export_df) > n_remove) {
    export_df <- export_df[seq_len(nrow(export_df) - n_remove), , drop = FALSE]
  } else {
    export_df <- export_df[0, , drop = FALSE]
  }
  
  #Création du csv et envoi
  file_name <- paste0("date_max_BDF_", format(current_date, "%Y%m%d"), ".csv")
  file_path <- file.path("Indicateurs_BDF_error", file_name)
  write.csv(export_df, file = file_path, row.names = FALSE, fileEncoding = "UTF-8")
  uploaded_doc <- google_upload(file_path, api_key = cle_API)
  
  # preparation prompt
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
  prompt_text <- prompt_template("BDF", current_date, trimestre_index, year_prev)
  
  # call LLM
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- chat_gemini$chat(uploaded_doc, prompt_text)
      return(resp)
    }, error = function(e) NA_character_)
  }, future.seed = TRUE)
  
  # parse resultats
  histoires <- sapply(out_list, function(x) {
    if (is.list(x) && !is.null(x$text)) x$text else if (is.character(x)) x else NA_character_
  })
  parsed_list <- lapply(histoires, function(txt) {
    if (is.null(txt) || length(txt) == 0) return(list(forecast = NA_real_, confidence = NA_integer_, raw = NA_character_))
    m <- regmatches(txt, regexec(forecast_confidence_pattern, txt))
    if (length(m[[1]]) >= 3) list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt) else list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
  })
  
  # prendre la médiane des prévisions, si n_repro = 1 alors la prévision elle-même
  forecasts_vec <- sapply(parsed_list, function(x) as.numeric(x$forecast))
  median_forecast <- if (n_repro >= 2) {
    if (all(is.na(forecasts_vec))) NA_real_ else median(forecasts_vec, na.rm = TRUE)
  } else {
    if (length(forecasts_vec) >= 1) as.numeric(forecasts_vec[1]) else NA_real_
  }
  
  # Calcul de l'erreur et enregistrement de l'erreur dans la ligne correspondante à la date actuelle
  error_current <- NA_real_
  pib_idx <- find_pib_index(current_date)
  if (length(pib_idx) == 1 && "PIB_PR" %in% names(df_PIB) && !is.na(median_forecast)) {
    error_current <- as.numeric(df_PIB$PIB_PR[pib_idx]) - median_forecast
  }
  errors_BDF[idx] <- error_current
  
  # Pour que lorsque l'on crée le prochain doc, l'erreur s'affiche à la bonne ligne
  if (exists("data_date")) {
    errors_by_data_BDF[[as.character(data_date)]] <- error_current
  }
  
  #enregistrement des résultats
  df_excel_BDF <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_excel_BDF[[paste0("forecast_", i)]] <- parsed_list[[i]]$forecast
    df_excel_BDF[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_excel_BDF[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
  }
  
  df_excel_BDF$median_forecast <- median_forecast
  df_excel_BDF$forecast_error <- error_current
  results_uploads_BDF[[row_id_BDF]] <- df_excel_BDF
  
  row_id_BDF <- row_id_BDF + 1
  Sys.sleep(0.5)
}

# création excel de résultats
df_excel_BDF <- do.call(rbind, results_uploads_BDF)
write.xlsx(df_excel_BDF, file = "indicateurs_BDF_error/upload_summary.xlsx", overwrite = TRUE)




# INSEE #

#même étapes que pour la boucle BDF

dir.create("indicateurs_INSEE_error", showWarnings = FALSE)

results_uploads_INSEE <- list()
row_id_INSEE <- 1

for (idx in seq_along(dates)) {
  current_date <- as.Date(dates[idx])

  if ("dates" %in% names(df_enq_INSEE)) {
    avail_idx <- which(as.Date(df_enq_INSEE$dates) <= current_date)
    if (length(avail_idx) >= 1) {
      data_date <- as.Date(df_enq_INSEE$dates[max(avail_idx)])
      df_temp <- df_enq_INSEE |> filter(dates <= data_date)
    } else {
      data_date <- as.Date(df_enq_INSEE$dates[which.min(as.Date(df_enq_INSEE$dates))])
      df_temp <- df_enq_INSEE |> filter(dates <= data_date)
    }
  } else {
    data_date <- as.Date(current_date)
    df_temp <- df_enq_INSEE |> filter(dates <= current_date)
  }
  
  if ("dates" %in% names(df_temp)) df_temp$dates <- as.Date(df_temp$dates)
  
  df_temp$LLM_prev_error <- sapply(df_temp$dates, function(d) {
    key <- as.character(d)
    if (!is.null(errors_by_data_INSEE[[key]])) as.character(errors_by_data_INSEE[[key]]) else NA_character_
  })
  
  lagged_errors_insee <- sapply(seq_along(errors_INSEE), function(i) {
    if (i > 1) as.character(errors_INSEE[i - 1]) else NA_character_
  })
  
  tmpl_insee <- if (nrow(df_temp) > 0) df_temp else df_enq_INSEE
  hist_rows <- make_hist_rows(tmpl_insee, dates_vector, lagged_errors_insee, prev_col = "LLM_prev_error")
  
  export_df <- dplyr::bind_rows(df_temp, hist_rows)
  

  n_remove <- length(dates_vector)
  if (nrow(export_df) > n_remove) {
    export_df <- export_df[seq_len(nrow(export_df) - n_remove), , drop = FALSE]
  } else {
    export_df <- export_df[0, , drop = FALSE]
  }
  
  file_name <- paste0("date_max_INSEE_", format(current_date, "%Y%m%d"), ".csv")
  write.csv(export_df, file = file.path("indicateurs_INSEE_error", file_name), row.names = FALSE, fileEncoding = "UTF-8")
  cat("Fichier CSV créé :", file_name, "\n")
  input_doc <- normalizePath(file.path("indicateurs_INSEE_error", file_name), winslash = "/", mustWork = TRUE)
  uploaded_doc <- google_upload(input_doc, api_key = cle_API)

  
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
  prompt_text <- prompt_template("INSEE", current_date, trimestre_index, year_prev)
  
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- chat_gemini$chat(uploaded_doc, prompt_text)
      return(resp)
    }, error = function(e) {
      message("API error: ", conditionMessage(e))
      return(NA_character_)
    })
  }, future.seed = TRUE)
  histoires <- sapply(out_list, function(x) {
    if (is.list(x) && !is.null(x$text)) return(x$text) else if (is.character(x)) return(x) else return(NA_character_)
  })
  parsed_list <- lapply(histoires, function(txt) {
    if (is.null(txt) || length(txt) == 0) return(list(forecast = NA_real_, confidence = NA_integer_, raw = NA_character_))
    m <- regmatches(txt, regexec(forecast_confidence_pattern, txt))
    if (length(m[[1]]) >= 3) list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt) else list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
  })
  
  forecasts_vec <- sapply(parsed_list, function(x) as.numeric(x$forecast))
  median_forecast <- if (n_repro >= 2) {
    if (all(is.na(forecasts_vec))) NA_real_ else median(forecasts_vec, na.rm = TRUE)
  } else {
    if (length(forecasts_vec) >= 1) as.numeric(forecasts_vec[1]) else NA_real_
  }
 
   error_current <- NA_real_
  pib_idx <- find_pib_index(current_date)
  if (length(pib_idx) == 1 && "PIB_PR" %in% names(df_PIB) && !is.na(median_forecast)) {
    error_current <- as.numeric(df_PIB$PIB_PR[pib_idx]) - median_forecast
  }
  errors_INSEE[idx] <- error_current
  
  if (exists("data_date")) {
    errors_by_data_INSEE[[as.character(data_date)]] <- error_current
  }
  df_excel_INSEE <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_excel_INSEE[[paste0("forecast_", i)]]  <- parsed_list[[i]]$forecast
    df_excel_INSEE[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_excel_INSEE[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
  }
  
  
  df_excel_INSEE$median_forecast <- median_forecast
  df_excel_INSEE$forecast_error <- error_current
  results_uploads_INSEE[[row_id_INSEE]] <- df_excel_INSEE
  
  row_id_INSEE <- row_id_INSEE + 1
  Sys.sleep(0.5)
}

df_excel_INSEE <- do.call(rbind, results_uploads_INSEE)
write.xlsx(df_excel_INSEE, file = "indicateurs_INSEE_error/upload_summary.xlsx", overwrite = TRUE)





