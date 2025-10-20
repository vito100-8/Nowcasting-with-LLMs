## FONCTION UTILITAIRES ##

#############
#Multi-usages
############

# get_last_doc : retourne le nom du fichier (ex: "EMC_2_2023") le plus récent disponible par rapport à la date où on se place
get_last_doc <- function(date_prev_df, target_date) {
  # target_date is Date
  candidats <- date_prev_df |>
    filter(date_finale_d <= as.Date(target_date))
  if (nrow(candidats) == 0) {
    warning(paste("Aucun document disponible avant", target_date))
    return(NULL)
  }
  dernier <- candidats |>
    arrange(desc(date_finale_d)) |>
    slice(1) |>
    pull(fichier)
  return(dernier)
}

# path_from_docname : renvoie chemin complet vers le PDF local 
path_from_docname <- function(doc_name, folder) {
  if (is.null(doc_name)) return(NULL)
  
  if (!grepl("\\.pdf$", doc_name, ignore.case = TRUE))
    doc_name <- paste0(doc_name, ".pdf")
  
  path <- file.path(folder, doc_name)
  
  if (!file.exists(path)) {
    warning("Fichier introuvable : ", path)
    return(NULL)
  }
  
  return(normalizePath(path, winslash = "/", mustWork = TRUE))
}


#Dirigeant de l'INSEE selon la date
INSEE_current_boss <- function(y_p){
  
  if(y_p <= as.Date("2012-02-21")){
    return("Jean-Philippe Cotis")
  }else if (y_p <= as.Date("2025-05-31")){
    return("Jean-Luc Tavernier")
  }else{
    return("Fabrice Lenglart")
  }
}


#Dirigeant de la BDF selon la date
BDF_current_boss <- function(y_p){
  
  if(y_p <= as.Date("2002-12-31")){
    return("Jean-Claude Trichet")
  }else if (y_p <= as.Date("2015-10-31")){
    return("Christian Noyer")
  }else{
    return("François Villeroy de Galhau")
  }
}


#Concaténer les enquêtes de l'INSEE/BDF en un PDF
merge_pdfs_INSEE <- function(files, output_path) {
  pdf_combine(input = files, output = output_path)
  return(output_path)
} 

###########
# LLM_Text
##########

#Rechercher l'EMC du lendemain de la date de prévision
get_next_doc <- function(date_prev_df, cur_day) {
  # target_date is Date of the corresponding EMC
  target_date <- as.Date(cur_day) + 1L
  next_doc <- date_prev_df |>
    filter(date_finale_d == as.Date(target_date))
  return(next_doc$fichier)
}


# Obtenir depuis le dossier les 3 documents : SER, BAT, et EMI en les cherchant par date
get_last_insee_docs_by_type <- function(target_date, doc_type, folder_to_search) {
  
  target_date <- as.Date(target_date)
  
  # Format : AAAA_MM_TYPE.pdf
  pattern <- paste0("^(\\d{4})_(\\d{2})_", doc_type, "\\.pdf$")
  all_files <- list.files(folder_to_search, pattern = pattern, full.names = FALSE)
  
  if (length(all_files) == 0) {
    warning(paste("Aucun fichier", doc_type, "trouvé dans", folder_to_search))
    return(NULL)
  }
  
  # Extraction stricte : 4 chiffres + underscore + 2 chiffres
  file_dates_df <- tibble(
    filename = all_files,
    year  = as.integer(str_extract(all_files, "^\\d{4}")),
    month = as.integer(str_extract(all_files, "(?<=\\d{4}_)\\d{2}"))
  ) |>
    mutate(doc_date = ymd(paste(year, month, "01", sep = "-")))
  
  doc_possible <- file_dates_df |>
    filter(doc_date < target_date)
  
  if (nrow(doc_possible) == 0) {
    warning("Aucun document antérieur à la date cible")
    return(NULL)
  }
  
  most_recent_doc_filename <- doc_possible |>
    arrange(desc(doc_date)) |>
    slice(1) |> 
    pull(filename)
  
  full_path <- path_from_docname(most_recent_doc_filename, folder = folder_to_search) 
  return(full_path)
}



#Concaténer les 3 enquêtes de l'INSEE
merge_pdfs <- function(files, output_path) {
  pdf_combine(input = files, output = output_path)
  return(output_path)
} 


################
#LLM_Text_12
################

# get_last_doc : retourne le nom du fichier (ex: "EMC_2_2023") le plus récent disponible par rapport à la date où on se place
get_last_12_doc <- function(date_prev_df, target_date) {
  # target_date is Date
  candidats <- date_prev_df %>%
    filter(date_finale_d <= as.Date(target_date))
  
  if (nrow(candidats) == 0) {
    warning(paste("Aucun document disponible avant", target_date))
    return(NULL)
  }
  
  derniers <- candidats %>%
    arrange(desc(date_finale_d)) %>%
    slice_head(n = 12) %>%   #prendre les 12 dernières enquêtes BDF
    pull(fichier)
  
  return(derniers)
}


# Obtenir depuis le dossier les 3 documents : SER, BAT, et EMI en les cherchant par date
get_last_12_insee_docs_by_type <- function(target_date, doc_type, folder_to_search) {
  
  target_date <- as.Date(target_date)
  
  # Format : AAAA_MM_TYPE.pdf
  pattern <- paste0("^(\\d{4})_(\\d{2})_", doc_type, "\\.pdf$")
  all_files <- list.files(folder_to_search, pattern = pattern, full.names = FALSE)
  
  if (length(all_files) == 0) {
    warning(paste("Aucun fichier", doc_type, "trouvé dans", folder_to_search))
    return(NULL)
  }
  
  # Extraction stricte : 4 chiffres + underscore + 2 chiffres
  file_dates_df <- tibble(
    filename = all_files,
    year  = as.integer(str_extract(all_files, "^\\d{4}")),
    month = as.integer(str_extract(all_files, "(?<=\\d{4}_)\\d{2}"))
  ) |>
    mutate(doc_date = ymd(paste(year, month, "01", sep = "-")))
  
  doc_possible <- file_dates_df |>
    filter(doc_date < target_date)
  
  if (nrow(doc_possible) == 0) {
    warning("Aucun document antérieur à la date cible")
    return(NULL)
  }
  
  most_recent_doc_filename <- doc_possible |>
    arrange(desc(doc_date)) |>
    slice_head(n = 12) |> 
    pull(filename)
  
  full_path <- path_from_docname(most_recent_doc_filename, folder = folder_to_search) 
  return(full_path)
}

####################
#LLM_all_inputs
###################


# get_last_doc : retourne le nom du fichier (ex: "EMC_2_2023") le plus récent disponible par rapport à la date où on se place
get_all_last_doc <- function(date_prev_df, target_date) {
  # target_date is Date
  candidats <- date_prev_df %>%
    filter(date_finale_d <= as.Date(target_date))
  
  if (nrow(candidats) == 0) {
    warning(paste("Aucun document disponible avant", target_date))
    return(NULL)
  }
  
  derniers <- candidats %>%
    arrange(desc(date_finale_d)) %>%
    pull(fichier)
  
  return(derniers)
}


# Obtenir depuis le dossier les 3 documents : SER, BAT, et EMI en les cherchant par date
get_all_last_insee_docs_by_type <- function(target_date, doc_type, folder_to_search) {
  
  target_date <- as.Date(target_date)
  
  # Format : AAAA_MM_TYPE.pdf
  pattern <- paste0("^(\\d{4})_(\\d{2})_", doc_type, "\\.pdf$")
  all_files <- list.files(folder_to_search, pattern = pattern, full.names = FALSE)
  
  if (length(all_files) == 0) {
    warning(paste("Aucun fichier", doc_type, "trouvé dans", folder_to_search))
    return(NULL)
  }
  
  # Extraction stricte : 4 chiffres + underscore + 2 chiffres
  file_dates_df <- tibble(
    filename = all_files,
    year  = as.integer(str_extract(all_files, "^\\d{4}")),
    month = as.integer(str_extract(all_files, "(?<=\\d{4}_)\\d{2}"))
  ) |>
    mutate(doc_date = ymd(paste(year, month, "01", sep = "-")))
  
  doc_possible <- file_dates_df |>
    filter(doc_date < target_date)
  
  if (nrow(doc_possible) == 0) {
    warning("Aucun document antérieur à la date cible")
    return(NULL)
  }
  
  most_recent_doc_filename <- doc_possible |>
    arrange(desc(doc_date)) |>
    pull(filename)
  
  full_path <- path_from_docname(most_recent_doc_filename, folder = folder_to_search) 
  return(full_path)
}

#########
#LLM_TS #A voir si on enlève
#########

#Prendre les données de la TS jusqu'à la date de prévision
get_last_ts <-function(target_date, df_ts){
  target_date <- as.Date(target_date)
  
  # Transformer Year + Quarter en date (dernier jour du trimestre)
  df_ts <- df_ts %>%
    mutate(
      date_finale_d = case_when(
        Trimestre == 1 ~ ymd(paste0(Année, "-04-30")),
        Trimestre == 2 ~ ymd(paste0(Année, "-07-30")),
        Trimestre == 3 ~ ymd(paste0(Année, "-10-30")),
        Trimestre == 4 ~ ymd(paste0(Année, "-01-30"))
      )
    )
  
  # Filtrer toutes les données ≤ target_date
  data_ts <- df_ts %>%
    filter(date_finale_d <= target_date)
  
  if (nrow(data_ts) == 0) {
    warning(paste("Aucune donnée disponible avant", target_date))
    return(NULL)
  }
  
  return(data_ts)
}

################################
# FONCTION LLM + ECONOMETRIE
#################################

#Fonction moyenne arithmétique
weight_avg <- function(pred_eco, pred_LLM) {
  nowcast <- 0.5*pred_eco + 0.5*pred_LLM
  return(list(
    nowcast = nowcast,
  ))
}

#Fonction inverse des erreurs moyennes quadratiques
inversed_weight <- function(pred_eco, pred_LLM, error_eco, error_LLM) {
    
    # Calcul des MSE pour chaque modèle
    MSE_eco <- mean(error_eco^2, na.rm = TRUE)
    MSE_LLM <- mean(error_LLM^2, na.rm = TRUE)
  
  
    # Calcul des poids inverses du MSE
    weight_raw <- c(1 / MSE_eco, 1 / MSE_LLM)
    weight <- weight_raw / sum(weight_raw)  # normalisation pour avoir w1 + w2 = 1
    
    # Résultat du nowcast
    nowcast <- weight[1] * pred_eco + weight[2] * pred_LLM
    
    # Retourne une liste claire
    return(list(
      nowcast = nowcast,
      weight_eco = w[1],
      weight_LLM = w[2]
    ))
  }
  
#Fonction type Method C of Improved methods of combining forecasts (Granger, Ramanathan)

# Inputs :

#  - y : vecteur numérique des valeurs du PIB
#  - forecasts : matrice ou data.frame n x k (prévisions historiques des k modèles)


# Output :

#      combined : vecteur de longueur n (prévisions combinées )
#      weights  : matrix n_preds x (k + intercept) des coefficients estimés à chaque pas
#      preds_idx: indices temporels correspondants aux prédictions (t+1 ici)


gr_rolling <- function(y, forecasts, rolling_window = 80) {
  # vérifications des inputs
  if (!is.numeric(y)) stop("y doit être un vecteur numérique.")
  if (!(is.matrix(forecasts) || is.data.frame(forecasts))) stop("forecasts doivent être une matrice ou un df.")
  forecasts <- as.data.frame(forecasts)
  n <- length(y)
  if (n != nrow(forecasts)) stop("y et forecasts doivent avoir le même nombre d'observations")
  if (rolling_window >= n) stop("rolling_window doit être strictement inférieur à la longueur de  y.")
  
  k <- ncol(forecasts)
  colnames(forecasts) <- ifelse(is.null(colnames(forecasts)),
                                paste0("f", seq_len(k)),
                                colnames(forecasts))
  
  # Initialisation sorties
  combined <- rep(NA_real_, n)
  # on stockera poids pour chaque prédiction, une colonne pour l'intercept
  coef_names <-  c("(Intercept)", colnames(forecasts)) 
  max_preds <- n - rolling_window
  weights_mat <- matrix(NA_real_, nrow = max_preds, ncol = length(coef_names),
                        dimnames = list(NULL, coef_names))
  pred_idx <- integer(max_preds)  # indices t+1 where we have a prediction
  row_id <- 1

  
  # boucle rolling
  for (i in rolling_window:(n-1)) {
    train_idx <- (i - rolling_window +1) : i
    test_idx  <- i + 1
    
    # construire df d'entraînement
    df_train <- data.frame(y = y[train_idx], forecasts[train_idx, , drop = FALSE])
    

    #regression
      formula <- as.formula(paste("y ~", paste(colnames(forecasts), collapse = " + ")))
    fit <- lm(formula, data = df_train)

    
    # préparation du newdata pour predict
    f_data <- as.data.frame(t(forecasts[test_idx, , drop = FALSE]))
    colnames(f_data) <- colnames(forecasts)
    
    # prédiction et stockage
    pred_val <- predict(fit, newdata = f_data)
    
    combined[test_idx] <- pred_val
    # récupérer coefficients des poids 
    coefs <- coef(fit)
    weights_mat[row_id, ] <- coefs
    
    #indice de la prévision
    pred_idx[row_id] <- test_idx
    
    row_id <- row_id + 1
  }
  
    #variables qu'on va retourner (row id - 1 car le dernier indice est length_df + 1)
    weights_mat <- weights_mat[1:(row_id - 1), , drop = FALSE]
    pred_idx <- preds_idx[1:(row_id - 1)]

  
  return(list(
    combined = combined,
    weights = weights_mat,
    pred_idx = preds_idx
  ))
}

  

