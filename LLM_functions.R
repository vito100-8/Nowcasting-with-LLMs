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
  
#Fonction méthode Bates-Granger
BG_weight <- function(pred_eco, pred_LLM, error_eco, error_LLM) {
  
    # Variances et covariance des erreurs
    var_eco <- var(error_eco, na.rm = TRUE)
    var_LLM <- var(error_LLM, na.rm = TRUE)
    covar <- cov(error_eco, error_LLM, use = "complete.obs")
    
    # Calcul des poids Bates & Granger (type BG_comb)
    weight_eco <- (var_LLM - covar) / (var_eco + var_LLM - 2 * covar)
    weight_LLM <- 1 - w_eco
    
    # Nowcast 
    nowcast <- weight_eco * pred_eco + weight_LLM * pred_LLM
    
    return(list(
      nowcast = nowcast,
      weights = c(Econométrie = weight_eco, LLM = weight_LLM),
      covariance = covar
    ))
  }
  
  

