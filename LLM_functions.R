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
merge_pdfs <- function(files, output_path) {
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




#######################
#LLM_rolling_text
######################

get_docs_to_merge <- function(current_date, 
                              df_date, 
                              date_prev_BDF, 
                              document_folder_BDF = "./docEMC_clean/",
                              document_folder_INSEE = "./INSEE_Scrap/",
                              output_folder_BDF = "./BDF_files_used/",
                              output_folder_INSEE = "./INSEE_files_used/") {
  
  # --- sécurité et prérequis
  if (!dir.exists(output_folder_BDF)) dir.create(output_folder_BDF, recursive = TRUE)
  if (!dir.exists(output_folder_INSEE)) dir.create(output_folder_INSEE, recursive = TRUE)
  
  # identification du trimestre et du rang dans le trimestre
  m <- month(current_date)
  y <- year(current_date)
  
  if (m == 1) {
    # janvier -> dernier trimestre de l'année précédente
    months_in_quarter <- 3
    year_ref <- y - 1
    q_trim <- 4
  } else {
    q_trim <- ((m - 2) %/% 3) + 1
    year_ref <- y
    months_in_quarter <- ((m - 2) %% 3) + 1
  }
  
  message(sprintf("Date: %s -> T%d (%d) mois dans le trimestre", 
                  format(current_date, "%Y-%m-%d"), q_trim, months_in_quarter))
  
 #BOUCLE BDF
  BDF_docs_to_merge <- c()
  current_ref_date <- current_date
  
  # Boucle pour récupérer autant de documents EMC que nécessaire
  for (j in seq_len(months_in_quarter)) {
    next_doc_name <- get_next_doc(date_prev_BDF, current_ref_date)

      full_path <- path_from_docname(next_doc_name, folder = document_folder_BDF)
      BDF_docs_to_merge <- c(BDF_docs_to_merge, full_path)
      
      current_index <- which(df_date$`Date Prevision` == current_ref_date)
      current_ref_date <- df_date$`Date Prevision`[current_index - 1]
  }
  
  BDF_docs_to_merge <- rev(BDF_docs_to_merge) # du plus ancien au plus récent
  BDF_combined_path <- file.path(output_folder_BDF,
                              paste0("combined_BDF_", format(current_date, "%Y%m%d"), ".pdf"))
  merge_pdfs(BDF_docs_to_merge, BDF_combined_path)
  
  #BOUCLE INSEE
  INSEE_docs_to_merge <- c()
  current_ref_date <- current_date
  
  for (j in seq_len(months_in_quarter)) {
    # récupérer les 3 types pour la date courante
    emi_path <- get_last_insee_docs_by_type(current_ref_date, "EMI", document_folder_INSEE)
    ser_path <- get_last_insee_docs_by_type(current_ref_date, "SER", document_folder_INSEE)
    bat_path <- get_last_insee_docs_by_type(current_ref_date, "BAT", document_folder_INSEE)
    
    all_insee_docs_to_combine <- c(emi_path, ser_path, bat_path)
    all_insee_docs_to_combine <- all_insee_docs_to_combine[file.exists(all_insee_docs_to_combine)]
    
    if (length(all_insee_docs_to_combine) > 0) {
      INSEE_docs_to_merge <- c(INSEE_docs_to_merge, all_insee_docs_to_combine)
    }
    
    current_index <- which(df_date$`Date Prevision` == current_ref_date)
    current_ref_date <- df_date$`Date Prevision`[current_index - 1]
  }
  
  INSEE_docs_to_merge <- unique(rev(INSEE_docs_to_merge))

  INSEE_combined_path <- file.path(output_folder_INSEE,
                        paste0("combined_INSEE_", format(current_date, "%Y%m%d"), ".pdf"))
  merge_pdfs(INSEE_docs_to_merge, INSEE_combined_path)

  
  
  
  
  #RENVOI DES CHEMINS DES FICHIERS BDF ET INSEE
  return(list(
    BDF_path = BDF_combined_path,
    INSEE_path = INSEE_combined_path

  ))
}



################################
# FONCTION LLM + ECONOMETRIE
#################################

#Fonction moyenne arithmétique
weight_avg <- function(pred_eco, pred_LLM) {
  nowcast <- rowMeans(c(pred_eco, pred_LLM))
  return(list(
    nowcast = nowcast
  ))
}

#Fonction inverse des erreurs moyennes quadratiques

rolling_inversed_weight <- function(y, pred_eco, pred_LLM, rolling_window) {
  
  # vérifications des inputs
  if (!is.numeric(y)) stop("y doit être un vecteur numérique.")
  if (length(pred_eco) != length(pred_LLM) || length(y) != length(pred_eco)) {
    stop("y, pred_eco et pred_LLM doivent avoir la même longueur.")
  }
  n <- length(y)
  if (rolling_window >= n) stop("rolling_window doit être plus petite que la taille de l’échantillon.")
  
  # initialisation des vecteurs de sortie (on pose que n_modèle = 2)
  nowcast <- rep(NA_real_, n)
  weights_mat <- matrix(NA_real_, nrow = n - rolling_window + 1,
                        ncol = 2, dimnames = list(NULL, c("w_eco", "w_LLM")))
  row_id <- 1
  
  # boucle rolling
  for (i in rolling_window:n) {
    # période courante : la fenêtre d'apprentissage est basée sur les observations passées
    train_idx <- (i - rolling_window + 1):i
    
    # calcul des erreurs dans la fenêtre courante
    err_eco <- y[train_idx] - pred_eco[train_idx]
    err_LLM <- y[train_idx] - pred_LLM[train_idx]
    
    # MSE de chaque modèle dans la fenêtre
    MSE_eco <- mean(err_eco^2, na.rm = TRUE)
    MSE_LLM <- mean(err_LLM^2, na.rm = TRUE)
    
    # calcul des poids inverses du MSE
    w_raw <- c(1 / MSE_eco, 1 / MSE_LLM)
    w_norm <- w_raw / sum(w_raw)
    
    # combinaison pondérée des prévisions à la date i 
    nowcast[i] <- w_norm[1] * pred_eco[i] + w_norm[2] * pred_LLM[i]
    
    # stocker les poids correspondants
    weights_mat[row_id, ] <- w_norm
    row_id <- row_id + 1
  }
  
  # aligner dimensions finales
  weights_mat <- weights_mat[1:(row_id - 1), , drop = FALSE]
  
  #Garder uniquement les prévisions et enlever les NAs
  nowcast = na.omit(nowcast)
  attr(nowcast, "na.action") <- NULL
  
  return(list(
    nowcast = nowcast,
    weights = weights_mat
  ))
}



#Fonction type Method C of Improved methods of combining forecasts (Granger, Ramanathan)

# Inputs :

#  - y : vecteur numérique des valeurs du PIB
#  - forecasts : matrice ou data.frame n x k (prévisions historiques des k modèles)


# Output :

#      combined : vecteur de longueur n (prévisions combinées )
#      weights  : matrix n_preds x (k + intercept) des coefficients estimés à chaque pas


gr_rolling <- function(y, forecasts, rolling_window = 80) {
  # vérifications des inputs
  if (!is.numeric(y)) stop("y doit être un vecteur numérique.")
  if (!(is.matrix(forecasts) || is.data.frame(forecasts))) stop("forecasts doivent être une matrice ou un df.")
  forecasts <- as.data.frame(forecasts)
  n <- length(y)
  if (n != nrow(forecasts)) stop("y et forecasts doivent avoir le même nombre d'observations")
  if (rolling_window >= n) stop("rolling_window doit être strictement inférieur à la longueur de  y.")
  

  
  # Initialisation sorties
  combined <- rep(NA_real_, n)
  # on stockera poids pour chaque prédiction, une colonne pour l'intercept
  coef_names <-  c("(Intercept)", colnames(forecasts)) 
  max_preds <- n - rolling_window
  weights_mat <- matrix(NA_real_, nrow = max_preds, ncol = length(coef_names),
                        dimnames = list(NULL, coef_names))
  row_id <- 1

  # boucle rolling
  for (i in rolling_window:(n-1)) {
    train_idx <- (i - rolling_window +1) : i
    test_idx  <- i + 1
    
    # construire df d'entraînement et de prévision

    df_train <- data.frame(var_y = y[train_idx], var1 = f1[train_idx], var2 =  f2[train_idx])
    df_prev <- data.frame(var_y = y[test_idx], var1 = f1[test_idx], var2 = f2[test_idx])
    
    #regression
    fit <- lm(var_y ~ var1 + var2, data = df_train)
  


    # prédiction et stockage
    pred_val <- predict(fit, newdata = df_prev)
    
    combined[test_idx] <- pred_val
    # récupérer coefficients des poids 
    coefs <- coef(fit)
    weights_mat[row_id, ] <- coefs
    
    
    row_id <- row_id + 1
  }
  
    #matrice des poids
    weights_mat <- weights_mat[1:(row_id - 1), ]

  forecast_comb = na.omit(combined)
  attr(forecast_comb, "na.action") <- NULL
  
  return(list(
    forecast_comb = forecast_comb,
    weights = weights_mat
  ))
}

