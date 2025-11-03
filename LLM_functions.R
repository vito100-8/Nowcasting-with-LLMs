## FONCTION UTILITAIRES ##

rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")


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

# path_from_docname : renvoie chemin complet vers le ou les PDF local/locaux
path_from_docname <- function(doc_name, folder) {
  if (is.null(doc_name)) return(NULL)
  
  # Si vecteur de plusieurs noms de docs et non pas un seul
  full_paths <- sapply(doc_name, function(single_doc_name) {
    if (!grepl("\\.pdf$", single_doc_name, ignore.case = TRUE)) {
      single_doc_name <- paste0(single_doc_name, ".pdf")
    }
    
    path <- file.path(folder, single_doc_name)
    
    if (!file.exists(path)) {
      warning("Fichier introuvable : ", path)
      return(NA_character_) # Retourne NA pour les fichiers introuvables
    }
    
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  })
  
  # Enlever les fichiers qui n'ont éventuellement pas été trouvés
  full_paths <- full_paths[!is.na(full_paths)]
  
  if (length(full_paths) == 0) {
    return(NULL) # Si aucun fichier trouvable
  }
  
  return(full_paths)
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
get_next_doc <- function(cur_day) {
  # target_date is Date of the corresponding EMC
  target_date <- cur_day + 1L
  next_doc <- date_publi_prev |>
    filter(date_finale_d == as.Date(target_date))
  return(next_doc$fichier)
}


# Obtenir depuis le dossier les 3 documents : SER, BAT, et EMI en les cherchant par date
get_last_insee_docs_by_type <- function(target_date, doc_type, folder_to_search) {
  
  target_date <- as.Date(target_date)
  
  #Chercher l'enquête du mois précédent (rollback ramène au dernier jour du mois d'avant)
  desired_date <- rollback(target_date)
  
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
  
  #On cherche l'enquête du mois d'avant qui, par transformation, sera toujours au 1er du mois 
  doc_possible <- file_dates_df |>
    filter(doc_date <= desired_date)
  
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
get_last_12_doc <- function(target_date) {

  # target_date is Date
  candidats <- date_publi_prev |>
    filter(date_finale_d <= as.Date(target_date) + 1L)
  
  if (nrow(candidats) == 0) {
    warning(paste("Aucun document disponible avant", target_date))
    return(NULL)
  }
  
  derniers <- candidats |>
    arrange(desc(date_finale_d)) |>
    slice_head(n = 12) |>   #prendre les 12 dernières enquêtes BDF
    pull(fichier)
  
  return(derniers)
}


# Obtenir depuis le dossier les 3 documents : SER, BAT, et EMI en les cherchant par date
get_last_12_insee_docs_by_type <- function(target_date, doc_type, folder_to_search) {
  
  target_date <- as.Date(target_date)
  
  
  #Chercher l'enquête du mois précédent (rollback ramène au dernier jour du mois d'avant)
  desired_date <- rollback(target_date)
  
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
    filter(doc_date < desired_date)
  
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

#######################
#LLM_all_inputs
######################
# Fonction pour transformer un df en un markdown (lisible par ellmer)
df_to_markdown_table <- function(df, title = NULL, max_rows = 50) {
  if (is.null(df) || nrow(df) == 0) {
    if (!is.null(title)) {
      return(paste0("No data available for '", title, "' for this period."))
    } else {
      return("No data available for this period.")
    }
  }
  
  # Vérifier que les dates sont dans un format lisible
  for (col in names(df)) {
    if (inherits(df[[col]], "Date")) {
      df[[col]] <- format(df[[col]], "%Y-%m-%d")
    }
    # Convert any list-columns to character for table rendering
    if (is.list(df[[col]])) {
      df[[col]] <- sapply(df[[col]], function(x) paste(x, collapse = ";"))
    }
  }
  
  # Convertir les colonnes en charactères 
  df <- df |> mutate(across(everything(), as.character))
  
  # On choisit aussi un nombre max de lignes à afficher pour avoir une limite à la taille du prompt
  if (nrow(df) > max_rows) {
    df <- tail(df, max_rows)
    # Add a note about truncation to the title
    if (!is.null(title)) {
      title <- paste0(title, " (showing last ", max_rows, " rows)")
    } else {
      title <- paste0("Data (showing last ", max_rows, " rows)")
    }
    warning(paste0("Dataframe truncated to last ", max_rows, " rows for markdown conversion."))
  }
  
  # Header
  header <- paste(names(df), collapse = " | ")
  separator <- paste(rep("---", ncol(df)), collapse = " | ")
  
  # Lignes
  rows <- apply(df, 1, function(row) paste(row, collapse = " | "))
  
  # Combiner le tout
  table_string <- ""
  if (!is.null(title)) {
    table_string <- paste0("### ", title, "\n\n")
  }
  table_string <- paste0(table_string, header, "\n", separator, "\n", paste(rows, collapse = "\n"))
  return(table_string)
}



#######################
#LLM_rolling_text
######################

get_docs_to_merge <- function(date_to_use, 
                              df_date, 
                              document_folder_BDF,
                              document_folder_INSEE ,
                              output_folder_BDF,
                              output_folder_INSEE) {
  
  #sécurité 
  if (!dir.exists(output_folder_BDF)) dir.create(output_folder_BDF, recursive = TRUE)
  if (!dir.exists(output_folder_INSEE)) dir.create(output_folder_INSEE, recursive = TRUE)
  
  # identification du trimestre et du rang dans le trimestre
  m <- month(date_to_use)
  y <- year(date_to_use)
  
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
 
  # construire la séquence des mois du trimestre (ex : q_trim = 1 alor de  Jan-Mar)
  quarter_first_month <- (q_trim - 1) * 3 + 1
  quarter_months <- quarter_first_month:(quarter_first_month + 2)
  # on sélectionne les mois voulus selon la date (ordre ancien->récent)
  months_to_fetch <- quarter_months[1:months_in_quarter]

 #Partie BDF
  
  BDF_docs_to_merge <- c()
  current_ref_date <- date_to_use

    #On va prendre tous les EMC publié à la date ou avant et n'en garder que le nombre souhaité (sleon position du mois dans trimestre)
    candidats <- date_publi_prev |>
      filter(date_finale_d <= as.Date(current_ref_date) + 1L)
    next_doc_name <- candidats |>
      arrange(desc(date_finale_d))
    
  docs_selected <- head(candidats$fichier, months_in_quarter)
    
   # chemins PDF complets
  BDF_docs_to_merge <- file.path(document_folder_BDF, selected)
  
  merge_pdfs(BDF_docs_to_merge, BDF_combined_path)
  
  #BOUCLE INSEE
  INSEE_docs_to_merge <- c()
  months_desc <- rev(months_to_fetch) 
  
  for (mm in months_desc) {
    
    target_year <- year_ref
    target_month <- mm 
   
    # Construire noms attendus AAAA_MM_TYPE.pdf
    emi_file <- file.path(document_folder_INSEE, sprintf("%04d_%02d_EMI.pdf", target_year, target_month))
    ser_file <- file.path(document_folder_INSEE, sprintf("%04d_%02d_SER.pdf", target_year, target_month))
    bat_file <- file.path(document_folder_INSEE, sprintf("%04d_%02d_BAT.pdf", target_year, target_month))
    # ordre demandé : EMI, SER, BAT (si existent) ; on ajoute seulement s'ils existent
    if (file.exists(emi_file)) INSEE_docs_to_merge <- c(INSEE_docs_to_merge, emi_file)
    if (file.exists(ser_file)) INSEE_docs_to_merge <- c(INSEE_docs_to_merge, ser_file)
    if (file.exists(bat_file)) INSEE_docs_to_merge <- c(INSEE_docs_to_merge, bat_file)
  }
    
  INSEE_combined_path <- file.path(output_folder_INSEE,
                        paste0("combined_INSEE_", format(current_date, "%Y%m%d"), ".pdf"))
  merge_pdfs(INSEE_docs_to_merge, INSEE_combined_path)

  
  #RENVOI DES CHEMINS DES FICHIERS BDF ET INSEE
  return(list(
    BDF_path = BDF_combined_path,
    INSEE_path = INSEE_combined_path

  ))
}


##############################
#LLM_excel_with_error
##############################

# Fonction de la même structure que un df (df_temp) avec un historique des erreurs pour pouvoir ensuite les rajouter au df initial
make_hist_rows <- function(template_df, dates_vec, llm_prev_vec, prev_col = "LLM_prev_error") {
  n <- length(dates_vec)
  tmpl <- template_df[1, , drop = FALSE]
  hist_rows <- tmpl[rep(1, n), , drop = FALSE]
  for (col in names(hist_rows)) {
    cls <- class(tmpl[[col]])[1]
    hist_rows[[col]] <- switch(cls,
                               "Date" = as.Date(rep(NA, n)),
                               "POSIXct" = as.POSIXct(rep(NA, n)),
                               "POSIXt" = as.POSIXct(rep(NA, n)),
                               "numeric" = rep(NA_real_, n),
                               "double" = rep(NA_real_, n),
                               "integer" = rep(NA_integer_, n),
                               rep(NA_character_, n)
    )
  }
  hist_rows$dates <- as.Date(dates_vec)
  hist_rows[[prev_col]] <- as.character(llm_prev_vec)
  hist_rows
}

# Matcher la prévision au bon PIB_PR (trouver le PIB qui match le quarter)
find_pib_index <- function(cur_date) {
  cur_date <- as.Date(cur_date)
  if ("dates" %in% names(df_PIB)) vec <- as.Date(df_PIB$dates) 
  else if ("Date" %in% names(df_PIB)) vec <- as.Date(df_PIB$Date)
  else vec <- as.Date(df_PIB[[1]])
  # match date exact
  exact <- which(vec == cur_date)
  if (length(exact) == 1) return(exact)
  # match par trimestre
  q_target <- quarter(cur_date)
  y_target <- year(cur_date)
  qidx <- which(quarter(vec) == q_target & lubridate::year(vec) == y_target)
  if (length(qidx) >= 1) return(qidx[1])
  # match avec le dernier PIB en date si pas dans le trimestre
  prev_idx <- which(vec <= cur_date)
  if (length(prev_idx) >= 1) return(tail(prev_idx, 1))
  # si aucune correspondance alors renvoi le prochain PIB (laisser ça ou faire une erreur ?)
  future_idx <- which(vec > cur_date)
  if (length(future_idx) >= 1) return(future_idx[1])
  integer(0)
}
#####################
# STATS DES
#####################
#Passage en long pour stat des plus simple à rédiger

to_long <- function(df, source_name) { 
  df |>
    pivot_longer(
      cols = matches("^(forecast|confidence)_\\d+$"),
      names_to = c(".value", "rep"),
      names_pattern = "(.*)_(\\d+)$"
    ) |>
    mutate(
      rep = as.integer(rep),
      forecast = as.numeric(forecast),
      confidence = as.integer(confidence),
      source = source_name
    )
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

