#INSERTION D'UN EXCEL ITERATIF SELON LA PERIODE DE PREVISION AU LLM#


rm(list = ls()) 
source("LLM_functions.R")

# Repertoire/ env
setwd(dirname(getActiveDocumentContext()$path))

here::i_am("LLM_Text.R")

load_dot_env('.env')   

###################################
# Paramètres initiaux
###################################

#Paramètres généraux
english <- 1
temp_LLM <- 0.7
n_repro <- 2
sys_prompt <- ifelse(english == 1,
                     "You will act as the economic agent you are told to be. Answer based on your knowledge and the document provided in less than 200 words, do not invent facts." ,
                     "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aide de vos connaissances et du document fourni, n'inventez pas de faits.")

# Vecteur de dates
dates_used <- as.Date(df_date[[1]])


# API Key (pour ellmer on utilise API_KEY_GEMINI)
cle_API <- Sys.getenv("API_KEY_GEMINI")

#Initialisation LLM
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")
chat_gemini <- chat_google_gemini( system_prompt = sys_prompt,
                                   base_url = "https://generativelanguage.googleapis.com/v1beta/", 
                                   api_key = cle_API, 
                                   model = "gemini-2.5-pro", 
                                   params(temperature = temp_LLM, max_tokens = 5000)
)


############################
#Données et nettoyage
############################

#Téléchargement données

df_PIB <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")
df_ISMA <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "mensuel ISMA")
df_enq_BDF <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_BDF")
df_enq_INSEE <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_INSEE")

#Nettoyage de df_enq_BDF

df_enq_BDF <- df_enq_BDF |> 
  slice(6:433) |> 
  mutate(dates = `Titre :...1`, , .keep = "unused") |>
  select(dates, everything())


#Nettoyage de df_enq_INSEE

df_enq_INSEE <- df_enq_INSEE |>
  rename(dates = `...1`) |>
  mutate(
    dates = str_replace_all(dates, 
                            c("janv\\." = "jan", "févr\\." = "feb", "mars" = "mar",
                              "avr\\." = "apr", "mai" = "may", "juin" = "jun",
                              "juil\\." = "jul", "août" = "aug", "sept\\." = "sep",
                              "oct\\." = "oct", "nov\\." = "nov", "déc\\." = "dec"))
  ) |>
  mutate(dates = as.Date(parse_date_time(dates, orders = "b Y"), origin = "1970-01-01"))

new_names <- paste0(names(df_enq_INSEE), " : ", df_enq_INSEE[1, ])
colnames(df_enq_INSEE) <- new_names


df_enq_INSEE <- df_enq_INSEE |>
  slice(2:430)|>
  mutate(
    dates = `dates : NA`, .keep = "unused") |>
  select(dates, everything())


####################################
# Prompts
###################################

if (english == 1) {
  try(Sys.setlocale("LC_TIME", "English"), silent = TRUE)
  #try(Sys.setlocale("LC_TIME", "en_US.UTF-8"), silent = TRUE)
  prompt_template_BDF <- function(d, q_trim, y_prev) {
    paste0(
      "Forget previous instructions and previous answers. You are ", BDF_current_boss(d), " (Governor of the Banque de France), giving a speech on France economic outlook. Today is ",
      format(d, "%d %B %Y"), ". ",
      "You will be provided with the latest Banque de France Monthly business survey (Industry, Services and Construction). Using ONLY the information in that document and information available on or before ", 
      format(d, "%d %B %Y"), ", provide a numeric forecast (decimal percent with sign, e.g. +0.3) for French real GDP growth for Q", q_trim, " ", y_prev, 
      " and a confidence level (integer 0-100). Output EXACTLY in this format on a single line (no extra text): ",
      "<forecast> (<confidence>). ",
      "Example: +0.3 (80). ",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), "."
    )
  }
  
  prompt_template_INSEE <- function(d, q_trim, y_prev) {
    paste0(
      "Forget previous instructions and previous answers. You are ", INSEE_current_boss(d), " director General at INSEE (National Institute of Statistics and Economic Studies), analyzing the French economy. Today is ",
      format(d, "%d %B %Y"), ". ",
      "You will be provided with the latest INSEE monthly business tendency surveys (Industry, Services and Construction) for France. Using ONLY the information in these documents and information available on or before ", 
      format(d, "%d %B %Y"), ", provide a numeric forecast (decimal percent with sign, e.g. +0.3) for French real GDP growth for Q", q_trim, " ", y_prev,
      " and a confidence level (integer 0-100). Output EXACTLY in this format on a single line (no extra text): ",
      "<forecast> (<confidence>). ",
      "Example: +0.3 (80). ",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), "."
    )
  }
  
  
} else {
  try(Sys.setlocale("LC_TIME", "French"), silent = TRUE)
  #try(Sys.setlocale("LC_TIME", "fr_FR.UTF-8"), silent = TRUE)
  prompt_template_BDF <- function(d, q_trim, y_prev) {
    paste0(
      "Oubliez les instructions et réponses précédentes. Vous êtes ", BDF_current_boss(d),  ", Gouverneur de la Banque de France, prononçant un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "Vous recevrez la dernière enquête mensuelle de conjoncture de la Banque de France (Industrie, Services et Batiment). En utilisant UNIQUEMENT les informations contenues dans ce document et celles disponibles au plus tard le ", 
      format(d, "%d %B %Y"), ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) pour la croissance du PIB réel français pour le trimestre ", q_trim, " ", y_prev,
      " ainsi qu'un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) : ",
      "<prévision> (<confiance>). ",
      "Exemple : +0.3 (80). ",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
  
  prompt_template_INSEE <- function(d, q_trim, y_prev) {
    paste0(
      "Oubliez les instructions et réponses précédentes. Vous êtes ", INSEE_current_boss(d),  ", Directeur Général de l'INSEE, analysant l'économie française. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "Vous recevrez les dernières enquêtes de conjoncture mensuelles de l'INSEE (EMI, SER, BAT) pour la France. En utilisant UNIQUEMENT les informations contenues dans ces documents et celles disponibles au plus tard le ", 
      format(d, "%d %B %Y"), ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) pour la croissance du PIB réel français pour le trimestre ", q_trim, " ", y_prev,
      " ainsi qu'un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) : ",
      "<prévision> (<confiance>). ",
      "Exemple : +0.3 (80). ",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}


###########################################
# BOUCLES ITERATIVES SELON LA DATE
#############################################



# BDF #
dir.create("Indicateurs_BDF", showWarnings = FALSE)

results_uploads_BDF <- list()
row_id_BDF <- 1

for (d in dates_used) {
  
  # Tronquage
  df_temp <- df_enq_BDF |> 
    filter(dates <= d)
  
  # Nom du fichier Excel
  file_name <- paste0("Indicateurs_BDF/date_max_", format(d, "%Y%m%d"), ".xlsx")
  
  # Sauvegarde locale
  write.xlsx(df_temp, file = file_name, overwrite = TRUE)
  cat("Fichier créé :", file_name, "\n")
  
  #Uploader le doc
  upload_file <- path_from_docname(file_name, folder = "Indicateurs_BDF")
  
  uploaded_doc <- google_upload(
    upload_file,
    base_url = "https://generativelanguage.googleapis.com/",
    api_key = cle_API
  )
  # Initialisation des dates
  current_date <- as.Date(d)
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
  prompt_text <- prompt_template_BDF(current_date, trimestre_index ,
                                       year_prev)
  
  # appel à Gemini en intégrant le document voulu
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- chat_gemini$chat(uploaded_doc, prompt_text)
      return(resp)}, error = function(e) {
        message("API error: ", conditionMessage(e))
        return(NA_character_)
      })
    
  }, future.seed = TRUE)
  
  # Parse les résultats
  histoires <- sapply(out_list, function(x) {if (is.list(x) && !is.null(x$text)) {
    return(x$text)
  } else if (is.character(x)) {
    return(x)
  } else {
    return(NA_character_)
  }})
  parsed_list <- lapply(histoires, function(txt) {
    if (is.null(txt) || length(txt) == 0) return(list(forecast = NA_real_, confidence = NA_integer_, raw = NA_character_))
    m <- regmatches(txt, regexec(forecast_confidence_pattern, txt))
    if (length(m[[1]]) >= 3) {
      list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt)
    } else {
      list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
    }
  })
  
  #Df des résultats
  df_excel_BDF <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_excel_BDF[[paste0("forecast_", i)]]  <- parsed_list[[i]]$forecast
    df_excel_BDF[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_excel_BDF[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
  }
  
  results_uploads_BDF[[row_id_INSEE]] <- df_excel_BDF
  row_id_BDF <- row_id_BDF + 1
  Sys.sleep(0.5)
}

# Stockage + document résultat
df_excel_BDF <- do.call(rbind, results_uploads_BDF)
write.xlsx(df_excel_BDF, file = "Indicateurs_BDF/upload_summary.xlsx", overwrite = TRUE)


# INSEE #

dir.create("Indicateurs_INSEE", showWarnings = FALSE)

results_uploads_INSEE <- list()
row_id_INSEE <- 1

for (d in dates_used) {
  
  # Tronquage
  df_temp <- df_enq_INSEE |> 
    filter(dates <= d)
  
  # Nom du fichier Excel
  file_name <- paste0("Indicateurs_INSEE/date_max_", format(d, "%Y%m%d"), ".xlsx")
  
  # Sauvegarde locale
  write.xlsx(df_temp, file = file_name, overwrite = TRUE)
  cat("Fichier créé :", file_name, "\n")
  
  #Uploader le doc
  upload_file <- path_from_docname(file_name, folder = "Indicateurs_INSEE")
  
  uploaded_doc <- google_upload(
    upload_file,
    base_url = "https://generativelanguage.googleapis.com/",
    api_key = cle_API
  )
    # Initialisation des dates
    current_date <- as.Date(d)
    mois_index <- as.integer(format(current_date, "%m"))
    year_current <- as.integer(format(current_date, "%Y"))
    trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
    year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
    prompt_text <- prompt_template_INSEE(current_date, trimestre_index ,
                                         year_prev)
    
    # appel à Gemini en intégrant le document voulu
    out_list <- future_lapply(seq_len(n_repro), function(i) {
      tryCatch({
        resp <- chat_gemini$chat(uploaded_doc, prompt_text)
        return(resp)}, error = function(e) {
          message("API error: ", conditionMessage(e))
          return(NA_character_)
        })
      
    }, future.seed = TRUE)
    
    # Parse les résultats
    histoires <- sapply(out_list, function(x) {if (is.list(x) && !is.null(x$text)) {
      return(x$text)
    } else if (is.character(x)) {
      return(x)
    } else {
      return(NA_character_)
    }})
    parsed_list <- lapply(histoires, function(txt) {
      if (is.null(txt) || length(txt) == 0) return(list(forecast = NA_real_, confidence = NA_integer_, raw = NA_character_))
      m <- regmatches(txt, regexec(forecast_confidence_pattern, txt))
      if (length(m[[1]]) >= 3) {
        list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt)
      } else {
        list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
      }
    })
    
    #Df des résultats
    df_excel <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
    for (i in seq_len(n_repro)) {
      df_excel_INSEE[[paste0("forecast_", i)]]  <- parsed_list[[i]]$forecast
      df_excel_INSEE[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
      df_excel_INSEE[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
    }
    
    results_uploads_INSEE[[row_id_INSEE]] <- df_excel_INSEE
    row_id_INSEE <- row_id_INSEE + 1
    Sys.sleep(0.5)
}

# Stockage + document résultat
df_excel_INSEE <- do.call(rbind, results_uploads_INSEE)
write.xlsx(df_excel_INSEE, file = "Indicateurs_INSEE/upload_summary.xlsx", overwrite = TRUE)












#############################################################

#Que faire des données ISMA/PIB ? Les ajouter aux enquêtes ? 


##### Mettre les anciennes prévisions des enquêtes (EMC) , réflechir comment faire pour les prev de l'INSEE
#### VS erreur du LLM


#EI = M - 1 (si prévision en janvier on prend jusqu'en décembre)