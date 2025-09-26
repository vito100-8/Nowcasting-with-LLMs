#### Script : Requêtes LLM (Gemini) avec PDF en pièce-jointe  ####

rm(list = ls())

# libs
library(pdftools)
library(here)
library(dotenv)
library(stringr)
library(dplyr)
library(openxlsx)
library(rstudioapi)
library(lubridate)
library(readxl)
library(future.apply)
plan(multisession, workers = 4)

library(jsonlite)
library(mime)

library(ellmer)

# Repertoire/ env
setwd(dirname(getActiveDocumentContext()$path))
here::i_am("LLM_Text.R")
load_dot_env('env')

###################################
# Paramètres initiaux
###################################

#Paramètres généraux
english <- 1
document_folder <- "docEMC_clean"

#Paramètres fichiers
## liste fichiers 
files <- list.files(here(document_folder), pattern = "\\.pdf$", full.names = FALSE)
write.csv(data.frame(file = files), here("List_files_short.csv"), row.names = FALSE)

## charger table de dates
date_prev_temp <- read_excel("Synthese_fileEMC.xlsx")
colnames(date_prev_temp)[1:4] <- c("fichier", "date_courte", "date_longue", "trimestre")

date_prev_temp <- date_prev_temp %>%
  mutate(annee_prev = as.numeric(str_extract(fichier, "\\d{4}$")),
         mois_prev = as.numeric(str_extract(fichier, "(?<=EMC_)\\d{1,2}(?=_)"))) %>%
  filter(annee_prev >= 2015)

date_prev_temp <- date_prev_temp %>%
  mutate(
    annee_prev = as.numeric(str_extract(fichier, "\\d{4}$")),
    mois_prev  = as.numeric(str_extract(fichier, "(?<=EMC_)\\d{1,2}(?=_)")),
    date_courte_d = as.Date(as.character(date_courte)),
    date_longue_d = as.Date(as.numeric((date_longue)), origin = "1899-12-30")
  ) %>%
  mutate(
    date_finale_d = case_when(
      annee_prev >= 2015 & annee_prev <= 2019 ~ date_courte_d,
      annee_prev >= 2020 & annee_prev <= 2024 ~ date_longue_d
    )
  )

date_prev <- date_prev_temp %>%
  select(fichier, trimestre, date_finale_d) %>%
  filter(!is.na(date_finale_d))

print(date_prev)

#Paramètres LLM

temp_LLM <- 0.7
n_repro <- 2

# API Key (pour ellmer on utilise API_KEY_GEMINI)
cle_API <- Sys.getenv("API_KEY_GEMINI")
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")


###################################
# Fonctions utilitaires
###################################

# get_last_doc : retourne le nom du fichier (ex: "EMC_2_2023") le plus récent disponible par rapport à la date où on se place
get_last_doc <- function(date_prev_df, target_date) {
  # target_date is Date
  candidats <- date_prev_df %>%
    filter(date_finale_d <= as.Date(target_date))
  if (nrow(candidats) == 0) {
    warning(paste("Aucun document disponible avant", target_date))
    return(NULL)
  }
  dernier <- candidats %>%
    arrange(desc(date_finale_d)) %>%
    slice(1) %>%
    pull(fichier)
  return(dernier)
}

# path_from_docname : renvoie chemin complet vers le PDF local 
path_from_docname <- function(doc_name, folder = document_folder) {
  if (is.null(doc_name)) return(NULL)
  if (!grepl("\\.pdf$", doc_name, ignore.case = TRUE)) doc_name <- paste0(doc_name, ".pdf")
  path <- file.path(folder, doc_name)
  if (!file.exists(path)) {
    warning("Fichier introuvable : ", path)
    return(NULL)
  }
  return(normalizePath(path, winslash = "/", mustWork = TRUE))
}

###################################
# Prompts
###################################

if (english == 1) {
  try(Sys.setlocale("LC_TIME", "English"), silent = TRUE)
  #try(Sys.setlocale("LC_TIME", "en_US.UTF-8"), silent = TRUE)
  prompt_template_BDF <- function(d, q_trim, y_prev) {
    paste0(
      "Forget previous instructions and previous answers. You are François Villeroy de Galhau (Governor of the Banque de France), giving a speech on France economic outlook. Today is ",
      format(d, "%d %B %Y"), ". ",
      "You will be provided with the latest Banque de France Monthly business survey (EMC). Using ONLY the information in that document and information available on or before ", 
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
      "Oubliez les instructions et réponses précédentes. Vous êtes François Villeroy de Galhau, Gouverneur de la Banque de France, prononçant un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "Vous recevrez la dernière enquête mensuelle de conjoncture de la Banque de France (EMC). En utilisant UNIQUEMENT les informations contenues dans ce document et celles disponibles au plus tard le ", 
      format(d, "%d %B %Y"), ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) pour la croissance du PIB réel français pour le trimestre ", q_trim, " ", y_prev,
      " ainsi qu'un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) : ",
      "<prévision> (<confiance>). ",
      "Exemple : +0.3 (80). ",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}

###################################
# Boucle principale 
###################################

# dates (exemple)
dates <- as.Date(c("2023-03-15", "2023-06-15"))

# Forecast regex pattern qui sera appelé dans la boucle pour parse
forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"


#Initialisation LLM
chat_gemini <- chat_google_gemini( system_prompt = "You will act as the economic agent you are told to be. Answer based on your knowledge in less than 200 words, and researches, do not invent facts." ,
                                   base_url = "https://generativelanguage.googleapis.com/v1beta/", 
                                   api_key = cle_API, 
                                   model = "gemini-2.5-pro", 
                                   params(temperature = 0.7, max_tokens = 5000)
                                   )

# Creation de la list contenant les résultats
results_BDF <- list()
row_id <- 1 

t1 <- Sys.time()
for (dt in dates) {
  current_date <- as.Date(dt) 
  
  # Trouver le bon pdf et son path
  docname <- get_last_doc(date_prev, current_date)
  pdf_path <- path_from_docname(docname, folder = document_folder)
  
  if (is.null(pdf_path)) {
    warning("No PDF found for date ", current_date, " — skipping.")
    next
  }
  
  # Chargement du pdf souhaité
  uploaded_doc <- google_upload(
    pdf_path,
    base_url = "https://generativelanguage.googleapis.com/",
    api_key = cle_API
  )
  # Initialisation des dates
  current_date <- as.Date(dt)
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
  df_bdf <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_bdf[[paste0("forecast_", i)]]  <- parsed_list[[i]]$forecast
    df_bdf[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_bdf[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
  }
  
  results_BDF[[row_id]] <- df_bdf
  row_id <- row_id + 1
  Sys.sleep(0.5)
}

# réunir les prévisions pour chaque date
df_results_text <- do.call(rbind, results_BDF)

# Enregistrement
write.xlsx(df_results_BDF, file = "resultats_BDF_Gemini_text.xlsx", sheetName = 'prevision', rowNames = FALSE)
print("Enregistré: resultats_BDF_Gemini_text.xlsx \n")

t2 <- Sys.time()
print(diff(range(t1, t2)))
