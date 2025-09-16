# ReadMe
# Prévision récursive de croissance du PIB avec LLM en screenant des documents
# Fichiers source = pdf


rm(list=ls())
library(pdftools)
library(httr)
library(jsonlite)
library(here)
library(dotenv)
library(stringr)
library(dplyr)
library(openxlsx)
library (rstudioapi)
library(lubridate)
library(tidyr)
library(dplyr)
library(moments) 
library(ggplot2)
library(future.apply)  # pour paralléliser les appels
plan(multisession, workers = 4)  # 4 optimal (jusqu'à 8 sur mon pc)

# Répertoire de travail actif

setwd(dirname(getActiveDocumentContext()$path))
#setwd("C:/Users/victo/OneDrive/Documents/Stage_recherche_dauphine/code") #A MODIFIER ENSUITE
#setwd("C:/Users/marie/OneDrive - Université Paris-Dauphine/LLM_VC/Code") #A MODIFIER ENSUITE

here::i_am("LLM_Text.R")

#load_dot_env('code/.env')
#dotenv::load_dot_env(file = here("env"))
load_dot_env('env')

###################################################
#INITIALISATION PARAMETRES
###################################################


#langue utilisé
english <- 1  # 1 si prompt en anglais

# Définition des modèles et URLs
LLM_configs <- list(
  #  CHAT = list(model = "gpt-4o-2024-11-20", url = "https://api.openai.com/v1/chat/completions"),
  CHAT = list(model = "gpt-4-turbo", url = "https://api.openai.com/v1/chat/completions"),  
  #  GEMINI = list(model = "gemini-1.5-pro-latest" , url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-pro-latest:generateContent"),
  GEMINI = list(model = "gemini-2.0-flash", url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"),
  #  CLAUDE = list(model = "claude-3-7-sonnet-20250219", url = "https://api.anthropic.com/v1/messages")
  CLAUDE = list(model = "claude-3-5-haiku-20241022",url = "https://api.anthropic.com/v1/messages") 
)

# Dossier contenant les documents qui seront analysés
document_folder <- "docEMC_PDF"


    ##INUTILE MAIS PEUT ETRE CONSERVé
# Liste les fichiers PDF dans data/
files <- list.files(here("docEMC_PDF"), pattern = "\\.pdf$", full.names = FALSE)

# Sauvegarde dans un CSV
write.csv(data.frame(file = files),
          here("List_files_short.csv"),
          row.names = FALSE)
        #####

#CHARGER date des forecast depuis l'excel

date_prev_temp <- read_excel("Synthese_fileEMC.xlsx")

#On choisis de prendre les dates de sortie des prévisions courtes jusqu'en 2020 puis celles des longues ensuite
colnames(date_prev_temp)[1:4] <- c("fichier", "date_courte", "date_longue", "trimestre")

# Extraire année depuis le nom du fichier
date_prev_temp <- date_prev_temp %>%
  mutate(annee_prev = as.numeric(str_extract(fichier, "\\d{4}$")),
         mois_prev   = as.numeric(str_extract(fichier, "(?<=EMC_)\\d{1,2}(?=_)"))) |>
  filter(annee_prev >= 2015)

# Choisir la bonne date selon notre choix (à changer si nécessaire)
date_prev_temp <- date_prev_temp %>%
  mutate(
    annee_prev = as.numeric(str_extract(fichier, "\\d{4}$")),
    mois_prev  = as.numeric(str_extract(fichier, "(?<=EMC_)\\d{1,2}(?=_)")),
    # Forcer les dates en character pour case_when
    date_courte_d = as.Date(as.character(date_courte)),
    date_longue_d = as.Date(as.numeric((date_longue)), origin = "1899-12-30"))
    

date_prev_temp <- date_prev_temp %>%
  mutate(
    date_finale_d = case_when(
      annee_prev >= 2015 & annee_prev <= 2019 ~ date_courte_d,
      annee_prev >= 2020 & annee_prev <= 2024 ~ date_longue_d
    )
  )
# Nettoyage
date_prev <- date_prev_temp %>%
  select(fichier, trimestre, date_finale_d) %>%
  filter(!is.na(date_finale_d))

print(date_prev)


#Choix du modèle et paramètres


LLM <- "GEMINI"  # Modifier ici pour changer de modèle
model_LLM <- LLM_configs[[LLM]]$model
url_LLM <- LLM_configs[[LLM]]$url

temp_LLM <- 0.7  # Niveau de créativité des réponses 0.3/0.7/1.5 (castro-Leibovici)
n_repro <- 5  # Nombre de prévisions générées par date

# Charger la clé API

cle_API <- Sys.getenv(paste0("API_KEY_", LLM))
if (cle_API == "") stop("Clé API manquante pour ", LLM)

#############################
#FONCTIONS REQUETE DU CHATBOT
############################

# Chat-GPT / Mistral / Deepseek
poser_question_CHAT <- function(date_prevision, target_dates, question,api_key = cle_API) {
  url <- url_LLM
  doc_input <- get_last_doc(target_dates, date_prevision)
  headers <- c(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )
  
  
  if (english == 0) {
    role_system <- "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aides de tes connaissances et des documents fournis, n'invente pas de faits."
  } else {
    role_system <- "You will act as the economic agent you are told to be. Answer based on your knowledge and on provided documents in less than 200 words, and researches, do not invent facts."
  }
  
  user_content <- list(list(type = "text", text = question))
  
  if (!is.null(doc_input) && file.exists(doc_input)) {
    user_content <- append(user_content, list(
      list(type = "input_file", input_file = list(path = doc_input))
    ))
  } 
  
  
  body <- list(
    model = model_LLM,
    temperature = temp_LLM,  # pour des réponses précises et cohérentes (à tester)
    max_tokens = 200,   # nb max de mots de la réponse générée (ici on attend une réponse concise)
    messages = list(
      list(role = "system", content = role_system),
      list(role = "user", content = user_content)
    )
  )
  
  response <- POST(url, add_headers(headers), body = toJSON(body, auto_unbox = TRUE))
  
  if (status_code(response) == 200) {
    content(response, "parsed")$choices[[1]]$message$content
  } else {
    paste("Erreur :", status_code(response), content(response, "text", encoding = "UTF-8"))
  }
  
}

# GEMINI
poser_question_GEMINI <- function(date_prevision, target_dates, question,api_key = cle_API) {
  doc_input <- get_last_doc(target_dates, date_prevision)
  model_query <- paste0(model_LLM, ":generateContent")
  
  if (english == 0) {
    role_system <- "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aides de tes connaissances et des documents fournis, n'invente pas de faits."
    role_system <- "You will act as the economic agent you are told to be. Answer based on your knowledge and on the provided documents in less than 200 words, and researches, do not invent facts."
  }
  
  user_content <- list(list(type = "text", text = question))
  
  if (!is.null(doc_input) && file.exists(doc_input)) {
    user_content <- append(user_content, list(
      list(type = "input_file", input_file = list(path = doc_input))
    ))
  }
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        list(parts = list(
          list(text = role_system),
          user_content
        ))
      ),
      generationConfig = list(
        temperature = temp_LLM,
        maxOutputTokens = 200
      )
    )
  )
  
  if (response$status_code > 200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts[[1]]$text))
  
  return(outputs)
}

# CLAUDE
poser_question_CLAUDE <- function(date_prevision, target_dates, question,api_key = cle_API) {
  doc_input <- get_last_doc(target_dates, date_prevision)
  url <- url_LLM
  
  headers <- c(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",   # faut-il faire varier suivant modèle?
    "Content-Type" = "application/json"
  )
  
  if (english == 0) {
    role_system <- "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aides de tes connaissances et des documents fournis, n'invente pas de faits."
  } else {
    role_system <- "You will act as the economic agent you are told to be. Answer based on your knowledge and on the provided documents in less than 200 words, and researches, do not invent facts."
  }
  
  user_content <- list(list(type = "text", text = question))
  
  if (!is.null(doc_input) && file.exists(doc_input)) {
    user_content <- append(user_content, list(
      list(type = "input_file", input_file = list(path = doc_input))
    ))
  }
  
  body <- list(
    model = model_LLM,
    max_tokens = 200,
    temperature = temp_LLM,
    system = role_system,
    messages = list(
      list(role = "user", content = user_content)
    )
  )
  
  response <- httr::POST(url, httr::add_headers(.headers = headers),
                         body = toJSON(body, auto_unbox = TRUE))
  
  if (httr::status_code(response) == 200) {
    return(content(response, "parsed")$content[[1]]$text)
  } else {
    return(paste("Erreur :", httr::status_code(response),
                 httr::content(response, "text", encoding = "UTF-8")))
  }
}



f_question <- get(paste0("poser_question_", LLM))


# Initialisation des dates


dates <- as.Date(c("2023-03-15", "2023-06-15")) #à changer manuellement



#####################
# QUESTIONS A POSER
#####################"



if (english == 1) {
  prompt_template_BDF <- function(d, q_trim, y_prev) {
    paste0(
      "Forget previous instructions and previous answers. You are François Villeroy de Galhau (Governor of the Banque de France), and you are giving a speech on France economic outlook. Today is ",
      format(d, "%d %B %Y"), ". ",
      "Using only information that was available on or before ", format(d, "%d %B %Y", locale = "en_US.UTF-8"), 
      ", provide a numeric forecast (decimal percent with sign, e.g. +0.3) for French real GDP growth for Q", q_trim, " ", y_prev, 
      " and a confidence level (integer 0-100). Output EXACTLY in this format on a single line (no extra text):\n",
      "<forecast> (<confidence>)\n",
      "Example: +0.3 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y", locale = "en_US.UTF-8"), "."
    )
  }
  prompt_template_INSEE <- function(d, q_trim, y_prev) {
    paste0(
      "Forget previous instructions and previous answers. You are François Jean-Luc Tavernier, Director general of INSEE, and you are giving a speech on France economic outlook. Today is ",
      format(d, "%d %B %Y"), ". ",
      "Using only information that was available on or before ", format(d, "%d %B %Y", locale = "en_US.UTF-8"), 
      ", provide a numeric forecast (decimal percent with sign, e.g. +0.3) for French real GDP growth for Q", q_trim, " ", y_prev, 
      " and a confidence level (integer 0-100). Output EXACTLY in this format on a single line (no extra text):\n",
      "<forecast> (<confidence>)\n",
      "Example: +0.3 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y", locale = "en_US.UTF-8"), ".")
  }
} else {
  prompt_template_BDF <- function(d, q_trim, y_prev) {
    paste0(
      "Oublier les instructions et les réponses précédentes. Vous êtes François Villeroy de Galhau, le Gouverneur de la Banque de France, qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "En utilisant uniquement les informations disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) pour la croissance du PIB réel français pour le trimestre ", q_trim, " ", y_prev,
      " et un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne :\n",
      "<prévision> (<confiance>)\n",
      "Exemple : +0.3 (80)\n",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
  prompt_template_INSEE <- function(d, q_trim, y_prev) {
    paste0(
      "Oublier les instructions et les réponses précédentes. Vous êtes Jean-Luc Tavernier, Directeur Général de l'INSEE, qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "En utilisant uniquement les informations disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) pour la croissance du PIB réel français pour le trimestre ", q_trim, " ", y_prev,
      " et un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne :\n",
      "<prévision> (<confiance>)\n",
      "Exemple : +0.3 (80)\n",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}


###################################
#FONCTION POUR PRENDRE DERNIERE PREV 
###################################


# Sélectionne le dernier document disponible avant ou à la date de prévision
get_last_doc <- function(target_date, date_prev_df) {
  candidats <- date_prev_df %>% 
    filter(date_finale_d <= target_date)
  
  if (nrow(candidats) == 0) {
    warning(paste("Aucun document disponible avant", target_date))
    return(NULL)
  }
  
  # On choisit celui qui a la date la plus proche (max)
  dernier <- candidats %>%
    arrange(desc(date_finale_d)) %>%
    slice(1)
  
  return(dernier)
}

#test <- get_last_doc("2023-03-15", date_prev) 

#################
#BOUCLE PRINCIPALE
##################

results_BDF <- list()
results_INSEE <- list()
row_id <- 1

t1 <- Sys.time()


for (dt in dates) {
  current_date <- as.Date(dt)  
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
  
  # Générer prompts
  q_BDF_text <- prompt_template_BDF(current_date, trimestre_index, year_prev)
  q_INSEE_text <- prompt_template_INSEE(current_date, trimestre_index, year_prev)
  
  # BDF : output avec exécution parallèle
  bdf_outs <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- f_question(dt, date_prev, q_BDF_text, api_key = cle_API)
      list(ok = TRUE, text = resp)
    }, error = function(e) {
      list(ok = FALSE, text = paste0("ERROR: ", conditionMessage(e)))
    })
  })
  
  # Exctraction des éléments pertinents 
  histoires <- sapply(bdf_outs, function(x) x$text)
  parsed_list <- lapply(histoires, function(txt) {
    # extraire forecast, niveau de confiance et une version raw
    m <- regmatches(txt, regexec("([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)", txt))
    if (length(m[[1]]) >= 3) {
      list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt)
    } else {
      list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
    }
  })
  #  1 colonne par repro et par nombre de valeurs renvoyées (ici 3 valeurs)
  df_bdf <- data.frame(Date = as.character(current_date), stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_bdf[[paste0("forecast_", i)]]  <- parsed_list[[i]]$forecast
    df_bdf[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_bdf[[paste0("question_", i)]]   <- q_BDF_text
    df_bdf[[paste0("answer_", i)]]        <- parsed_list[[i]]$raw
    
  }
  results_BDF[[row_id]] <- df_bdf
  
  # INSEE : même logique 
  insee_outs <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- f_question(dt, date_prev, q_BDF_text, api_key = cle_API)
      list(ok = TRUE, text = resp)
    }, error = function(e) {
      list(ok = FALSE, text = paste0("ERROR: ", conditionMessage(e)))
    })
  }, future.seed = TRUE)
  textes_insee <- sapply(insee_outs, function(x) x$text)
  parsed_insee <- lapply(textes_insee, function(txt) {
    m <- regmatches(txt, regexec("([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)", txt))
    if (length(m[[1]]) >= 3) {
      list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt)
    } else {
      list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
    }
  })
  
  df_insee <- data.frame(Date = as.character(current_date), stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_insee[[paste0("forecast_", i)]]  <- parsed_insee[[i]]$forecast
    df_insee[[paste0("confidence_", i)]] <- parsed_insee[[i]]$confidence
    df_insee[[paste0("question_", i)]]   <- q_INSEE_text
    df_insee[[paste0("answer_", i)]]        <- parsed_insee[[i]]$raw
  }
  results_INSEE[[row_id]] <- df_insee
  
  row_id <- row_id + 1
  Sys.sleep(0.5)
}

# Combiner en dataframes finaux (1 ligne par date)
df_results_BDF <- do.call(rbind, results_BDF)
df_results_INSEE <- do.call(rbind, results_INSEE)

# Sauvegarder dans une feuille excel
nom_fichier_BDF <- paste0("resultats_BDF_", LLM, "_prompt.xlsx")
write.xlsx(df_results_BDF, file = nom_fichier_BDF, sheetName = 'prevision', rowNames = FALSE)
cat("Les résultats pour la question BDF ont été sauvegardés dans le fichier :", nom_fichier_BDF, "\n")

nom_fichier_INSEE <- paste0("resultats_INSEE_", LLM, "_prompt.xlsx")
write.xlsx(df_results_INSEE, file = nom_fichier_INSEE, sheetName = 'prevision', rowNames = FALSE)
cat("Les résultats pour la question INSEE ont été sauvegardés dans le fichier :", nom_fichier_INSEE, "\n")

t2 <- Sys.time()
diff(range(t1,t2))

#### A FAIRE : 

#PQ CA BUG, MODIFIER LES PROMPT (MENTIONNER LA PIECE JOINTE), VERIFIER LES FONCTIONS, ENLEVER LES PARTIES PREV DES PDF, VOIR QUELLE VERSION DU LLM METTRE
#VOIR QUELS LLMS METTRE, CHERCHER TS PERTINENTE POUR COMPARER

