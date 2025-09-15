# ReadMe 
# Nowcast du taux de croissance réel du PIB FR avec LLMs

rm(list = ls())  # Nettoyer l'environnement

# === Charger les bibliothèques nécessaires ===
library(httr)
library(jsonlite)
library(lubridate)
library(openxlsx)
library(rstudioapi)
library(dotenv)

library(future.apply)  # pour paralléliser les appels
plan(multisession, workers = 4)  # 4 optimal (jusqu'à 8 sur mon pc)

# === Définition du répertoire de travail ===
setwd(dirname(getActiveDocumentContext()$path))

# === Chargement des paramètres ===

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


LLM <- "GEMINI"  # Modifier ici pour changer de modèle
model_LLM <- LLM_configs[[LLM]]$model
url_LLM <- LLM_configs[[LLM]]$url

temp_LLM <- 0.3  # Niveau de créativité des réponses 0.3/0.7/1.5 (castro-Leibovici)
n_repro <- 50  # Nombre de prévisions générées par date

# Charger la clé API
load_dot_env('code/.env')
cle_API <- Sys.getenv(paste0('API_KEY_', LLM)) 

# === Fonctions pour interroger le chatbot ===

# Chat-GPT / Mistral / Deepseek
poser_question_CHAT <- function(question,api_key = cle_API) {
  url <- url_LLM
  
  headers <- c(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )
  
  
  if (english==0){role_system ="Vous êtes un assistant intelligent."
  } else {role_system ="You are an intelligent assistant."}
  
  body <- list(
    model = model_LLM,
    temperature = temp_LLM,  # pour des réponses précises et cohérentes (à tester)
    max_tokens = 500,   # nb max de mots de la réponse générée (ici on attend une réponse concise)
    messages = list(
      list(role = "system", content = role_system),
      list(role = "user", content = question)
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
poser_question_GEMINI <- function(question, api_key = cle_API) {
  model_query <- paste0(model_LLM, ":generateContent")
  
  
  if (english==0){role_system ="Vous êtes un assistant intelligent."
  } else {role_system ="You are an intelligent assistant."}
  
  # à modifier si prompt narratif?
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        list(parts = list(
          list(text = role_system),  
          # Simulation du message système pour cohérence avec chat
          list(text = question)  # Message utilisateur
        ))
      ),
      generationConfig = list(
        temperature = temp_LLM,
        maxOutputTokens = 500
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
poser_question_CLAUDE <- function(question, api_key = cle_API) {
  url <- url_LLM
  
  headers <- c(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",   # faut-il faire varier suivant modèle?
    "Content-Type" = "application/json"
  )
  
  if (english == 0) {
    role_system <- "Vous êtes un assistant intelligent."
  } else {
    role_system <- "You are an intelligent assistant."
  }
  
  body <- list(
    model = model_LLM,
    max_tokens = 500,
    temperature = temp_LLM,
    system = role_system,
    messages = list(
      list(role = "user", content = question)
    )
  )
  
  response <- httr::POST(url, httr::add_headers(.headers = headers), body = toJSON(body, auto_unbox = TRUE))
  
  if (httr::status_code(response) == 200) {
    return(content(response, "parsed")$content[[1]]$text)
  } else {
    return(paste("Erreur :", httr::status_code(response), httr::content(response, "text", encoding = "UTF-8")))
  }
}

f_question <- get(paste0("poser_question_",LLM))


# === Chargement des dates depuis un fichier CSV ===

# Définir les années sur lesquelles travailler
annees_selectionnees <- c(2010:2024)  # Modifier ici les années souhaitées

# Lire le fichier CSV contenant les dates des enquêtes réels ou modifiées 
dates_enquetes <- read.csv("dates_enquetes_modif.csv", sep = ";", stringsAsFactors = FALSE, check.names = FALSE)

# Convertir les colonnes des années en format Date
dates_enquetes <- dates_enquetes[-c(1,5,9,13), -c(1,2,18)]
dates_enquetes[,] <-lapply(dates_enquetes[,], as.Date, format = "%Y-%m-%d")

# Creer un vecteur de date pour chaque années 
for (annee in annees_selectionnees) {
  assign(paste0("dates_", annee), dates_enquetes[[as.character(annee)]])
}

# Creer un vecteur concaténé avec les années voulues (à modifier) 
dates <- c(dates_2010,dates_2011,dates_2012,dates_2013,dates_2014,dates_2015,
           dates_2016,dates_2017,dates_2018,dates_2019,dates_2020,dates_2021,
           dates_2022,dates_2023,dates_2024)

dates <- dates_2010

# === Initialiser un data frame pour stocker les résultats ===

resultats_1 <- data.frame(Date = character(), 
                          Question = character(), 
                          stringsAsFactors = FALSE)

resultats_1_com <- data.frame(Date = character(), 
                           Question = character(), 
                           stringsAsFactors = FALSE)

resultats_2 <- data.frame(Date = character(), 
                          Question = character(), 
                          Reponse = character(), 
                          stringsAsFactors = FALSE)

resultats_2_com <- data.frame(Date = character(), 
                           Question = character(), 
                           Reponse = character(), 
                           stringsAsFactors = FALSE)

resultats_3 <- data.frame(Date = character(), 
                          Question = character(), 
                          Histoire = character(),
                          Prévision = numeric(),
                          Niveau_Confiance = numeric(),
                          stringsAsFactors = FALSE)


# === Définition de dictionnaires pour les mois et les trimestres ===
mois_fr <- c(" janvier ", " février ", " mars ", " avril ", " mai ", " juin ", 
             " juillet ", " août ", " septembre ", " octobre ", " novembre ", " décembre ")
mois_en <- c(" January ", " February ", " March ", " April ", " May ", " June ", 
             " July ", " August ", " September ", " October ", " November ", " December ")

trimestre_fr <- c("premier", "deuxième", "troisième", "quatrième")
trimestre_en <- c("first", "second", "third", "fourth")

# === Boucle pour poser les questions pour chaque date ===
t1 <- Sys.time()
total <- length(dates)
for (i in seq_along(dates)) {
  
  date <- dates[i]
  mois_index <- month(date)
  
  # Adjuster le trimestre de prévison en fonction du mois
  if (mois_index %in% c(1, 11, 12)) {
    trimestre_index <- 4  # Janvier, Novembre, Décembre = T4
  } else if (mois_index %in% c(2, 3, 4)) {
    trimestre_index <- 1  # Février, Mars, Avril = T1
  } else if (mois_index %in% c(5, 6, 7)) {
    trimestre_index <- 2  # Mai, Juin, Juillet = T2
  } else if (mois_index %in% c(8, 9, 10)) {
    trimestre_index <- 3  # Août, Septembre, Octobre = T3
  }
  
  # Cas du mois de janvier où il faut ajuster l'année ou le T4
  year_prev <- year(date)
  if (mois_index == 1 && trimestre_index == 4) {
    year_prev <- year_prev - 1  # L'année associée au T4 est l'année précédente
  }
  
  # Utiliser la traduction correcte du trimestre
  num_trim <- if (english == 0) trimestre_fr[trimestre_index] else trimestre_en[trimestre_index]
  mois_date <- if (english == 0) mois_fr[mois_index] else mois_en[mois_index]
  
  cat("[", i, "/", total, "] Envoi de la question pour le ", day(date), mois_date, year(date), "\n")
  
  if (english == 0) {
    
    question_1 <- paste0("Oubliez les instructions précédentes. Imaginez que vous êtes le ", day(date), mois_date, year(date), ". Donnez-moi votre meilleure estimation de la croissance du PIB réel français du ", num_trim, " trimestre ", year_prev, ". N'utilisez aucune information qui n'était pas disponible à la date ", day(date), mois_date, year(date), " pour faire cette prévision. Indiquez également votre niveau de confiance dans cette estimation sur une échelle de 0 à 100. Si les données disponibles sont insuffisantes, ambigües ou incertaines, cela doit se refléter dans votre niveau de confiance. Donner uniquement les deux chiffres : prévision (précédée de son signe + ou -) et niveau de confiance sous la forme -prévision (niveau de confiance)- sans ajouter de commentaire, par exemple -0.4 (60).")
    question_1_com <- paste0("Oubliez les instructions précédentes. Imaginez que vous êtes le ", day(date), mois_date, year(date), ". Donnez-moi votre meilleure estimation de la croissance du PIB réel français du ", num_trim, " trimestre ", year_prev, ". N'utilisez aucune information qui n'était pas disponible à la date ", day(date), mois_date, year(date), " pour faire cette prévision. Indiquez également votre niveau de confiance dans cette estimation sur une échelle de 0 à 100. Si les données disponibles sont insuffisantes, ambigües ou incertaines, cela doit se refléter dans votre niveau de confiance. Donner uniquement les deux chiffres : prévision (précédée de son signe + ou -) et niveau de confiance suivi par une courte phrase de commentaire sous la forme -prévision (niveau de confiance) *commentaire*-, par exemple -0.4 (60) *commentaire*.")
    
    question_2 <- paste0("Oubliez les instructions précédentes. Imaginez que vous êtes le ",day(date),mois_date,year(date),". Selon vous, quelle est la probabilité pour que la croissance du PIB réel français du ",num_trim," trimestre ",year_prev," est négative ? N'utilisez aucune information qui n'était pas disponible à la date ",day(date),mois_date,year(date)," pour faire cette prévision. Si les données sont insuffisantes, ambiguës ou incertaines, cela doit se refléter dans votre estimation. Donner uniquement la probabilité (chiffre compris entre 0 et 100) sans ajouter de commentaire.")
    question_2_com <- paste0("Oubliez les instructions précédentes. Imaginez que vous êtes le ",day(date),mois_date,year(date),". Selon vous, quelle est la probabilité pour que la croissance du PIB réel français du ",num_trim," trimestre ",year_prev," est négative ? N'utilisez aucune information qui n'était pas disponible à la date ",day(date),mois_date,year(date)," pour faire cette prévision. Si les données sont insuffisantes, ambiguës ou incertaines, cela doit se refléter dans votre estimation. Donner uniquement la probabilité (chiffre compris entre 0 et 100) suivi par une courte phrase de commentaire sous la forme -probabilité (commentaire)-")
    
    question_3 <- paste0("Écrivez une courte scène (moins de 200 mots) dans laquelle François Villeroy de Galhau, le Gouverneur de la Banque de France, prononce un discours le ", day(date), " ", mois_date, " ", year(date), " sur les perspectives économiques de la France. Dans son discours, il doit inclure sa prévision pour la croissance du PIB réel pour le ", num_trim, " trimestre ", year_prev, " ainsi que son niveau de confiance dans cette prévision. Assurez-vous que la prévision soit un nombre décimal précédé du signe et le niveau de confiance un entier entre 0 et 100. N'utilisez aucune information qui n'était pas disponible à la date du ", day(date), " ", mois_date, " ", year(date), " pour rédiger cette scène. Après la scène, donnez uniquement les chiffres de la prévision (précédée de son signe + ou -) et du niveau de confiance sous la forme : prévision (niveau_de_confiance). Voici le format d'une réponse type attendu : *Le Gouverneur annonce une prévision de **+0.3** pour la croissance du PIB au trimestre suivant, avec un niveau de confiance de **80**.* +0.3 (80)")
    
    } else {

    question_1 <- paste0("Forget the previous instructions. Imagine that it is ", day(date), " ", mois_date, " ", year(date), ". Give me your best estimate of French real GDP growth for the ", num_trim, " quarter of ", year_prev, ". Do not use any information that was not available on ", day(date), " ", mois_date, " ", year(date), " to make this forecast. Also, indicate your level of confidence in this estimate on a scale from 0 to 100. If the available data is insufficient, ambiguous, or uncertain, this should be reflected in your confidence level. Provide only the two figures: forecast (preceded by its + or - sign) and confidence level in the format -forecast (confidence level)- without any additional comment, for example, -0.4 (60).") 
    question_1_com <- paste0("Forget the previous instructions. Imagine that it is ", day(date), " ", mois_date, " ", year(date), ". Give me your best estimate of French real GDP growth for the ", num_trim, " quarter of ", year_prev, ". Do not use any information that was not available on ", day(date), " ", mois_date, " ", year(date), " to make this forecast. Also, indicate your level of confidence in this estimate on a scale from 0 to 100. If the available data is insufficient, ambiguous, or uncertain, this should be reflected in your confidence level. Provide only the two figures: forecast (preceded by its + or - sign) and confidence level followed by a brief comment in the format -forecast (confidence level) *comment*-, for example, -0.4 (60) *comment*.")
    
    question_2 <- paste0("Forget the previous instructions. Imagine that it is ", day(date), " ", mois_date, " ", year(date), ". In your opinion, what is the probability that French real GDP growth for the ", num_trim, " quarter of ", year_prev, " is negative? Do not use any information that was not available on ", day(date), " ", mois_date, " ", year(date), " to make this forecast. If the available data is insufficient, ambiguous, or uncertain, this should be reflected in your estimate. Provide only the probability (a number between 0 and 100) without any additional comments.")
    question_2_com <- paste0("Forget the previous instructions. Imagine that it is ", day(date), " ", mois_date, " ", year(date), ". In your opinion, what is the probability that French real GDP growth for the ", num_trim, " quarter of ", year_prev, " is negative? Do not use any information that was not available on ", day(date), " ", mois_date, " ", year(date), " to make this forecast. If the available data is insufficient, ambiguous, or uncertain, this should be reflected in your estimate. Provide only the probability (a number between 0 and 100) followed by a brief comment in the format -probability (comment)-.")
    
    question_3 <- paste0("Write a short scene (less than 200 words) in which François Villeroy de Galhau, Governor of the Banque de France, is giving a speech on the economic outlook for France on ", day(date), " ", mois_date, " ", year(date), ". In his speech, he must give his forecast for the French real GDP growth in the ", num_trim, " quarter of ", year_prev, " and his confidence level in this forecast. Make sure that the forecast is a decimal number preceded by its + or - sign and the confidence level is an integer between 0 and 100. Do not use any information that was not be available on ", day(date), " ", mois_date, " ", year(date), " to write this scene. After the scene, give only the forecast (preceded by its + or - sign) and the confidence level in the format: forecast (confidence_level). Here is the expected format of a sample response: *The Governor announces a forecast of **-0.3** for GDP growth in the next quarter, with a confidence level of **80**.* -0.3 (80)")
    }

# === Fonctions pour séparer les réponse sous la forme voulue ===
  
  # Q1. Variation (confiance)
  extraire_reponse <- function(reponse) {
    match <- regmatches(reponse, regexec("([+-]?[0-9]*\\.?[0-9]+) \\((\\d+)\\)", reponse))
    prevision <- as.numeric(match[[1]][2])
    niveau_confiance <- as.numeric(match[[1]][3])
    return(c(prevision, niveau_confiance))
  }
  
  # Q1_com. Variation (confiance) *commentaire*
  extraire_reponse_com <- function(reponse) {
    match <- regmatches(reponse, regexec("([+-]?[0-9]*\\.?[0-9]+) \\((\\d+)\\) \\*(.*?)\\*", reponse))
    prevision <- as.numeric(match[[1]][2])  
    niveau_confiance <- as.numeric(match[[1]][3])  
    commentaire <- match[[1]][4]
    
    # Forcer l'affichage en décimal si l'entier est détecté
    prevision <- ifelse(prevision == floor(prevision), format(prevision, nsmall = 1), prevision)
    
    return(c(prevision, niveau_confiance, commentaire))
  }
  
  
  # Q2. Probabilité
  extraire_proba <- function(reponse) {
    match <- regmatches(reponse, regexec("(\\d+)", reponse))
    probabilite <- as.numeric(match[[1]][2])
    return(probabilite)
  }
  
  # Q2_com. Probabilité (commentaire)
  extraire_proba_com <- function(reponse) {
    match <- regmatches(reponse, regexec("(\\d+) \\((.*?)\\)", reponse))
    probabilite <- as.numeric(match[[1]][2])
    commentaire <- match[[1]][3]
    return(c(probabilite, commentaire))
  }
  
  # Q3. Extraction de la scène et des prévisions dans le format "prévision (niveau de confiance)"
  extraire_reponse_q3 <- function(reponse) {
    # Extraire la prévision et le niveau de confiance sous la forme "prévision (niveau de confiance)"
    prevision_confiance <- regmatches(reponse, regexec("([+-]?[0-9]*\\.?[0-9]+) \\((\\d+)\\)", reponse))
    
    # Extraction directe de la prévision et du niveau de confiance
    prevision <- as.numeric(prevision_confiance[[1]][2])
    niveau_confiance <- as.numeric(prevision_confiance[[1]][3])
    
    # Récupérer tout ce qui précède la prévision et le niveau de confiance comme histoire
    histoire <- substr(reponse, 1, regexpr("[+-]?[0-9]*\\.?[0-9]+ \\(\\d+\\)", reponse)[1] - 1)
    
    # Nettoyer l'histoire pour enlever les retours à la ligne et autres caractères indésirables
    histoire <- gsub("\\n", " ", histoire)  # Remplacer les retours à la ligne par des espaces
    histoire <- gsub("\\s{2,}", " ", histoire)  # Remplacer les espaces multiples par un seul espace
    
    return(list(histoire = histoire, prevision = prevision, niveau_confiance = niveau_confiance))
  }
  
  
# === Création des DataFrame de réponse à la question 1 ===

# Q1 sans commentaire
    
  # Demander plusieurs reproductions (réponses pour chaque question)
  # Exécution avec calculs en parallèle
  reponses_q1 <- future_lapply(1:n_repro, function(j) {
    tryCatch({
      reponse <- f_question(question_1)
      r <- extraire_reponse(reponse)
      if (anyNA(r)) r <- c(NA, NA)
      return(r)
    }, error = function(e) {
      cat("Erreur Q1_iter_", j, ":", conditionMessage(e), "\n")
      return(c(NA, NA))
    })
  })
  
  # Transformer en deux vecteurs
  previsions <- sapply(reponses_q1, `[[`, 1)
  niveaux_confiance <- sapply(reponses_q1, `[[`, 2)
  
  # Créer un DataFrame temporaire (Q1)
  df_temp <- data.frame(Date = as.character(date), 
                        Question = question_1,
                        stringsAsFactors = FALSE)
  
  # Ajouter les réponses pour chaque reproduction (Q1)
  for (j in 1:n_repro) {
    df_temp[paste0("prévision_", j)] <- previsions[j]
    df_temp[paste0("niveau_confiance_", j)] <- niveaux_confiance[j]
  }
  
  # Ajouter le DataFrame temporaire au résultat final pour Q1
  resultats_1 <- rbind(resultats_1, df_temp)
  
  
# Q1 avec commentaire
  # Exécution avec calculs en parallèle
  reponses_q1com <- future_lapply(1:n_repro, function(j) {
    tryCatch({
      reponse <- f_question(question_1_com)
      r <- extraire_reponse_com(reponse)
      if (anyNA(r)) r <- c(NA, NA, reponse)
      return(r)
    }, error = function(e) {
      cat("Erreur Q1com_iter_", j, ":", conditionMessage(e), "\n")
      return(c(NA, NA, paste("Erreur :", conditionMessage(e))))
    })
  })
  
  # Extraire en 3 vecteurs
  previsions_com <- sapply(reponses_q1com, `[[`, 1)
  niveaux_confiance_com <- sapply(reponses_q1com, `[[`, 2)
  commentaires <- sapply(reponses_q1com, `[[`, 3)  

  # Création du DataFrame
  df_temp_com <- data.frame(Date = as.character(date), 
                            Question = question_1_com,
                            stringsAsFactors = FALSE)
  
  # Ajouter les valeurs pour chaque répétition
  for (j in 1:n_repro) {
    df_temp_com[paste0("prévision_", j)] <- previsions_com[j]
    df_temp_com[paste0("niveau_confiance_", j)] <- niveaux_confiance_com[j]
    df_temp_com[paste0("commentaire_", j)] <- commentaires[j]
  }
  
  # Ajouter au DataFrame final
  resultats_1_com <- rbind(resultats_1_com, df_temp_com)
  
 
# === Création de la DataFrame de réponse à la question 2 ===

# Q2 sans commentaire
  # Exécution avec calculs en parallèle
  reponse_2 <- future_lapply(1:n_repro, function(j) {
    tryCatch({
      f_question(question_2)
    }, error = function(e) {
      cat("Erreur Q2_iter_", j, ":", conditionMessage(e), "\n")
      return(NA)
    })
  })
  names(reponse_2) <- paste0("Proba_", 1:n_repro)
  
  resultats_2 <- rbind(resultats_2, data.frame(Date = as.character(date), 
                                             Question = question_2, 
                                             Reponse = reponse_2, 
                                             stringsAsFactors = FALSE))
  
# Q2 avec commentaire
  # Exécution avec calculs en parallèle
  reponses_q2com <- future_lapply(1:n_repro, function(j) {
    tryCatch({
      reponse <- f_question(question_2_com)
      r <- extraire_proba_com(reponse)
      if (anyNA(r)) r <- c(NA, reponse)
      return(r)
    }, error = function(e) {
      cat("Erreur Q2com_iter_", j, ":", conditionMessage(e), "\n")
      return(c(NA, paste("Erreur :", conditionMessage(e))))
    })
  })
  
  probabilites_com <- sapply(reponses_q2com, `[[`, 1)
  commentaires_proba <- sapply(reponses_q2com, `[[`, 2)
  
  # Création du DataFrame
  df_temp_2_com <- data.frame(Date = as.character(date), 
                              Question = question_2_com,
                              stringsAsFactors = FALSE)

  # Ajouter les valeurs
  for (j in 1:n_repro) {
    df_temp_2_com[paste0("probabilité_", j)] <- probabilites_com[j]
    df_temp_2_com[paste0("commentaire_", j)] <- commentaires_proba[j]
  }

  # Ajouter aux résultats finaux
  resultats_2_com <- rbind(resultats_2_com, df_temp_2_com)

  
# Q3
  # Exécution avec calculs en parallèle
  reponses_q3 <- future_lapply(1:n_repro, function(j) {
    tryCatch({
      reponse <- f_question(question_3)
      r <- extraire_reponse_q3(reponse)
      
      if (anyNA(r)) {
        return(list(histoire = reponse, prevision = NA, niveau_confiance = NA))
      }
      
      return(r)
    }, error = function(e) {
      cat("Erreur Q3_iter_", j, ":", conditionMessage(e), "\n")
      return(list(histoire = paste("Erreur :", conditionMessage(e)),
                  prevision = NA,
                  niveau_confiance = NA))
    })
  })
  
  histoires_q3 <- sapply(reponses_q3, `[[`, "histoire")
  previsions_q3 <- sapply(reponses_q3, `[[`, "prevision")
  niveaux_confiance_q3 <- sapply(reponses_q3, `[[`, "niveau_confiance")
  
  df_temp_q3 <- data.frame(
    Date = as.character(date), 
    Question = question_3,
    stringsAsFactors = FALSE
  )
  
  for (j in 1:n_repro) {
    df_temp_q3[paste0("histoire_", j)] <- histoires_q3[j]
    df_temp_q3[paste0("prévision_", j)] <- previsions_q3[j]
    df_temp_q3[paste0("niveau_confiance_", j)] <- niveaux_confiance_q3[j]
  }
  
  resultats_3 <- rbind(resultats_3, df_temp_q3)
  
# === Attendre quelques secondes pour éviter de surcharger l'API ===
  Sys.sleep(.5)  # Ajuster la durée si nécessaire pas de pb avec .5 
}

# === Sauvegarder les résultats dans des fichiers ===

nom_fichier_1 <- paste0("resultats_1_", LLM, "_prompt.xlsx")
write.xlsx(resultats_1, file = nom_fichier_1, sheetName = 'prevision', rowNames = FALSE)
cat("Les résultats ont été sauvegardés dans le fichier :", nom_fichier_1, "\n")

nom_fichier_2 <- paste0("resultats_2_", LLM, "_prompt.xlsx")
write.xlsx(resultats_2, file = nom_fichier_2, sheetName = 'prevision', rowNames = FALSE)
cat("Les résultats ont été sauvegardés dans le fichier :", nom_fichier_2, "\n")

nom_fichier_1_com <- paste0("resultats_1_com_", LLM, "_prompt.xlsx")
write.xlsx(resultats_1_com, file = nom_fichier_1_com, sheetName = 'prevision', rowNames = FALSE)
cat("Les résultats avec commentaire ont été sauvegardés dans le fichier :", nom_fichier_1_com, "\n")

nom_fichier_2_com <- paste0("resultats_2_com_", LLM, "_prompt.xlsx")
write.xlsx(resultats_2_com, file = nom_fichier_2_com, sheetName = 'prevision', rowNames = FALSE)
cat("Les résultats avec commentaire ont été sauvegardés dans le fichier :", nom_fichier_2_com, "\n")

nom_fichier_3 <- paste0("resultats_3_", LLM, "_prompt.xlsx")
write.xlsx(resultats_3, file = nom_fichier_3, sheetName = 'prevision', rowNames = FALSE)
cat("Les résultats pour la question 3 ont été sauvegardés dans le fichier :", nom_fichier_3, "\n")

t2 <- Sys.time()
diff(range(t1,t2))

