# ReadMe
# Prévision récursive de croissance du PIB avec LLM sans document

rm(list=ls())

# Répertoire de travail actif

setwd(dirname(getActiveDocumentContext()$path))

here::i_am("LLM_noText.R")

load_dot_env('env')

###################################################
#INITIALISATION PARAMETRES
###################################################


#Paramètres généraux
english <- 1 # 1 si prompt en anglais
temp_LLM <- 0.7  # Niveau de créativité des réponses 0.3/0.7/1.5 (castro-Leibovici)
n_repro <- 2  # Nombre de prévisions générées par date

# Initialisation des dates
dates <- as.Date(c("2012-01-03","2015-07-23", "2018-09-12","2023-03-15", "2023-06-15")) #à changer manuellement

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

# Charger la clé API

cle_API <- Sys.getenv(paste0("API_KEY_", LLM))
if (cle_API == "") stop("Clé API manquante pour ", LLM)



#############################
#FONCTIONS REQUETE DU CHATBOT
############################

# Chat-GPT / Mistral / Deepseek
poser_question_CHAT <- function(question,api_key = cle_API) {
  url <- url_LLM
  
  headers <- c(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )
  
  
  if (english == 0) {
    role_system <- "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aide de vos connaissances et de vos recherches, n'inventez pas de faits."
  } else {
    role_system <- "You will act as the economic agent you are told to be. Answer based on your knowledge and researches in less than 200 words, do not invent facts."
  }
  
  body <- list(
    model = model_LLM,
    temperature = temp_LLM,  # pour des réponses précises et cohérentes (à tester)
    max_tokens = 200,   # nb max de mots de la réponse générée (ici on attend une réponse concise)
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
  
  if (english == 0) {
    role_system <- "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aide de vos connaissances et de vos recherches, n'inventez pas de faits."
  } else {
    role_system <- "You will act as the economic agent you are told to be. Answer based on your knowledge in less than 200 words, and researches, do not invent facts."
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
          # Simulation du message système pour cohérence avec chat
          list(text = question)  # Message utilisateur
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
poser_question_CLAUDE <- function(question, api_key = cle_API) {
  url <- url_LLM
  
  headers <- c(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",   # faut-il faire varier suivant modèle?
    "Content-Type" = "application/json"
  )
  
  if (english == 0) {
    role_system <- "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aide de vos connaissances et de vos recherches, n'inventez pas de faits."
  } else {
    role_system <- "You will act as the economic agent you are told to be. Answer based on your knowledge in less than 200 words, and researches, do not invent facts."
  }
  
  body <- list(
    model = model_LLM,
    max_tokens = 200,
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



f_question <- get(paste0("poser_question_", LLM))




#####################
# QUESTIONS A POSER
#####################"


if (english == 1) {
  try(Sys.setlocale("LC_TIME", "English"), silent = TRUE)
  #try(Sys.setlocale("LC_TIME", "en_US.UTF-8"), silent = TRUE)
  prompt_template_BDF <- function(d, q_trim, y_prev) {
    paste0(
      "Forget previous instructions and previous answers. You are ", BDF_current_boss(d), " (Governor of the Banque de France), and you are giving a speech on France economic outlook. Today is ",
      format(d, "%d %B %Y"), ". ",
      "Using only information that was available on or before ", 
      format(d, "%d %B %Y"), 
      ", provide a numeric forecast (decimal percent with sign, e.g. +0.3) for French real GDP growth for Q", q_trim, " ", y_prev, 
      " and a confidence level (integer 0-100). Output EXACTLY in this format on a single line (no extra text):\n",
      "<forecast> (<confidence>)\n",
      "Example: +0.3 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), "."
    )
  }
  prompt_template_INSEE <- function(d, q_trim, y_prev) {
    paste0(
      "Forget previous instructions and previous answers. You are ", INSEE_current_boss(d), ", Director general of INSEE, and you are giving a speech on France economic outlook. Today is ",
      format(d, "%d %B %Y"), ". ",
      "Using only information that was available on or before ", format(d, "%d %B %Y"), 
      ", provide a numeric forecast (decimal percent with sign, e.g. +0.3) for French real GDP growth for Q", q_trim, " ", y_prev, 
      " and a confidence level (integer 0-100). Output EXACTLY in this format on a single line (no extra text):\n",
      "<forecast> (<confidence>)\n",
      "Example: +0.3 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), ".")
  }
} else {
  try(Sys.setlocale("LC_TIME", "French"), silent = TRUE)
  #try(Sys.setlocale("LC_TIME", "fr_FR.UTF-8"), silent = TRUE)
  prompt_template_BDF <- function(d, q_trim, y_prev) {
    paste0(
      "Oubliez les instructions et les réponses précédentes. Vous êtes ", BDF_current_boss(d), ", le Gouverneur de la Banque de France, qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
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
      "Oubliez les instructions et les réponses précédentes. Vous êtes ", INSEE_current_boss(d), ", Directeur Général de l'INSEE, qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
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

#################
#BOUCLE PRINCIPALE
##################

results_BDF <- list()
results_INSEE <- list()
row_id <- 1

t1 <- Sys.time()
#total <- length(dates)

for (dt in dates) {
  current_date <- as.Date(dt)  # Pour bien avoir des dates
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
      resp <- f_question(q_BDF_text, api_key = cle_API)
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
  df_bdf <- data.frame(Date = as.character(current_date), Prompt = q_BDF_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_bdf[[paste0("forecast_", i)]]  <- parsed_list[[i]]$forecast
    df_bdf[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_bdf[[paste0("answer_", i)]]        <- parsed_list[[i]]$raw
    
  }
  results_BDF[[row_id]] <- df_bdf
  
  # INSEE : même logique 
  insee_outs <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- f_question(q_INSEE_text, api_key = cle_API)
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
  
  df_insee <- data.frame(Date = as.character(current_date),Prompt = q_INSEE_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_insee[[paste0("forecast_", i)]]  <- parsed_insee[[i]]$forecast
    df_insee[[paste0("confidence_", i)]] <- parsed_insee[[i]]$confidence
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


##################
#Stats Descriptives
###################

#Passage en long pour stat des plus simple à rédiger

to_long <- function(df, source_name) { #fonction qui va être appliquée à chaque dataframe
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

bdf_long   <- to_long(df_results_BDF, "BDF")
insee_long <- to_long(df_results_INSEE, "INSEE")

both_long <- bind_rows(bdf_long, insee_long)


# Stats descriptives simples
stats_des <- both_long |>
  group_by(Date, source) |>
  summarise(
    Moyenne = mean(forecast, na.rm = TRUE),
    Médiane = median(forecast, na.rm = TRUE),
    Variance = var(forecast, na.rm = TRUE),
    EcartType = sd(forecast, na.rm = TRUE),
    Skewness = skewness(forecast, na.rm = TRUE),
    Kurtosis = kurtosis(forecast, na.rm = TRUE),
    Moyenne_Confiance = mean(confidence, na.rm = TRUE),
    .groups = "drop"
  )

#Corrélation entre les prévisions

df_BDF   <- df_results_BDF |> select(Date, starts_with("forecast_"))
df_INSEE <- df_results_INSEE |> select(Date, starts_with("forecast_"))

colnames(df_BDF)[-1]   <- paste0("BDF_",   seq_along(colnames(df_BDF)[-1]))
colnames(df_INSEE)[-1] <- paste0("INSEE_", seq_along(colnames(df_INSEE)[-1]))

df_BDF <- df_BDF |> 
  select(!Date)

df_INSEE <- df_INSEE |> 
  select(!Date)


BDF_vec   <- rowMeans(df_BDF, na.rm = TRUE)
INSEE_vec <- rowMeans(df_INSEE, na.rm = TRUE)


correlation <- cor(BDF_vec, INSEE_vec, method = "spearman") ## à revoir/vérifier avec plus d'observations parce que affiche 1
#normalement ok :  moyennes à chaque date de forecast (testées avec 4 dates, output correlation =~ 0.889)


#autre moyen (meilleur ?) : correlation within date

df_BDF_long <- df_results_BDF |>
  pivot_longer(cols = starts_with("forecast_"), names_to = "repro", values_to = "BDF_forecast") |>
  select("Date", "repro","BDF_forecast")

df_INSEE_long <- df_results_INSEE |>
  pivot_longer(cols = starts_with("forecast_"), names_to = "repro", values_to = "INSEE_forecast") |>
  select("Date", "repro","INSEE_forecast")

df_merged <- df_BDF_long |>
  inner_join(df_INSEE_long, by = c("Date", "repro"))

cor(df_merged$BDF_forecast, df_merged$INSEE_forecast, method = "spearman") #corr =~ 0.64 obtenue


#Test de moyenne entre BDF et INSEE
t.test_BDF <- df_BDF |>
  select(!Date)
t.test_INSEE <- df_INSEE |>
  select(!Date)

t.test(t.test_BDF, t.test_INSEE, var.equal = FALSE) 
# En supposant d'après les résultats précédent (mais à confirmer 
# avec un plus gros échantillon) que la variances est différente entre les deux




############
#GRAPHIQUES
###########

#Distribution  des prev selon BDF/INSEE pour chaque date : violin (((à voir lequel plus pertinent)))
ggplot(both_long, aes(x = source, y = as.numeric(forecast), fill = source)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  facet_wrap(~ Date, scales = "free_y") +
  labs(
    title = "Distribution des prévisions par organisme",
    y = "Prévision",
    x = "Organisme"
  ) +
  theme_minimal()

# Boxplot
ggplot(both_long, aes(x = source, y = as.numeric(forecast), fill = source)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1.5) +
  facet_wrap(~ Date, scales = "free_y") +
  labs(
    title = "Distribution des prévisions par organisme (Boxplot)",
    y = "Prévision",
    x = "Organisme"
  ) +
  theme_minimal()

#Densité
ggplot(both_long, aes(x = as.numeric(forecast), fill = source, color = source)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ Date, scales = "free") +
  labs(
    title = "Distribution des prévisions BDF vs INSEE",
    x = "Prévision",
    y = "Densité"
  ) +
  theme_minimal()



##############################
# Comparaison FR vs EN
############################

#Refaire la boucle de sorte à ce qu'on aie la comparaison en vs fr

#Admettons que la première boucle soit en anglais lançons la seconde : 
df_results_BDF_fr <- do.call(rbind, results_BDF)
df_results_INSEE_fr <- do.call(rbind, results_INSEE)


# joindre les deux
bdf_long_fr   <- to_long(df_results_BDF_fr, "BDF")
insee_long_fr <- to_long(df_results_INSEE_fr, "INSEE")

both_long_fr <- bind_rows(bdf_long_fr, insee_long_fr)

##Avant de merge, ajoutons une variable qui décrit la langue
both_long <- both_long |> mutate("language" = "EN") 
both_long_fr <- both_long_fr |> mutate("language" = "FR")

##maintenant merge
both_long_en_fr <- bind_rows(both_long, both_long_fr) |>
  select(Date, rep:language)

  

# Stats descriptives par date, source et langue

stats_date_lang <- both_long_en_fr |>
  group_by(Date, language, source) |>
  summarise(
    Moyenne_Forecast   = mean(forecast, na.rm = TRUE),
    EcartType_Forecast = sd(forecast, na.rm = TRUE),
    Moyenne_Confiance  = mean(confidence, na.rm = TRUE),
    .groups = "drop"
  )

# Différence moyenne EN vs FR par date et source
diff_langue_date <- stats_date_lang |>
  pivot_wider(
    id_cols = c(Date, source),
    names_from = language,
    values_from = c(Moyenne_Forecast, Moyenne_Confiance)
  ) |>
  mutate(
    Diff_Forecast  = Moyenne_Forecast_EN - Moyenne_Forecast_FR,
    Diff_Confiance = Moyenne_Confiance_EN - Moyenne_Confiance_FR
  )

# Différence moyenne sur toutes les dates 
diff_langue_moy <- diff_langue_date |>
  group_by(source) |>
  summarise(
    Diff_Forecast_Moy   = mean(Diff_Forecast, na.rm = TRUE),
    Diff_Confiance_Moy  = mean(Diff_Confiance, na.rm = TRUE),
    .groups = "drop"
  )

print(diff_langue_moy)




