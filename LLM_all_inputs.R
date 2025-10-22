#Script : Requêtes LLM (Gemini) avec PDF, TS et recherches


rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")


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

document_folder_BDF <- "docEMC_clean"
document_folder_INSEE <- "INSEE_Scrap"

# dates (exemple)
df_date <- as.Date(c("2023-03-01", "2023-06-01")) # format aaaa-mm-jj
#Dates utilisées
df_date <- read_xlsx(here("dates_prev.xlsx"))

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



#### CHARGER DATES EMC POUR LA BDF #######
## charger table de dates
date_prev_temp_BDF <- read_excel("Synthese_fileEMC.xlsx")
colnames(date_prev_temp_BDF)[1:4] <- c("fichier", "date_courte", "date_longue", "trimestre")

date_prev_temp_BDF <- date_prev_temp_BDF |>
  mutate(
    annee_prev = as.numeric(str_extract(fichier, "^\\d{4}")),        # 4 chiffres au début
    mois_prev  = as.numeric(str_extract(fichier, "(?<=\\d{4}_)\\d{1,2}"))  # chiffres après l'année et le _
  ) |>
  filter(annee_prev >= 2015)


date_prev_temp_BDF <- date_prev_temp_BDF |>
  mutate(
    annee_prev = as.numeric(str_extract(fichier, "^\\d{4}")),  # 4 chiffres au début
    mois_prev  = as.numeric(str_extract(fichier, "(?<=EMC_)\\d{1,2}(?=_)")), # chiffres après l'année et le _
    date_courte_d = as.Date(as.character(date_courte)),
    date_longue_d = as.Date(as.numeric((date_longue)), origin = "1899-12-30")
  ) |>
  mutate(
    date_finale_d = case_when(
      annee_prev >= 2015 & annee_prev <= 2019 ~ date_courte_d,
      annee_prev >= 2020 & annee_prev <= 2024 ~ date_longue_d
    )
  )

date_prev_BDF<- date_prev_temp_BDF |>
  select(fichier, trimestre, date_finale_d) |>
  filter(!is.na(date_finale_d))
# on supprime les lignes où la variable date_finale_d est manquante (NA) 
# car pas de publication de l'enquête (pandémie)

print(date_prev_BDF)

###################################
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

###################################
# Boucle principale BDF
###################################


# Forecast regex pattern qui sera appelé dans la boucle pour parse
forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"

# Creation de la list contenant les résultats
results_BDF <- list()
row_id_BDF <- 1 

t1 <- Sys.time()
for (dt in dates) {
  current_date <- as.Date(dt) 
  
  # Récupérer les 12 derniers documents
  all_last_docs <- get_all_last_doc(date_prev_BDF, current_date)
  
  #Transformer en chemins complets
  all_bdf_docs_to_combine <- map_chr(all_last_docs, path_from_docname, folder = document_folder_BDF)
  
  #Ajout du PDF de la TS
  pib_enq_path <- normalizePath("./Data_PIB_ENQ-1-3.pdf", winslash = "/", mustWork = TRUE)
  all_bdf_docs_to_combine <- c(all_bdf_docs_to_combine, pib_enq_path)
  
  # Créer le PDF combiné
  combined_pdf_path <- file.path("./BDF_files_used/", paste0("combined_BDF_", format(current_date, "%Y%m%d"), ".pdf"))
  BDF_path <- merge_pdfs(all_bdf_docs_to_combine, combined_pdf_path)
  
  
  # Chargement du pdf souhaité
  uploaded_doc <- google_upload(
    BDF_path,
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
  
  results_BDF[[row_id_BDF]] <- df_bdf
  row_id_BDF<- row_id_BDF+ 1
  Sys.sleep(0.5)
}

# réunir les prévisions pour chaque date
df_results_all_BDF <- do.call(rbind, results_BDF)

# Enregistrement
write.xlsx(df_results_all_BDF, file = "resultats_BDF_Gemini_all.xlsx", sheetName = 'prevision', rowNames = FALSE)
print("Enregistré: resultats_BDF_Gemini_all.xlsx \n")

t2 <- Sys.time()
print(diff(range(t1, t2)))


########################
#BOUCLE PRINCIPALE INSEE
########################

# Forecast regex pattern qui sera appelé dans la boucle pour parse
forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"

# Creation de la list contenant les résultats
results_INSEE <- list()
row_id_INSEE <- 1 

t1 <- Sys.time()

for (dt in dates) {
  current_date <- as.Date(dt) 
  
  all_insee_docs_to_combine <- c(
    get_last_12_insee_docs_by_type(current_date, "EMI",  document_folder_INSEE),
    get_last_12_insee_docs_by_type(current_date, "SER",  document_folder_INSEE),
    get_last_12_insee_docs_by_type(current_date, "BAT",  document_folder_INSEE)
  )
  
  # Ajouter du PDF de la TS
  pib_enq_path <- normalizePath("./Data_PIB_ENQ-1-3.pdf", winslash = "/", mustWork = TRUE)
  all_insee_docs_to_combine <- c(all_insee_docs_to_combine, pib_enq_path)
  
  #Concaténation en un PDF
  combined_pdf_path <- file.path( "./INSEE_files_used/", paste0("combined_INSEE_", format(current_date, "%Y%m%d"), ".pdf"))
  INSEE_path <- merge_pdfs(all_insee_docs_to_combine, combined_pdf_path)
  
  # Chargement du pdf concaténé souhaité
  uploaded_doc <- google_upload(
    INSEE_path,
    base_url = "https://generativelanguage.googleapis.com/",
    api_key = cle_API
  )
  
  # Initialisation des dates
  current_date <- as.Date(dt)
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
  df_insee <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_insee[[paste0("forecast_", i)]]  <- parsed_list[[i]]$forecast
    df_insee[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_insee[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
  }
  
  results_INSEE[[row_id_INSEE]] <- df_insee
  row_id_INSEE <- row_id_INSEE + 1
  Sys.sleep(0.5)
}

# réunir les prévisions pour chaque date
df_results_all_INSEE <- do.call(rbind, results_INSEE)


# Enregistrement
write.xlsx(df_results_all_INSEE, file = "resultats_INSEE_Gemini_all.xlsx", sheetName = 'prevision', rowNames = FALSE)
print("Enregistré: resultats_INSEE_Gemini_all.xlsx \n")

t2 <- Sys.time()
print(diff(range(t1, t2)))


##################
#Stats Descriptives
###################

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

bdf_text_long   <- to_long(df_results_all_BDF, "BDF")
insee_text_long <- to_long(df_results_all_INSEE, "INSEE")

both_text_long <- bind_rows(bdf_text_long, insee_text_long)


# Stats descriptives simples
stats_des_text <- both_text_long |>
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


#Arrangement des df afin de pouvoir mieux les exploiter
df_BDF_text   <- df_results_all_BDF |> select(Date, starts_with("forecast_"))
df_INSEE_text  <- df_results_all_INSEE |> select(Date, starts_with("forecast_"))

colnames(df_BDF_text)[-1]   <- paste0("BDF_",   seq_along(colnames(df_BDF_text)[-1]))
colnames(df_INSEE_text)[-1] <- paste0("INSEE_", seq_along(colnames(df_INSEE_text)[-1]))

df_BDF_text <- df_BDF_text |> 
  select(!Date)

df_INSEE_text <- df_INSEE_text |> 
  select(!Date)

#Corrélation entre les prévisions
BDF_cor   <- rowMeans(df_BDF_text, na.rm = TRUE)
INSEE_cor <- rowMeans(df_INSEE_text, na.rm = TRUE)


correlation <- cor(BDF_cor, INSEE_cor, method = "spearman") ## à revoir/vérifier avec plus d'observations parce que affiche 1
#normalement ok :  moyennes à chaque date de forecast (testées avec 4 dates, output correlation =~ 0.889)



#Test de moyenne entre BDF et INSEE

t.test(df_BDF_text, df_INSEE_text, var.equal = FALSE) 
# En supposant d'après les résultats précédent (mais à confirmer 
# avec un plus gros échantillon) que la variances est différente entre les deux




############
#GRAPHIQUES
###########

#Distribution  des prev selon BDF/INSEE pour chaque date : violin (((à voir lequel plus pertinent)))
ggplot(both_text_long, aes(x = source, y = as.numeric(forecast), fill = source)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  facet_wrap(~ Date, scales = "free_y") +
  labs(
    title = "Distribution des prévisions par organisme",
    y = "Prévision",
    x = "Organisme"
  ) +
  theme_minimal()

# Boxplot
ggplot(both_text_long, aes(x = source, y = as.numeric(forecast), fill = source)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1.5) +
  facet_wrap(~ Date, scales = "free_y") +
  labs(
    title = "Distribution des prévisions par organisme (Boxplot)",
    y = "Prévision",
    x = "Organisme"
  ) +
  theme_minimal()

#Densité
ggplot(both_text_long, aes(x = as.numeric(forecast), fill = source, color = source)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ Date, scales = "free") +
  labs(
    title = "Distribution des prévisions BDF vs INSEE",
    x = "Prévision",
    y = "Densité"
  ) +
  theme_minimal()






