#### Script : Requêtes LLM (Gemini) avec TS en input ####
#TS utilisée : Comptes trimestriels (base 2020) - Évolution du Produit intérieur brut total - Volume aux prix de l'année précédente chaînés - Série CVS-CJO  Identifiant 011794844

rm(list = ls())

# Repertoire/ env
setwd(dirname(getActiveDocumentContext()$path))
here::i_am("LLM_TS.R")
load_dot_env('.env')


###################################
# Paramètres initiaux
###################################

#Paramètres généraux
english <- 1 # 1 si prompt en anglais
temp_LLM <- 0.7  # Niveau de créativité des réponses 0.3/0.7/1.5 (castro-Leibovici)
n_repro <- 2  # Nombre de prévisions générées par date

# API Key (pour ellmer on utilise API_KEY_GEMINI)
cle_API <- Sys.getenv("API_KEY_GEMINI")
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")

#Chargement TS
gdp_ts <- read.xlsx(here("Time series", "serie_011794844_23092025.xlsx"))
gdp_ts <- gdp_ts |> 
  slice(4:305) |>
  mutate("Période" = as.character(Libellé),
         "Data" = `Comptes.trimestriels.(base.2020).-.Évolution.du.Produit.intérieur.brut.total.-.Volume.aux.prix.de.l'année.précédente.chaînés.-.Série.CVS-CJO`,
         .keep = "none") |>
  separate(Période, into = c("Année", "Trimestre"), sep = "-T") 


###################################
# Fonctions utilitaires
###################################



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
      "You will find hereafter a Time Serie of the French quarterly GDP growth. Using ONLY the the data provided and information available on or before ", 
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
      "Vous trouverez ci-après une série temporelle sur la croissance trimestrielle du PIB français. En utilisant UNIQUEMENT ces données fournies et les informations disponibles au plus tard le ", 
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
                                   params(temperature = temp_LLM, max_tokens = 5000)
)

# Creation de la list contenant les résultats
results_BDF <- list()
row_id <- 1 

t1 <- Sys.time()
for (dt in dates) {
  current_date <- as.Date(dt) 

  #Chargement des données souhaitées
  ts_data <-  get_last_ts(current_date, gdp_ts)
    
  
  # Initialisation des dates
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
  prompt_text <- paste0(prompt_template_BDF(current_date, trimestre_index ,
                                     year_prev), ts_data)
  
  # appel à Gemini 
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- chat_gemini$chat(prompt_text)
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
  df_bdf <- data.frame(
    Date   = as.character(current_date),
    Prompt = prompt_template_BDF(current_date, trimestre_index , year_prev),
    stringsAsFactors = FALSE)
  
  for (i in seq_len(n_repro)) {
    df_bdf[[paste0("forecast_", i)]]   <- parsed_list[[i]]$forecast
    df_bdf[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_bdf[[paste0("answer_", i)]]     <- parsed_list[[i]]$raw
  }
  
  results_BDF[[row_id]] <- df_bdf
  row_id <- row_id + 1
  Sys.sleep(0.5)
}

# réunir les prévisions pour chaque date
df_results_TS <- do.call(rbind, results_BDF)


# Enregistrement
write.xlsx(df_results_BDF, file = "resultats_BDF_Gemini_ts.xlsx", sheetName = 'prevision', rowNames = FALSE)
print("Enregistré: resultats_BDF_Gemini_ts.xlsx \n")

t2 <- Sys.time()
print(diff(range(t1, t2)))

############### A FAIRE ####################

#DONNER UN FICHIER EXCEL QUI SARRETE AVANT LA DATE DE PREVISION : pas que le LLM aie accès aux dernières données (idem que reco pour text avec xls bright)
#Faire après LLM TEXT si pertinent
