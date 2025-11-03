#Script : Requête LLM avec enquête en input, de 1 à 3 selon le mois au sein du trimestre

rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")

#######################
#Paramètres spécifiques
#######################

#Systeme prompt
sys_prompt <- ifelse(english == 1,
                     "You will act as the economic agent you are told to be. Answer based on your knowledge and the document provided in less than 200 words. You will use only the information available as of the forecast date, do not invent facts." ,
                     "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots, à l'aide de vos connaissances et du document fourni. Vous n'utiliserez que l'information disponible à la date du jour de la prévision, n'inventez pas de faits."
)

#Initialisation LLM
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")
chat_gemini <- chat_google_gemini( system_prompt = sys_prompt,
                                   base_url = "https://generativelanguage.googleapis.com/v1beta/", 
                                   api_key = cle_API, 
                                   model = "gemini-2.5-pro", 
                                   params(temperature = temp_LLM, max_tokens = 5000)
)

document_folder_BDF <- "docEMC_clean"
document_folder_INSEE <- "INSEE_Scrap"
output_folder_BDF <- "BDF_files_used"
output_folder_INSEE <- "INSEE_files_used"



###################################
# Prompts 
###################################


if (english == 1) {
  try(Sys.setlocale("LC_TIME", "English"), silent = TRUE)
  
  #Renvoie le bon dirigeant
  current_boss <- function(type, d) {
    if (type == "BDF") return(BDF_current_boss(d))
    if (type == "INSEE") return(INSEE_current_boss(d))
  }
  
  # 
  prompt_template <- function(type, d, q_trim, y_prev) {
    boss <- current_boss(type, d)
    position <- ifelse(type == "BDF", "Governor of the Banque de France" , "Director General of INSEE")
    current_quarter <- if (q_trim == 1){
      "first"}
    else if (q_trim == 2){
      "second"
    }else if (q_trim == 3){
      "third"}else{
        "fourth"}
    
    
    paste0(
      "Forget the previous instructions and answers. You are ", boss, ", ", position, 
      ", and you are giving a speech about the economic outlook of France. Today is ",
      format(d, "%d %B %Y"), ". ",
      "Using only information that was available on or before ", format(d, "%d %B %Y"),
      ", provide a numeric forecast (decimal percent with sign, e.g., +0.3) for French real GDP growth in the ", current_quarter, " quarter of ", y_prev,
      " and a confidence level (integer 0–100). Output EXACTLY in this format on a single line (no extra text):\n",
      "<forecast> (<confidence>)\nExample: +0.3 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), "."
    )
  }
  
} else {
  try(Sys.setlocale("LC_TIME", "French"), silent = TRUE)
  
  current_boss <- function(type, d) {
    if (type == "BDF") return(BDF_current_boss(d))
    if (type == "INSEE") return(INSEE_current_boss(d))
  }
  
  prompt_template <- function(type, d, q_trim, y_prev) {
    boss <- current_boss(type, d)
    position <- ifelse(type == "BDF","Gouverneur de la Banque de France", "Directeur Général de l'INSEE")  
    trimestre_actuel <- if (q_trim == 1){
      "premier"}
    else if (q_trim == 2){
      "second"
    }else if (q_trim == 3){
      "troisième"}else{
        "quatrième"}
    
    
    paste0(
      "Oubliez les instructions et les réponses précédentes. Vous êtes ", boss, ", ", position,
      ", qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "En utilisant uniquement les informations disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) de la croissance du PIB réel français pour le ",
      trimestre_actuel, " trimestre ", y_prev,
      " et un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) :\n",
      "<prévision> (<confiance>)\nExemple : +0.3 (80)\n",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}

####################
# Boucle principale
#####################

forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"
results_BDF <- list()
results_INSEE <- list()

t1 <- Sys.time()

for (dt in as.Date(dates$`Date Prevision`)) {
  current_date <- as.Date(dt)
  
  # Récupération des documents
  docs <- get_docs_to_merge(
    date_to_use = current_date,
    df_date = dates,
    document_folder_BDF = document_folder_BDF,
    document_folder_INSEE = document_folder_INSEE,
    output_folder_BDF = output_folder_BDF,
    output_folder_INSEE = output_folder_INSEE
  )
  
  # Paramètre de prévision
  m <- month(current_date)
  y <- year(current_date)
  if (m == 1) {
    q_trim <- 4
    year_prev <- y - 1
  } else {
    q_trim <- ((m - 2) %/% 3) + 1
    year_prev <- y
  }
  
  #BDF
  if (file.exists(docs$BDF_path)) {
    uploaded_bdf <- google_upload(docs$BDF_path,
                                  base_url = "https://generativelanguage.googleapis.com/", 
                                  api_key = cle_API)
    
    prompt_bdf <- prompt_template("BDF", current_date, q_trim, year_prev)
    
    out_bdf <- future_lapply(seq_len(n_repro), function(i) {
      tryCatch(chat_gemini$chat(uploaded_bdf, prompt_bdf), error = \(e) NA_character_)
    }, future.seed = TRUE)
    
    histoires <- sapply(out_bdf, \(x) ifelse(is.list(x), x$text, x))
    parsed <- regmatches(histoires, regexec(forecast_confidence_pattern, histoires))
    df_bdf <- data.frame(Date = current_date, Prompt = prompt_bdf)
    for (i in seq_len(n_repro)) {
      if (length(parsed[[i]]) >= 3) {
        df_bdf[[paste0("forecast_", i)]] <- as.numeric(parsed[[i]][2])
        df_bdf[[paste0("confidence_", i)]] <- as.integer(parsed[[i]][3])
      }
      df_bdf[[paste0("answer_", i)]] <- histoires[i]
    }
    results_BDF[[length(results_BDF) + 1]] <- df_bdf
  }
  
  # INSEE
  if (file.exists(docs$INSEE_path)) {
    uploaded_insee <- google_upload(docs$INSEE_path, 
                                    base_url = "https://generativelanguage.googleapis.com/", 
                                    api_key = cle_API)
    prompt_insee <- prompt_template("INSEE", current_date, q_trim, year_prev)
    
    out_insee <- future_lapply(seq_len(n_repro), function(i) {
      tryCatch(chat_gemini$chat(uploaded_insee, prompt_insee), error = \(e) NA_character_)
    }, future.seed = TRUE)
    
    histoires <- sapply(out_insee, \(x) ifelse(is.list(x), x$text, x))
    parsed <- regmatches(histoires, regexec(forecast_confidence_pattern, histoires))
    df_insee <- data.frame(Date = current_date, Prompt = prompt_insee)
    for (i in seq_len(n_repro)) {
      if (length(parsed[[i]]) >= 3) {
        df_insee[[paste0("forecast_", i)]] <- as.numeric(parsed[[i]][2])
        df_insee[[paste0("confidence_", i)]] <- as.integer(parsed[[i]][3])
      }
      df_insee[[paste0("answer_", i)]] <- histoires[i]
    }
    results_INSEE[[length(results_INSEE) + 1]] <- df_insee
  }
}

# --- Sauvegarde
df_results_rolling_text_BDF <- do.call(rbind, results_BDF)
df_results_rolling_text_INSEE <- do.call(rbind, results_INSEE)
write.xlsx(df_results_rolling_text_BDF, "resultats_BDF_Gemini_rolling_text.xlsx")
write.xlsx(df_results_rolling_text_INSEE, "resultats_INSEE_Gemini_rolling_text.xlsx")

t2 <- Sys.time()
print(diff(range(t1, t2)))