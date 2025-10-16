rm(list=ls())
source("LLM_functions.R")

# Répertoire de travail actif

setwd(dirname(getActiveDocumentContext()$path))

here::i_am("LLM_noText.R")

load_dot_env('.env')

###################################################
#INITIALISATION PARAMETRES
###################################################


#Paramètres généraux
english <- 1 # 1 si prompt en anglais
temp_LLM <- 0.7  # Niveau de créativité des réponses 0.3/0.7/1.5 (castro-Leibovici)
n_repro <- 2  # Nombre de prévisions générées par date*
sys_prompt <- ifelse(english == 1,
                     "You will act as the economic agent you are told to be. Answer based on your knowledge and researches in less than 200 words, do not invent facts." ,
                     "Vous allez incarner des agents économiques spécifiés. Répondez aux questions en moins de 200 mots à l'aide de vos connaissances et de vos recherches, n'inventez pas de faits.")


# Initialisation des dates
dates <- as.Date(c("2012-01-03")) #à changer manuellement
dates <- df_date #utiliser le même df que dans Text (nécessaire de l'avoir déclaré avant)

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


#####################
# QUESTIONS A POSER
#####################


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
      "Forget previous instructions and previous answers. You are ", boss, ", ", position, 
      ", and you are giving a speech about the economic outlook of France. Today is ",
      format(d, "%d %B %Y"), ". ",
      "Using only information available on or before ", format(d, "%d %B %Y"),
      ", provide a numeric forecast (decimal percent with sign, e.g. +0.3) for French real GDP growth for the ", current_quarter, " quarter of ", y_prev,
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

#################
# BOUCLE PRINCIPALE
##################

results_all <- list()
row_id <- 1

t1 <- Sys.time()

for (dt in dates) {
  current_date <- as.Date(dt)
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current

  for (type in c("BDF", "INSEE")) {
    
    q_text <- prompt_template(type, current_date, trimestre_index, year_prev)
    
    outs <- future_lapply(seq_len(n_repro), function(i) {
      tryCatch({
        resp <- chat_gemini$chat(q_text)
        return(resp)
      }, error = function(e) {
        message("API error: ", conditionMessage(e))
        return(NA_character_)
      })
    }, future.seed = TRUE)
    
    textes <- unlist(outs)
    parsed <- lapply(textes, function(txt) {
      m <- regmatches(txt, regexec("([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)", txt))
      if (length(m[[1]]) >= 3) {
        list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt)
      } else {
        list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
      }
    })
    
    df_tmp <- data.frame(
      Date = as.character(current_date),
      Type = type,
      Prompt = q_text,
      stringsAsFactors = FALSE
    )
    
    for (i in seq_len(n_repro)) {
      df_tmp[[paste0("forecast_", i)]]  <- parsed[[i]]$forecast
      df_tmp[[paste0("confidence_", i)]] <- parsed[[i]]$confidence
      df_tmp[[paste0("answer_", i)]]     <- parsed[[i]]$raw
    }
    
    results_all[[row_id]] <- df_tmp
    row_id <- row_id + 1
    Sys.sleep(0.5)
  }
}

# Combine final dataframe
df_results <- do.call(rbind, results_all)

# Sauvegarde unique
nom_fichier <- paste0("resultats_noText.xlsx")
write.xlsx(df_results, file = nom_fichier, sheetName = 'prevision', rowNames = FALSE)

cat("Les résultats (BDF et INSEE) ont été sauvegardés dans le fichier :", nom_fichier, "\n")

t2 <- Sys.time()
diff(range(t1,t2))
