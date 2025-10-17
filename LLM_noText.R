# ReadMe
# Prévision récursive de croissance du PIB avec LLM sans document

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
df_date <- as.Date(c("2012-01-03")) #POUR TESTER : à changer manuellement

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

#################
# BOUCLE PRINCIPALE
##################

results_all <- list()
row_id <- 1

t1 <- Sys.time()

for (dt in df_date) {
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




######################################
#CAS BENCHMARK : économique expert et non pas qql en particulier