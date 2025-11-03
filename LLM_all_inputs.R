#Script : Requêtes LLM (Gemini) avec PDF, TS et recherches

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


######################################
#Données utilisées
######################################

#Téléchargement données

df_PIB <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")
df_enq_BDF <- read.xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_BDF")
df_enq_INSEE <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_INSEE")


#Nettoyage de df_enq_INSEE

df_enq_INSEE <- df_enq_INSEE |>
  mutate(
    dates = str_replace_all(dates, 
                            c("janv\\." = "jan", "févr\\." = "feb", "mars" = "mar",
                              "avr\\." = "apr", "mai" = "may", "juin" = "jun",
                              "juil\\." = "jul", "août" = "aug", "sept\\." = "sep",
                              "oct\\." = "oct", "nov\\." = "nov", "déc\\." = "dec"))
  ) |>
  mutate(dates_temp = as.Date(parse_date_time(dates, orders = "b Y"), origin = "1970-01-01")) 


v_dates <- rollforward(df_enq_INSEE$dates_temp)
df_enq_INSEE <- df_enq_INSEE |>
  mutate(dates = v_dates) |>
  select(!dates_temp)


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
      ", giving a speech about the economic outlook of France. Today is ",
      format(d, "%d %B %Y"), ". ",
      "You will be provided with a document with information about the current state and recent past of the French economy. ",
      "Using only the information in that document and information that was available on or before ", format(d, "%d %B %Y"),
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
      "Vous recevrez un document concernant la situation actuelle et passée de l'économie française. ",
      "En utilisant UNIQUEMENT les informations contenues dans ce document et celles disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) de la croissance du PIB réel français pour le ",
      trimestre_actuel, " trimestre ", y_prev,
      " et un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) :\n",
      "<prévision> (<confiance>)\nExemple : +0.3 (80)\n",
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
for (dt in as.Date(dates$`Date Prevision`)) {
  current_date <- as.Date(dt) 
  
  df_enq_BDF_filtered <- df_enq_BDF |>
    filter(dates < current_date)
  
  # Transformer les données filtrées en texte Markdown lisible
  bdf_data_markdown <- df_to_markdown_table(df_enq_BDF_filtered, title = "Banque de France Survey Data")
  contents_bdf_data <- ContentText(bdf_data_markdown)
  
  # Trouver le bon pdf et son path
  docname <- get_next_doc(current_date)
  pdf_path <- path_from_docname(docname, folder = document_folder_BDF)
  
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
  prompt_text <- prompt_template("BDF", current_date, trimestre_index ,
                                     year_prev)
 
  # appel à Gemini en intégrant le document voulu
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- chat_gemini$chat(uploaded_doc, prompt_text, contents_bdf_data)
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

for (dt in as.Date(dates$`Date Prevision`)) {
  current_date <- as.Date(dt) 
  
  df_enq_INSEE_filtered <- df_enq_INSEE |> filter(dates < current_date)
  
  #Données en texte
  insee_data_markdown <- df_to_markdown_table(df_enq_INSEE_filtered, title = "INSEE Survey Data")
  contents_INSEE_data <- ContentText(insee_data_markdown)
  
  # Trouver les bons pdf, le chemin d'accès et les concaténer
  emi_path <- get_last_insee_docs_by_type(current_date,"EMI",  document_folder_INSEE)
  ser_path <- get_last_insee_docs_by_type(current_date, "SER",document_folder_INSEE)
  bat_path <- get_last_insee_docs_by_type(current_date, "BAT",document_folder_INSEE)
  
  ##concaténation des documents dans le chemin d'accès spécifié
  all_insee_docs_to_combine <- c(emi_path, ser_path, bat_path)
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
  prompt_text <- prompt_template("INSEE", current_date, trimestre_index ,
                                 year_prev)
  
  # appel à Gemini en intégrant le document voulu
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- chat_gemini$chat(uploaded_doc, prompt_text, contents_INSEE_data)
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






