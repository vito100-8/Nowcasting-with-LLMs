#INSERTION D'UN EXCEL ITERATIF SELON LA PERIODE DE PREVISION AU LLM

#donner csv/xlsx en input (xls_write) : recréer un fichier excel/csv  à chaque itération car nouvelle date

df_PIB <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")
df_ISMA <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "mensuel ISMA")
df_enq_BDF <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_BDF")
df_enq_INSEE <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_INSEE")

# CODE POUR ORDONNER LES DONNEES INSEE

sub_indicators <- unlist(df_enq_INSEE[1, -1])
main_headers_raw <- colnames(df_enq_INSEE)[-1] # Exclure la colonne 'Date'

clean_industry_name <- function(col_name) {
  if (str_detect(col_name, "Ind manuf")) {
    return("Industrie")
  } else if (str_detect(col_name, "Bâtiment")) {
    return("Bâtiment")
  } else if (str_detect(col_name, "Services")) {
    return("Services")
  } else {
    return(NA_character_)
  }
}

new_data_col_names <- map2_chr(main_headers_raw, sub_indicators, ~ {
  industry_type <- clean_industry_name(.x)
  sub_indicator_name <- .y
  paste(industry_type, sub_indicator_name, sep = "_")
})


new_colnames <- c("Date", new_data_col_names)


df_enq_INSEE_cleaned <- df_enq_INSEE[-1, ] # Supprime la première ligne (les sous-indicateurs)
colnames(df_enq_INSEE_cleaned) <- new_colnames

df_enq_INSEE_cleaned <- df_enq_INSEE_cleaned %>%
  # Convertir toutes les autres colonnes (les valeurs d'indicateurs) en numérique
  mutate(across(-Date, as.numeric))


df_enq_INSEE_long <- df_enq_INSEE_cleaned %>%
  pivot_longer(
    cols = -Date, # Toutes les colonnes sauf 'Date'
    names_to = "Industry_SubIndicator", # Le nouveau nom de colonne pour les noms combinés
    values_to = "Value" # Le nouveau nom de colonne pour les valeurs
  )

df_enq_INSEE_final <- df_enq_INSEE_long %>%
  separate(
    col = Industry_SubIndicator,
    into = c("Industry_type", "Sub_indicator"),
    sep = "_",
    extra = "merge" 
  ) %>%

  select(Date, Industry_type, Sub_indicator, Value)


#CODE POUR ORDONNER LES DONNES BDF

#Essayer donner csv en input (xls_write) : recréer un fichier excel/csv  à chaque itération car nouvelle date
##### Mettre les anciennes prévisions des enquêtes (EMC) , réflechir comment faire pour les prev de l'INSEE
#### VS erreur du LLM


#EI = M - 1 (si prévision en janvier on prend jusqu'en décembre)