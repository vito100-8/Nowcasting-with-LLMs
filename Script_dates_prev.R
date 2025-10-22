#Création d'un xlsx avec toutes les dates de prévision


#### CHARGER DATES EMC POUR LA BDF #######
## charger table de dates
date_prev_temp_BDF <- read_excel("Synthese_fileEMC.xlsx")
colnames(date_prev_temp_BDF)[1:4] <- c("fichier", "date_courte", "date_longue", "trimestre")

# Reformater les noms de fichiers : EMC_MM_YYYY → YYYY_MM_EMC
date_prev_temp_BDF <- date_prev_temp_BDF |>
  mutate(
    fichier = str_replace(fichier, "^EMC_(\\d{1,2})_(\\d{4})$", "\\2_\\1_EMC")
  ) |>
  # Forcer les mois à deux chiffres
  mutate(
    fichier = str_replace(fichier, "^(\\d{4})_(\\d{1})_", "\\1_0\\2_")
  )

# Extraire année et mois + filtrer les années pertinentes
date_prev_temp_BDF <- date_prev_temp_BDF |>
  mutate(
    annee_prev = as.numeric(str_extract(fichier, "^\\d{4}")),
    mois_prev  = as.numeric(str_extract(fichier, "(?<=\\d{4}_)\\d{2}"))
  ) |>
  filter(annee_prev >= 2015)

# Créer les variables de dates
date_prev_temp_BDF <- date_prev_temp_BDF |>
  mutate(
    date_courte_d = as.Date(as.character(date_courte)),
    date_longue_d = case_when(
      is.na(date_longue) ~ as.Date(NA),
      suppressWarnings(!is.na(as.numeric(date_longue))) ~ as.Date(as.numeric(date_longue), origin = "1899-12-30"),
      TRUE ~ as.Date(NA)
    ),
    date_finale_d = case_when(
      annee_prev >= 2015 & annee_prev <= 2019 ~ date_courte_d,
      annee_prev >= 2020 & annee_prev <= 2024 ~ date_longue_d
    )
  ) 

date_publi_prev<- date_prev_temp_BDF |>
  select(fichier, trimestre, date_finale_d) |>
  filter(!is.na(date_finale_d)) 


# on supprime les lignes où la variable date_finale_d est manquante (NA) 
# car pas de publication de l'enquête (pandémie)

#Correction d'une date d'EMC
date_publi_prev$date_finale_d[107] <- as.Date("2024-07-10")

write.xlsx(date_publi_prev, file = here("date_publi_prev.xlsx"), )


### Initialisation de la date à la veille de la parution de chaque EMC ##

df_date <- date_publi_prev|>
  mutate(`Date Prevision` = date_finale_d - 1,
         .keep = "none")


write.xlsx(df_date, file = here("dates_prev.xlsx"))


