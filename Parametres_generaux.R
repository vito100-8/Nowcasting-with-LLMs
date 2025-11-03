# SCRIPT PARAMETRES GENERAUX

##############################
# Répertoire de travail actif
#############################

setwd(dirname(getActiveDocumentContext()$path))

here::i_am("LLM_noText.R")

load_dot_env('.env')

###################################################
#INITIALISATION PARAMETRES
###################################################


#Paramètres généraux
english <- 1 # 1 si prompt en anglais
temp_LLM <- 0.7  # Niveau de créativité des réponses 0.3/0.7/1.5 (castro-Leibovici)
n_repro <- 2  # Nombre de prévisions générées par date


# Initialisation des dates
#df_date <- as.Date(c("2012-01-03")) #POUR TESTER : à changer manuellement

#Dates utilisées
dates <- read_xlsx(here("dates_prev.xlsx"))

# API Key (pour ellmer on utilise API_KEY_GEMINI)
cle_API <- Sys.getenv("API_KEY_GEMINI")


