
#HARMONISER NOMS DE FICHIERS INSEE
#A NOTER : certains fichiers ne sont pas reconnus et ont été modifiés manuellement



######
#Packages
########
library(stringr)
library(stringi)
library(dplyr)
library(fs) 
library(lubridate)

###################
# Fct utilitaires 
#################


expand_two_digit_year <- function(yy2) {
  yy <- as.integer(yy2)
  if (is.na(yy)) return(NA_integer_)
  if (yy <= 25) return(2000 + yy)
  return(1900 + yy)
}

# Validation mois/année
is_valid_month_year <- function(mon, yr, min_year = 2007, max_year = 2025) {
  !is.na(mon) && !is.na(yr) &&
    mon >= 1 && mon <= 12 &&
    yr >= min_year && yr <= max_year
}

# Mapping des noms de mois français -> numéro
month_name_to_num <- function(nm) {
  nm <- tolower(nm)
  nm <- stri_trans_general(nm, "Latin-ASCII") # retire accents
  nm <- str_trim(nm)
  mapping <- list(
    "janvier"=1, "janv"=1, "jan"=1,
    "fevrier"=2, "fev"=2,
    "mars"=3, "mar"=3,
    "avril"=4, "avr"=4,
    "mai"=5,
    "juin"=6,
    "juillet"=7, "juil"=7,
    "aout"=8, "aou"=8,
    "septembre"=9, "sept"=9, "sep"=9,
    "octobre"=10, "oct"=10,
    "novembre"=11, "nov"=11,
    "decembre"=12, "dec"=12
  )
  if (nm %in% names(mapping)) return(mapping[[nm]])
  return(NA_integer_)
}

# Détection du type de fichier

detect_type <- function(s) {
  # s doit être la chaîne déjà normalisée (minuscules, accents en ascii, underscores)
  txt <- tolower(s)
  txt <- stringi::stri_trans_general(txt, "Latin-ASCII")
  
 
  tokens_emi <- c(
    "emi", "industrie", "industriel", "industriels", "indus",
    "manufactur", "manufacturing", "fabrication", "fabrications",
    "industrie_du", "industrie-",
    "industrial", "industries", "industri"
  )
  
  tokens_bat <- c(
    "bat", "batiment", "batimentaire", "batiments", "bati",
    "construction", "construc", "immeuble", "immeubles", "batic",
    "bâtiment", "bâtiments" # accents déjà retirés mais safe to keep
  )
  
  tokens_ser <- c(
    "ser", "service", "services", "serv", "servic",
    "tertiaire", "commerce", "commer", "restaur", "restauration",
    "hotel", "hotellerie", "transport", "transports", "activite", "activité"
  )
  
 
  pattern_for <- function(tokens) {
    # use non-capturing group for tokens; anchor with start or underscore and end or underscore
    paste0("(^|_)(?:", paste(tokens, collapse = "|"), ")(_|$)")
  }
  
  # check in order (EMI first, BAT, SER)
  if (stringr::str_detect(txt, pattern_for(tokens_emi))) return("EMI")
  if (stringr::str_detect(txt, pattern_for(tokens_bat))) return("BAT")
  if (stringr::str_detect(txt, pattern_for(tokens_ser))) return("SER")
  
  return("UNK")
}


###################
#Extracton type et date
###################
extract_type_and_date <- function(filename) {
  base <- tolower(basename(filename))
  base_noext <- str_remove(base, "\\.pdf$")
  
  # Normalisation
  s <- stri_trans_general(base_noext, "Latin-ASCII")
  s <- str_replace_all(s, "\\bir\\d{1,}\\b", "")    # supprime IRxx
  s <- str_replace_all(s, "[^a-z0-9]", "_")         # remplace non-alnum
  s <- str_replace_all(s, "_+", "_")                # compresse underscores
  s <- str_remove_all(s, "^_+|_+$")                 # trim underscores
  
  type <- detect_type(s)
  
  return_if_valid <- function(mon, yr) {
    if (is_valid_month_year(mon, yr)) {
      return(list(
        type = ifelse(is.na(type), "UNK", type),
        month = sprintf("%02d", as.integer(mon)),
        year = as.integer(yr),
        formatted = paste0(ifelse(is.na(type), "UNK", type),
                           "_", sprintf("%02d", as.integer(mon)), "_", yr)
      ))
    } else {
      return(NULL)
    }
  }
  
  # YYYYMM
  m <- str_match(s, "(20\\d{2})(0[1-9]|1[0-2])")
  if (!all(is.na(m))) {
    res <- return_if_valid(as.integer(m[1,3]), as.integer(m[1,2]))
    if (!is.null(res)) return(res)
  }
  
  # YYYY_MM ou YYYY-MM
  m <- str_match(s, "(20\\d{2})[_-](0[1-9]|1[0-2])")
  if (!all(is.na(m))) {
    res <- return_if_valid(as.integer(m[1,3]), as.integer(m[1,2]))
    if (!is.null(res)) return(res)
  }
  
  # 6 chiffres 
  m6 <- str_match(s, "(\\d{6})")
  if (!all(is.na(m6))) {
    s6 <- m6[1,2]
    # YYYYMM
    ya <- as.integer(substr(s6, 1, 4)); ma <- as.integer(substr(s6, 5, 6))
    if (!is.na(ya) && !is.na(ma) && is_valid_month_year(ma, ya)) {
      return(return_if_valid(ma, ya))
    }
    # MMYY
    mb <- as.integer(substr(s6, 3, 4)); yb <- expand_two_digit_year(substr(s6, 5, 6))
    if (!is.na(mb) && !is.na(yb) && is_valid_month_year(mb, yb)) {
      return(return_if_valid(mb, yb))
    }
    # MMYYYY
    mc <- as.integer(substr(s6, 1, 2)); yc <- as.integer(substr(s6, 3, 6))
    if (!is.na(mc) && !is.na(yc) && is_valid_month_year(mc, yc)) {
      return(return_if_valid(mc, yc))
    }
  }
  
  # MMYYYY ou MM_YYYY
  m <- str_match(s, "(0[1-9]|1[0-2])[_-]?(20\\d{2})")
  if (!all(is.na(m))) {
    res <- return_if_valid(as.integer(m[1,2]), as.integer(m[1,3]))
    if (!is.null(res)) return(res)
  }
  
  #  Nom du mois (FR) + année
  months_alt <- c("janv","jan","fev","fevrier","mar","mars","avr","avril","mai",
                  "juin","juil","juillet","aou","aout","sep","sept","oct","nov","dec","decembre")
  months_regex <- paste0("(", paste(months_alt, collapse = "|"), ")")
  m <- str_match(s, paste0(months_regex, "[_-]?([0-9]{2,4})"))
  if (!all(is.na(m))) {
    mon_num <- month_name_to_num(m[1,2])
    year_str <- m[1,3]
    yr <- ifelse(nchar(year_str) == 2,
                 expand_two_digit_year(year_str),
                 as.integer(year_str))
    if (!is.na(mon_num) && !is.na(yr) && is_valid_month_year(mon_num, yr)) {
      return(return_if_valid(mon_num, yr))
    }
  }
  
  #  Nom du mois suivi directement de YY
  m <- str_match(s, paste0(months_regex, "([0-9]{2})\\b"))
  if (!all(is.na(m))) {
    mon_num <- month_name_to_num(m[1,2])
    yr <- expand_two_digit_year(m[1,3])
    if (!is.na(mon_num) && !is.na(yr) && is_valid_month_year(mon_num, yr)) {
      return(return_if_valid(mon_num, yr))
    }
  }
  
  # Fallback
  return(list(type = ifelse(is.na(type), "UNK", type),
              month = NA_character_,
              year = NA_integer_,
              formatted = "UNK"))
}

###################
#Fonction pour renommer
###################
harmonize_insee_filenames <- function(dir, dry_run = TRUE, pattern = "\\.pdf$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  results <- data.frame(original = character(0),
                        proposed = character(0),
                        status = character(0),
                        stringsAsFactors = FALSE)
  
  for (f in files) {
    parsed <- extract_type_and_date(f)
    base <- basename(f)
    if (!is.na(parsed$month) && !is.na(parsed$year) && parsed$formatted != "UNK") {
      newname <- paste0(parsed$type, "_", parsed$month, "_", parsed$year, ".pdf")
      newpath <- file.path(dirname(f), newname)
      results <- bind_rows(results,
                           data.frame(original = base,
                                      proposed = newname,
                                      status = ifelse(dry_run, "DRY_RUN", "WILL_RENAME"),
                                      stringsAsFactors = FALSE))
      if (!dry_run) {
        if (file.exists(newpath)) {
          newpath <- file.path(dirname(f),
                               paste0(tools::file_path_sans_ext(newname),
                                      "_dup_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"))
        }
        file.rename(f, newpath)
      }
    } else {
      results <- bind_rows(results,
                           data.frame(original = base,
                                      proposed = NA_character_,
                                      status = "SKIPPED_UNK",
                                      stringsAsFactors = FALSE))
    }
  }
  return(results)
}

##################################
#Verif puis renommer les fichiers 
##################################
 res_preview <- harmonize_insee_filenames("C:/Users/LEDA/Documents/INSEE_Scrap", dry_run = TRUE)
# print(res_preview)
harmonize_insee_filenames(dir = "C:/Users/LEDA/Documents/INSEE_Scrap", dry_run = FALSE)


#DERNIERS FICHIERS MODIFIES A LA MAIN

####################
#VERIF FICHIERS MANQUANTS
###############################

path <- "C:/Users/LEDA/Documents/INSEE_Scrap"
files <- list.files(path, pattern = "\\.pdf$", full.names = FALSE)

# Extraire date depuis le nom
df <- data.frame(file = files, stringsAsFactors = FALSE) |>
  mutate(
    name  = str_remove(file, "\\..*$"),
    type  = str_extract(name, "^[A-Z]{3}"),                # SER / EMI / BAT
    month = as.integer(str_extract(name, "(?<=_)[0-9]{2}(?=_)")),
    year  = as.integer(str_extract(name, "[0-9]{4}$"))
  ) |>
  filter(!is.na(type) & !is.na(month) & !is.na(year))

# Paramètre : combien de fichiers attendus par type et par mois
expected_files_per_type = 1  

# Créer toutes les combinaisons possibles
all_combinations <- expand.grid(
  type = c("EMI","SER","BAT"),
  year = unique(df$year),
  month = 1:12
)

# Compter fichiers existants
count_nb <- df |>
  group_by(type, year, month) |>
  summarise(n_files = n(), .groups = "drop")

# Joindre avec toutes les combinaisons pour détecter manquants
liste_entiere <- all_combinations|>
  left_join(count_df, by = c("type","year","month")) |>
  mutate(
    n_files = ifelse(is.na(n_files), 0, n_files),
    expected = expected_files_per_type,
    status = ifelse(n_files >= expected, "OK", "MISSING")
  ) |>
  arrange(year, month, type)

# Afficher
liste_entiere |>
  select(year, month, type, n_files, expected, status) |>
  print()

liste_manquant <- liste_entiere |>
  filter(status == "MISSING", !(year == 2025 & month >= 10)) |>
  select(type, year, month) 

print(liste_manquant)
#41 fichiers manquant*

# En commençant en 2010
liste_manquant2 <- liste_entiere |>
  filter(status == "MISSING", !(year == 2025 & month >= 10), year > 2009) |>
  select(type, year, month) 
#15 fichiers manquants : 10 Batiment, 3 Service, 2 EMI
