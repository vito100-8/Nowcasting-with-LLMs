# BENCHMARK : MODELE ISMA



rm(list = ls())
library()

data_pib <- read_xlsx("Data_PIB_ENQ.xlsx", sheet = "PIB")
data_enq <- read_xlsx("Data_PIB_ENQ.xlsx", sheet = "ENQ_BDF")

data_pib <- data_pib |>
  select(dates, PIB, PIB_PR)

#####################
#MODELE ECONOMETRIQUE
######################

EVLIV <- data_enq$EVLIV
PREVPRO <- data_enq$PREVPRO
PIB <- data_pib$PIB

#ou 
data_enq <- data_enq |>
  mutate(dates = `...1`, .keep = "all") |> 
  select(!...1)

EVLIV <- data_enq |>
  select(dates, EVLIV)
PREVPRO <- data_enq |>
  select(dates, PREVPRO)
PIB <- data_pib |>
  select(dates, PIB)


#Convertir en date 
data_enq <- data_enq |>
  mutate(year = year(dates),
         month = month(dates),
         quarter = quarter(dates))


#Date que l'on souhaite prévoir 
dates <- as.Date(c("2023-03-15", "2023-06-15"))

########################
# FONCTION NOWCAST
######################
ggplot(data_pib, aes(x = dates, y = PIB)) + 
  geom_line()

#a noter : fevrier-avril T1 mai_juillet T2 aout octobre T3 novembre janvier  T4 >>> EMC 1 est en février de l'année


#Pas de variations au sein d'une même date : RMSE et MAE varie seulement lorsque 'lon prend plusieurs date #  prédictions des LLM qui varie entre plusieurs itérations

















