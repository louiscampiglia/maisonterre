library(dplyr)

# Grouper les données par heure (en ignorant les minutes et les secondes) et calculer la moyenne de la colonne valeur
convecteurs <- convecteurs %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Puissance_Convecteurs = mean(Puissance_Convecteurs, na.rm = TRUE))

energie2 <- energie %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Energie_Eclairage = mean(Energie_Eclairage, na.rm = TRUE),
            Energie_VMC = mean(Energie_VMC,na.rm = TRUE),
            Energie_Prises = mean(Energie_Prises,na.rm = TRUE),
            Energie_Convecteurs = mean(Energie_Convecteurs,na.rm = TRUE))

humidite2 <- humidite %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Humidite_Interieur_Sud.mean = mean(Humidite_Interieur_Sud.mean, na.rm = TRUE),
            Humidite_Interieur_Nord.mean = mean(Humidite_Interieur_Nord.mean,na.rm = TRUE),
            Humidite_RELATIVE_E4000.mean = mean(Humidite_RELATIVE_E4000.mean,na.rm = TRUE),
            Humidite_ABSOLUE_E4000.mean = mean(Humidite_ABSOLUE_E4000.mean,na.rm = TRUE))

mur_12 <- mur_1 %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Mur_Temp_1_1.mean = mean(Mur_Temp_1_1.mean, na.rm = TRUE),
            Mur_Temp_1_2.mean = mean(Mur_Temp_1_2.mean,na.rm = TRUE),
            Mur_Temp_1_3.mean = mean(Mur_Temp_1_3.mean ,na.rm = TRUE))

mur_22 <- mur_2 %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Mur_Temp_2_2.mean = mean(Mur_Temp_2_2.mean, na.rm = TRUE),
            Mur_Temp_2_3.mean = mean(Mur_Temp_2_3.mean,na.rm = TRUE))

mur_sud2 <- mur_sud %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Mur_Temp_sud_1.mean = mean(Mur_Temp_sud_1.mean, na.rm = TRUE),
            Mur_Temp_sud_2.mean = mean( Mur_Temp_sud_2.mean,na.rm = TRUE),
            Mur_Temp_sud_3.mean = mean(Mur_Temp_sud_3.mean,na.rm = TRUE))

temperature2 <- temperature %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Temperature_Interieur_Sud.mean = mean(Temperature_Interieur_Sud.mean, na.rm = TRUE),
            Temperature_Interieur_Nord.mean = mean(Temperature_Interieur_Nord.mean,na.rm = TRUE),
            Temperature_E4000.mean = mean(Temperature_E4000.mean,na.rm = TRUE),
            Temperature_ressentie_E4000.mean = mean(Temperature_ressentie_E4000.mean, na.rm = TRUE),
            Station_Meteo_Text = mean(Station_Meteo_Text, na.rm = TRUE),
            Mur_Temp_1_1.mean = mean(Mur_Temp_1_1.mean, na.rm = TRUE))

toit2 <- toit %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Mur_Temp_toit_1.mean = mean(Mur_Temp_toit_1.mean, na.rm = TRUE),
            Mur_Temp_toit_2.mean = mean(Mur_Temp_toit_2.mean,na.rm = TRUE),
            Mur_Temp_toit_3.mean = mean(Mur_Temp_toit_3.mean, na.rm = TRUE))

pyranometres2 <- pyranometres_corr %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(Pyranometre_Est_Wm.mean = mean(Pyranometre_Est_Wm.mean , na.rm = TRUE),
            Pyranometre_Nord_Wm.mean  = mean(Pyranometre_Nord_Wm.mean,na.rm = TRUE),
            Pyranometre_Ouest_Wm.mean  = mean(Pyranometre_Ouest_Wm.mean, na.rm = TRUE),
            Pyranometre_Sud_Wm.mean  = mean(Pyranometre_Sud_Wm.mean, na.rm = TRUE))

co22 <- co2 %>%
  group_by(Time = format(Time, "%Y-%m-%d-%H")) %>%
  summarise(CO2_Interieur_Sud.mean = mean(CO2_Interieur_Sud.mean, na.rm = TRUE),
            CO2_Interieur_Nord.mean = mean(CO2_Interieur_Nord.mean, na.rm = TRUE),
            CO2_E4000.mean = mean(CO2_E4000.mean, na.rm = TRUE),
            `COV_E4000.mean  x100` = mean(`COV_E4000.mean  x100`, na.rm = TRUE))
