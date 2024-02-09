
library(ggplot2)
library(dplyr)
library(zoo)

#COMMENT GERER LES NA?


#Methode d'interpolation temporelle si NA aléatoires.
#mputation par la moyenne (Pour de NA en début ou fin de colonne)
#Imputation par regression pour bcp de données manquantes à la suite. 
#Ou suppression si trop de données manquantes à la suite.


#MUR SUD
# Interpolation pour les valeurs manquantes au milieu de la série temporelle
mur_sud$Mur_Temp_sud_1.mean <- na.approx(mur_sud$Mur_Temp_sud_1.mean, na.rm = FALSE)
mur_sud$Mur_Temp_sud_2.mean <- na.approx(mur_sud$Mur_Temp_sud_2.mean, na.rm = FALSE)
mur_sud$Mur_Temp_sud_3.mean <- na.approx(mur_sud$Mur_Temp_sud_3.mean, na.rm = FALSE)
#Problème il y a bcp de NA au début (environ 95% des lignes)
# On supprime les NA
# Suppression des lignes en fonction des indices spécifiques
indices_a_supprimer <- c(1:504286)
mur_sud <- mur_sud %>%
  slice(-indices_a_supprimer)
#mur_Sud ne contient plus de NA


#MUR 1
#On a un faible nombre de valeurs de température intérieure absurdes (de -20°C à -250°C), on les rajoute aux NA
mur_1$Mur_Temp_1_1.mean[mur_1$Mur_Temp_1_1.mean<0]<-NA
mur_1$Mur_Temp_1_2.mean[mur_1$Mur_Temp_1_2.mean<0]<-NA
# Interpolation pour les valeurs manquantes au milieu de la série temporelle
mur_1$Mur_Temp_1_1.mean <- na.approx(mur_1$Mur_Temp_1_1.mean, na.rm = FALSE)
mur_1$Mur_Temp_1_2.mean <- na.approx(mur_1$Mur_Temp_1_2.mean, na.rm = FALSE)
mur_1$Mur_Temp_1_3.mean <- na.approx(mur_1$Mur_Temp_1_3.mean, na.rm = FALSE)
#Imputation par la moyenne pour les NA (6% + 2%)
mur_1$Mur_Temp_1_1.mean[1:33089] <- mean(mur_1$Mur_Temp_1_1.mean, na.rm = TRUE)
mur_1$Mur_Temp_1_2.mean[1:33089] <- mean(mur_1$Mur_Temp_1_2.mean, na.rm = TRUE)
mur_1$Mur_Temp_1_3.mean[1:33089] <- mean(mur_1$Mur_Temp_1_3.mean, na.rm = TRUE)
mur_1$Mur_Temp_1_1.mean[48004:59322] <- mean(mur_1$Mur_Temp_1_1.mean, na.rm = TRUE)
mur_1$Mur_Temp_1_2.mean[48004:59322] <- mean(mur_1$Mur_Temp_1_2.mean, na.rm = TRUE)
mur_1$Mur_Temp_1_3.mean[48004:59322] <- mean(mur_1$Mur_Temp_1_3.mean, na.rm = TRUE)
#mur_1 ne contient plus de NA.

#MUR 2
# Interpolation pour les valeurs manquantes au milieu de la série temporelle
mur_2$Mur_Temp_2_2.mean <- na.approx(mur_2$Mur_Temp_2_2.mean, na.rm = FALSE)
mur_2$Mur_Temp_2_3.mean <- na.approx(mur_2$Mur_Temp_2_3.mean, na.rm = FALSE)
#Imputation par la moyenne pour les NA (6%)
mur_2$Mur_Temp_2_2.mean[1:33095] <- mean(mur_2$Mur_Temp_2_2.mean, na.rm = TRUE)
mur_2$Mur_Temp_2_3.mean[1:33095] <- mean(mur_2$Mur_Temp_2_3.mean, na.rm = TRUE)
#mur_2 ne contient plus de NA


#TOIT
# Interpolation pour les valeurs manquantes au milieu de la série temporelle
toit$Mur_Temp_toit_1.mean<- na.approx(toit$Mur_Temp_toit_1.mean, na.rm = FALSE)
toit$Mur_Temp_toit_2.mean <- na.approx(toit$Mur_Temp_toit_2.mean, na.rm = FALSE)
toit$Mur_Temp_toit_3.mean <- na.approx(toit$Mur_Temp_toit_3.mean, na.rm = FALSE)
#Problème il y a bcp de NA au début (environ 97% des lignes)
# On supprime les NA
# Suppression des lignes en fonction des indices spécifiques
indices_a_supprimer <- c(1:510319)
toit <- toit %>%
  slice(-indices_a_supprimer)
#toit ne contient plus de NA


#PYRANOMETRES
# Interpolation pour les valeurs manquantes au milieu de la série temporelle
pyranometres$Pyranometre_Est_Wm.mean<- na.approx(pyranometres$Pyranometre_Est_Wm.mean, na.rm = FALSE)
pyranometres$Pyranometre_Nord_Wm.mean <- na.approx(pyranometres$Pyranometre_Nord_Wm.mean, na.rm = FALSE)
pyranometres$Pyranometre_Ouest_Wm.mean <- na.approx(pyranometres$Pyranometre_Ouest_Wm.mean, na.rm = FALSE)
pyranometres$Pyranometre_Sud_Wm.mean<- na.approx(pyranometres$Pyranometre_Sud_Wm.mean, na.rm = FALSE)
#Imputation par la moyenne pour les NA ( 0.16%)
pyranometres$Pyranometre_Est_Wm.mean[1:296] <- mean(pyranometres$Pyranometre_Est_Wm.mean, na.rm = TRUE)
pyranometres$Pyranometre_Nord_Wm.mean[1:296] <- mean(pyranometres$Pyranometre_Nord_Wm.mean, na.rm = TRUE)
pyranometres$Pyranometre_Ouest_Wm.mean[1:296] <- mean(pyranometres$Pyranometre_Ouest_Wm.mean, na.rm = TRUE)
pyranometres$Pyranometre_Sud_Wm.mean[1:296] <- mean(pyranometres$Pyranometre_Sud_Wm.mean, na.rm = TRUE)
#pyranometres ne contient plus de NA



#TEMPERATURE

# Diviser les données en deux parties : mesurées toutes les 5 minutes et mesurée toutes les minutes
temp_5min <- temperature %>%
  select(`Time`, starts_with("Temperature"), `Mur_Temp_1_1.mean`) %>%
  mutate(`Time` = as.POSIXct(`Time`, format="%Y-%m-%d %H:%M:%S"))

temp_min <- temperature %>%
  select(`Time`, Station_Meteo_Text) %>%
  mutate(`Time` = as.POSIXct(`Time`, format="%Y-%m-%d %H:%M:%S"))


#On a un faible nombre de valeurs de tempÃ©rature intÃ©rieure absurdes (de -20Â°C Ã  -250Â°C), on les rajoute aux NA
#temperature$Mur_Temp_1_1.mean[temperature$Mur_Temp_1_1.mean<0]<-NA

# Définir une fonction pour remplir les valeurs manquantes dans chaque groupe
fill_group <- function(data) {
  filled_data <- data %>%
    summarise(across(starts_with("Temperature"), mean, na.rm = TRUE), 
              `Mur_Temp_1_1.mean` = first(`Mur_Temp_1_1.mean`))
  return(filled_data)
}

# Diviser les données en groupes plus petits
group_size <- 1000  # Vous pouvez ajuster la taille du groupe selon vos besoins
num_groups <- nrow(temp_5min) %/% group_size + 1

filled_groups <- lapply(1:num_groups, function(i) {
  start_index <- (i - 1) * group_size + 1
  end_index <- min(i * group_size, nrow(temp_5min))
  group_data <- temp_5min[start_index:end_index, ]
  filled_group <- fill_group(group_data)
  return(filled_group)
})

# Fusionner les groupes remplis en un seul dataframe
temp_5min_filled <- bind_rows(filled_groups)

# Ajouter la variable de temps
start_time <- min(temp_5min$`Time`)
end_time <- max(temp_5min$`Time`)
time_sequence <- seq(from = start_time, to = end_time, by = 300)  # Créer une séquence de temps toutes les 5 minutes
temp_5min_filled$`Time` <- time_sequence[seq_along(temp_5min_filled$`Mur_Temp_1_1.mean`)]

#On a des données toutes les 5 min (le temps est à la fin)
 # Interpolation pour les valeurs manquantes au milieu de la sÃ©rie temporelle
temp_5min_filled$Temperature_Interieur_Sud.mean<- na.approx(temp_5min_filled$Temperature_Interieur_Sud.mean, na.rm = FALSE)
temp_5min_filled$Temperature_Interieur_Nord.mean <- na.approx(temp_5min_filled$Temperature_Interieur_Nord.mean , na.rm = FALSE)
temp_5min_filled$Temperature_E4000.mean <- na.approx(temp_5min_filled$Temperature_E4000.mean, na.rm = FALSE)
temp_5min_filled$Temperature_ressentie_E4000.mean<- na.approx(temp_5min_filled$Temperature_ressentie_E4000.mean, na.rm = FALSE)
temp_5min_filled$Station_Meteo_Text <- na.approx(temp_5min_filled$Station_Meteo_Text, na.rm = FALSE)
temp_5min_filled$Mur_Temp_1_1.mean <- na.approx(temp_5min_filled$Mur_Temp_1_1.mean, na.rm = FALSE)

# #Imputation par la moyenne pour les NA (2.80%)
# temperature$Temperature_Interieur_Sud.mean[1:872] <- mean(temperature$Temperature_Interieur_Sud.mean, na.rm = TRUE)
# temperature$Temperature_Interieur_Sud.mean[1281357] <- mean(temperature$Temperature_Interieur_Sud.mean, na.rm = TRUE)
# temperature$Temperature_Interieur_Nord.mean[1:871] <- mean(temperature$Temperature_Interieur_Nord.mean, na.rm = TRUE)
# temperature$Temperature_Interieur_Nord.mean[1281357] <- mean(temperature$Temperature_Interieur_Nord.mean, na.rm = TRUE)
# temperature$Temperature_E4000.mean[1:870] <- mean(temperature$Temperature_E4000.mean, na.rm = TRUE)
# temperature$Temperature_E4000.mean[1281357] <- mean(temperature$Temperature_E4000.mean, na.rm = TRUE)
# temperature$Temperature_ressentie_E4000.mean[1:870] <- mean(temperature$Temperature_ressentie_E4000.mean, na.rm = TRUE)
# temperature$Temperature_ressentie_E4000.mean[1281314:1281357] <- mean(temperature$Temperature_ressentie_E4000.mean, na.rm = TRUE)
# temperature$Station_Meteo_Text[1:4033] <- mean(temperature$Station_Meteo_Text, na.rm = TRUE)
# temperature$Station_Meteo_Text[1281314:1281357] <- mean(temperature$Station_Meteo_Text, na.rm = TRUE)
# temperature$Mur_Temp_1_1.mean[1:31143] <- mean(temperature$Mur_Temp_1_1.mean, na.rm = TRUE)
# temperature$Mur_Temp_1_1.mean[1281357] <- mean(temperature$Mur_Temp_1_1.mean, na.rm = TRUE)

temp_5min_filled <- select(temp_5min_filled, -`Mur_Temp_1_1.mean`)
#température ne contient plus de NA


#ENERGY
# Regrouper les données par timestamp et combiner les valeurs dans chaque groupe
#En fait il y avait plusieurs lignes pour le meme timestamp donc beaucoup de NA évitables.
energie_grouped <- energy %>%
  group_by(`Time`) %>%
  summarise_all(~if(all(is.na(.))) NA else first(na.omit(.)))
# Interpolation pour les valeurs manquantes au milieu de la sÃ©rie temporelle
energie_grouped$Energie_Eclairage <- na.approx(energie_grouped$Energie_Eclairage, na.rm = FALSE)
energie_grouped$Energie_Prises <- na.approx(energie_grouped$Energie_Prises, na.rm = FALSE)
energie_grouped$Energie_Convecteurs <- na.approx(energie_grouped$Energie_Convecteurs, na.rm = FALSE)
#Il y a que des zÃ©ros et des NA dans Energie_VMC donc on la supprime
energieSansVMC <- subset(energie_grouped, select = -c(Energie_VMC))
#energieSansVMC ne contient plus de NA


#HUMIDITE
# Interpolation pour les valeurs manquantes au milieu de la série temporelle
humidite$Humidite_Interieur_Sud.mean <- na.approx(humidite$Humidite_Interieur_Sud.mean, na.rm = FALSE)
humidite$Humidite_Interieur_Nord.mean <- na.approx(humidite$Humidite_Interieur_Nord.mean, na.rm = FALSE)
humidite$Humidite_RELATIVE_E4000.mean <- na.approx(humidite$Humidite_RELATIVE_E4000.mean, na.rm = FALSE)
humidite$Humidite_ABSOLUE_E4000.mean <- na.approx(humidite$Humidite_ABSOLUE_E4000.mean, na.rm = FALSE)
#Imputation par la moyenne pour les NA (2.67%)
humidite$Humidite_Interieur_Sud.mean[1:293] <- mean(humidite$Humidite_Interieur_Sud.mean, na.rm = TRUE)
humidite$Humidite_Interieur_Nord.mean[1:292] <- mean(humidite$Humidite_Interieur_Nord.mean, na.rm = TRUE)
humidite$Humidite_RELATIVE_E4000.mean[1:292] <- mean(humidite$Humidite_RELATIVE_E4000.mean, na.rm = TRUE)
humidite$Humidite_ABSOLUE_E4000.mean[1:292] <- mean(humidite$Humidite_ABSOLUE_E4000.mean, na.rm = TRUE)
#humidite ne contient plus de NA
