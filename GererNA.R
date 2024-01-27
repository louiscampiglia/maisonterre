
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
#On a un faible nombre de valeurs de température intérieure absurdes (de -20°C à -250°C), on les rajoute aux NA
temperature$Mur_Temp_1_1.mean[temperature$Mur_Temp_1_1.mean<0]<-NA
# Interpolation pour les valeurs manquantes au milieu de la série temporelle
temperature$Temperature_Interieur_Sud.mean<- na.approx(temperature$Temperature_Interieur_Sud.mean, na.rm = FALSE)
temperature$Temperature_Interieur_Nord.mean <- na.approx(temperature$Temperature_Interieur_Nord.mean , na.rm = FALSE)
temperature$Temperature_E4000.mean <- na.approx(temperature$Temperature_E4000.mean, na.rm = FALSE)
temperature$Temperature_ressentie_E4000.mean<- na.approx(temperature$Temperature_ressentie_E4000.mean, na.rm = FALSE)
temperature$Station_Meteo_Text <- na.approx(temperature$Station_Meteo_Text, na.rm = FALSE)
temperature$Mur_Temp_1_1.mean <- na.approx(temperature$Mur_Temp_1_1.mean, na.rm = FALSE)
#Imputation par la moyenne pour les NA (2.80%)
temperature$Temperature_Interieur_Sud.mean[1:872] <- mean(temperature$Temperature_Interieur_Sud.mean, na.rm = TRUE)
temperature$Temperature_Interieur_Sud.mean[1281357] <- mean(temperature$Temperature_Interieur_Sud.mean, na.rm = TRUE)
temperature$Temperature_Interieur_Nord.mean[1:871] <- mean(temperature$Temperature_Interieur_Nord.mean, na.rm = TRUE)
temperature$Temperature_Interieur_Nord.mean[1281357] <- mean(temperature$Temperature_Interieur_Nord.mean, na.rm = TRUE)
temperature$Temperature_E4000.mean[1:870] <- mean(temperature$Temperature_E4000.mean, na.rm = TRUE)
temperature$Temperature_E4000.mean[1281357] <- mean(temperature$Temperature_E4000.mean, na.rm = TRUE)
temperature$Temperature_ressentie_E4000.mean[1:870] <- mean(temperature$Temperature_ressentie_E4000.mean, na.rm = TRUE)
temperature$Temperature_ressentie_E4000.mean[1281314:1281357] <- mean(temperature$Temperature_ressentie_E4000.mean, na.rm = TRUE)
temperature$Station_Meteo_Text[1:4033] <- mean(temperature$Station_Meteo_Text, na.rm = TRUE)
temperature$Station_Meteo_Text[1281314:1281357] <- mean(temperature$Station_Meteo_Text, na.rm = TRUE)
temperature$Mur_Temp_1_1.mean[1:31143] <- mean(temperature$Mur_Temp_1_1.mean, na.rm = TRUE)
temperature$Mur_Temp_1_1.mean[1281357] <- mean(temperature$Mur_Temp_1_1.mean, na.rm = TRUE)
#pyranometres ne contient plus de NA


#ENERGY
# Regrouper les données par timestamp et combiner les valeurs dans chaque groupe
#En fait il y avait plusieurs lignes pour le meme timestamp donc beaucoup de NA évitables.
energy_grouped <- energy %>%
  group_by(`ï..Time`) %>%
  summarise_all(~if(all(is.na(.))) NA else first(na.omit(.)))
# Interpolation pour les valeurs manquantes au milieu de la sÃ©rie temporelle
energy_grouped$Energie_Eclairage <- na.approx(energy_grouped$Energie_Eclairage, na.rm = FALSE)
energy_grouped$Energie_Prises <- na.approx(energy_grouped$Energie_Prises, na.rm = FALSE)
energy_grouped$Energie_Convecteurs <- na.approx(energy_grouped$Energie_Convecteurs, na.rm = FALSE)
#Il y a que des zÃ©ros et des NA dans Energie_VMC donc on la supprime
energySansVMC <- subset(energy_grouped, select = -c(Energie_VMC))
#energySansVMC ne contient plus de NA


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
