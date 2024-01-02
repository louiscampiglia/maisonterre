
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
# Interpolation pour les valeurs manquantes au milieu de la série temporelle
energy$Energie_Eclairage <- na.approx(energy$Energie_Eclairage, na.rm = FALSE)
energy$Energie_Prises <- na.approx(energy$Energie_Prises, na.rm = FALSE)
energy$Energie_Convecteurs <- na.approx(energy$Energie_Convecteurs, na.rm = FALSE)
#Il y a que des zéros et des NA dans Energie_VMC donc on la supprime
energySansVMC <- subset(energy, select = -c(Energie_VMC))
# Imputation par la moyenne pour les valeurs manquantes au début ou a la fin
energySansVMC$Energie_Eclairage[2770827:2770828] <- mean(energySansVMC$Energie_Eclairage, na.rm = TRUE)
energySansVMC$Energie_Prises[1] <- mean(energySansVMC$Energie_Prises, na.rm = TRUE)
energySansVMC$Energie_Prises[2770828] <- mean(energySansVMC$Energie_Prises, na.rm = TRUE)
energySansVMC$Energie_Convecteurs[1:3] <- mean(energySansVMC$Energie_Convecteurs, na.rm = TRUE)
#ou bien on supprime les lignes car il y en a pas bcp?
#energySansVMC ne contient plus de NA