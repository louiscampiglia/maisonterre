
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
#Imputation par la moyenne pour les NA au début (6% + 2%)
mur_1$Mur_Temp_1_1.mean[1:33089] <- mean(mur_1$Mur_Temp_1_1.mean, na.rm = TRUE)
mur_1$Mur_Temp_1_2.mean[1:33089] <- mean(mur_1$Mur_Temp_1_2.mean, na.rm = TRUE)
mur_1$Mur_Temp_1_3.mean[1:33089] <- mean(mur_1$Mur_Temp_1_3.mean, na.rm = TRUE)
#Imputation par regression
model <- lm(Mur_Temp_1_1.mean ~ Mur_Temp_1_2.mean, data = mur_1, subset = !is.na(Mur_Temp_1_1.mean))
indices_a_imputer <- 48004:59322
mur_1$Mur_Temp_1_1.mean[indices_a_imputer] <- predict(model, newdata = mur_1[indices_a_imputer, ])
model_2 <- lm(Mur_Temp_1_2.mean ~ Mur_Temp_1_3.mean, data = mur_1, subset = !is.na(Mur_Temp_1_2.mean))
mur_1$Mur_Temp_1_2.mean[indices_a_imputer] <- predict(model_2, newdata = mur_1[indices_a_imputer, ])
model_3 <- lm(Mur_Temp_1_2.mean ~ Mur_Temp_1_3.mean, data = mur_1, subset = !is.na(Mur_Temp_1_2.mean))
mur_1$Mur_Temp_1_2.mean[indices_a_imputer] <- predict(model_3, newdata = mur_1[indices_a_imputer, ])



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
