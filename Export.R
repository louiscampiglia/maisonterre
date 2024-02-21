# Pour utiliser les données nettoyées

#Il faut dabord créer un fichier csv vide
#Chemin à modifier 

write.csv(convecteurs, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/convecteurs.csv", row.names = FALSE)
write.csv(energieSansVMC, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/energie.csv", row.names = FALSE)
write.csv(humidite, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/humidite.csv", row.names = FALSE)
write.csv(mur_1, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/mur_1.csv", row.names = FALSE)
write.csv(mur_2, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/mur_2.csv", row.names = FALSE)
write.csv(mur_sud, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/mur_sud.csv", row.names = FALSE)
write.csv(pyranometres, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/pyranometres.csv", row.names = FALSE)
write.csv(temperature, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/temperature.csv", row.names = FALSE)
write.csv(toit, "C:/Users/RN Mougin/Documents/Projet_Tutore_Clean_Data/toit.csv", row.names = FALSE)

# Pour format Excel
# library(openxlsx)
# write.xlsx(nom_de_vos_données, "chemin_vers_le_fichier.xlsx")
