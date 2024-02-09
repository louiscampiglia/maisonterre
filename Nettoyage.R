library(ggplot2)
library(dplyr)
library(lubridate)
library(anytime)

# Les données sont renommées toutes en français et sans accent A CHANGER DANS DATA_5_YEARS!!
# setwd("votre chemin data_5years")

#MUR SUD
# Remplacer "null" par NA dans toutes les colonnes
mur_sud[mur_sud == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
mur_sud$Time <- as.POSIXct(mur_sud$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Remplacer "," par le point et convertir en double
mur_sud$Mur_Temp_sud_1.mean<- as.numeric(gsub(",", ".", mur_sud$Mur_Temp_sud_1.mean))
mur_sud$Mur_Temp_sud_2.mean<- as.numeric(gsub(",", ".", mur_sud$Mur_Temp_sud_2.mean))
mur_sud$Mur_Temp_sud_3.mean<- as.numeric(gsub(",", ".", mur_sud$Mur_Temp_sud_3.mean))


#MUR 1
# Remplacer "null" par NA dans toutes les colonnes
mur_1[mur_1 == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
mur_1$Time <- as.POSIXct(mur_1$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Remplacer "," par le point et convertir en double
mur_1$Mur_Temp_1_1.mean<- as.numeric(gsub(",", ".", mur_1$Mur_Temp_1_1.mean))
mur_1$Mur_Temp_1_2.mean<- as.numeric(gsub(",", ".", mur_1$Mur_Temp_1_2.mean))
mur_1$Mur_Temp_1_3.mean<- as.numeric(gsub(",", ".", mur_1$Mur_Temp_1_3.mean))


#MUR 2
# Remplacer "null" par NA dans toutes les colonnes
mur_2[mur_2 == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
mur_2$Time <- as.POSIXct(mur_2$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Remplacer "," par le point et convertir en double
mur_2$Mur_Temp_2_2.mean<- as.numeric(gsub(",", ".", mur_2$Mur_Temp_2_2.mean))
mur_2$Mur_Temp_2_3.mean<- as.numeric(gsub(",", ".", mur_2$Mur_Temp_2_3.mean))

#TOIT
# Remplacer "null" par NA dans toutes les colonnes
toit[toit == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
toit$Time <- as.POSIXct(toit$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Remplacer "," par le point et convertir en double
toit$Mur_Temp_toit_1.mean<- as.numeric(gsub(",", ".", toit$Mur_Temp_toit_1.mean))
toit$Mur_Temp_toit_2.mean<- as.numeric(gsub(",", ".", toit$Mur_Temp_toit_2.mean))
toit$Mur_Temp_toit_3.mean<- as.numeric(gsub(",", ".", toit$Mur_Temp_toit_3.mean))
#inversition des numéros de capteurs 1 et 3 du toit pour que ca concorde avec les murs :
save_toit_1 <- toit$Mur_Temp_toit_1.mean
toit$Mur_Temp_toit_1.mean <- toit$Mur_Temp_toit_3.mean
toit$Mur_Temp_toit_3.mean <- save_toit_1


#PYRANOMETRES
# Remplacer "null" par NA dans toutes les colonnes
pyranometres[pyranometres == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
pyranometres$Time <- as.POSIXct(pyranometres$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Remplacer "," par le point et convertir en double
pyranometres$Pyranometre_Est_Wm.mean<- as.numeric(gsub(",", ".", pyranometres$Pyranometre_Est_Wm.mean))
pyranometres$Pyranometre_Nord_Wm.mean<- as.numeric(gsub(",", ".", pyranometres$Pyranometre_Nord_Wm.mean))
pyranometres$Pyranometre_Ouest_Wm.mean<- as.numeric(gsub(",", ".", pyranometres$Pyranometre_Ouest_Wm.mean))
pyranometres$Pyranometre_Sud_Wm.mean<- as.numeric(gsub(",", ".", pyranometres$Pyranometre_Sud_Wm.mean))


#TEMPERATURE
# Remplacer "null" par NA dans toutes les colonnes
temperature[temperature == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
temperature$Time <- as.POSIXct(temperature$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Remplacer "," par le point et mettre en double 
temperature$Temperature_Interieur_Sud.mean<- as.numeric(gsub(",", ".", temperature$Temperature_Interieur_Sud.mean))
temperature$Temperature_Interieur_Nord.mean<- as.numeric(gsub(",", ".", temperature$Temperature_Interieur_Nord.mean))
temperature$Temperature_E4000.mean<- as.numeric(gsub(",", ".", temperature$Temperature_E4000.mean))
temperature$Temperature_ressentie_E4000.mean<- as.numeric(gsub(",", ".", temperature$Temperature_ressentie_E4000.mean))
temperature$Mur_Temp_1_1.mean<- as.numeric(gsub(",", ".", temperature$Mur_Temp_1_1.mean))


#HUMIDITE
# Remplacer "null" par NA dans toutes les colonnes
humidite[humidite == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
humidite$Time <- as.POSIXct(humidite$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Remplacer "," par le point et mettre en double
humidite$Humidite_Interieur_Sud.mean<- as.numeric(gsub(",", ".", humidite$Humidite_Interieur_Sud.mean))
humidite$Humidite_Interieur_Nord.mean<- as.numeric(gsub(",", ".", humidite$Humidite_Interieur_Nord.mean))
humidite$Humidite_RELATIVE_E4000.mean<- as.numeric(gsub(",", ".", humidite$Humidite_RELATIVE_E4000.mean))
humidite$Humidite_ABSOLUE_E4000.mean<- as.numeric(gsub(",", ".", humidite$Humidite_ABSOLUE_E4000.mean))


#CONVECTEURS
# Remplacer "null" par NA dans toutes les colonnes
convecteurs[convecteurs == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
convecteurs$Time <- as.POSIXct(convecteurs$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Remplacer "â€¯" par rien dans la colonne Puissance_Convecteurs(KW) et convertir en double
convecteurs$Puissance_Convecteurs <- as.numeric(gsub("â€¯", "", convecteurs$Puissance_Convecteurs))
## ou (selon la version de R) : 
## convecteurs$Puissance_Convecteurs <- as.numeric(gsub(".", "", convecteurs$Puissance_Convecteurs))
# Supprimer les virgules des colonnes
convecteurs$Puissance_Convecteurs <- gsub(",", ".", convecteurs$Puissance_Convecteurs)


#ENERGY

# Remplacer "null" par NA dans toutes les colonnes
energie[energie == "null"] <- NA
# Convertir les dates en objets temporels avec as.POSIXct
energie$Time <- as.POSIXct(energie$Time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Supprimer les virgules des colonnes
energie$Energie_Eclairage <- gsub(",", ".", energie$Energie_Eclairage)
energie$Energie_VMC <- gsub(",", ".", energie$Energie_VMC)
energie$Energie_Prises <- gsub(",", ".", energie$Energie_Prises)
energie$Energie_Convecteurs <- gsub(",", ".", energie$Energie_Convecteurs)
# Remplacer "â€¯" par rien et convertir en double
energie$Energie_Prises <- as.numeric(gsub("â€¯", "", energie$Energie_Prises))
energie$Energie_Convecteurs <- as.numeric(gsub("â€¯", "", energie$Energie_Convecteurs))
## ou, selon la version de R :
## energy$Energie_Prises <- as.numeric(gsub(".", "", energy$Energie_Prises))
## energy$Energie_Convecteurs <- as.numeric(gsub(".", "", energy$Energie_Convecteurs))
energie$Energie_Eclairage <- as.numeric(energie$Energie_Eclairage)
energie$Energie_VMC <- as.numeric(energie$Energie_VMC)









#pour mur_1, energy et température, j'ai combiné 2 fichiers car je n'ai pas réussi à afficher 5 ans d'un coup sur le site

str(toit)  # données depuis septembre août 2023 // 3 cateurs dans le toit :
    #numérotation inverse par rapport aux murs : _1 proche interieur, _2 milieu, _3 proche exterieur

str(mur_sud)  # données depuis fin août 2023 // 3 cateurs dans le mur sud :
    #_1 proche exterieur, _2 milieu, _3 proche interieur

str(mur_1)  # 3 cateurs dans le mur 1 : _1 proche exterieur, _2 milieu, _3 proche interieur

str(mur_2)  #  2 cateurs dans le mur 2 (l'interieur ne marche pas) : _1 proche exterieur, _2 milieu

str(pyranomètres) # energie solaire (W/m2) des 4 côtés

str(convecteurs) # puissance (W) ; mise en route dès que Ti<19°C

str(energy)  # energie (kWh) cumulée depuis le début : convecteurs, eclairage et prises

str(température)  # température (°C) exterieur, station météo, intérieur (nord + sud),
    #ressentie (avec humidité) et dans le mur 1 proche interieur.

str(humidité)  # humidité relative (%) à l'interieur (nord + sud) et à l'extrieur
    # + humidité absolue (g/kgas à l'exterieur)








#régularité données
#données manquantes

#moyennes glissantes



#plot
#observations


library(stringr)

str_replace(convecteurs,
            pattern = "â€¯", 
            replacement = ",")


















