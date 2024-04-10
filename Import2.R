library(readr)


setwd("C:/Users/RN Mougin/Desktop/Projet_tutore/data/data_5years")
getwd()


#CONVECTEURS
convecteurs <- read_delim("convecteurs.csv",delim = ";", escape_double = FALSE, 
                          col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")),
                          na = "NA", trim_ws = TRUE, skip = 1)
#Retirer les espaces
convecteurs$Puissance_Convecteurs <- str_replace_all(convecteurs$Puissance_Convecteurs, "\\s+", "")
#Char en Int
convecteurs$Puissance_Convecteurs <- as.integer(convecteurs$Puissance_Convecteurs)


#MUR_2
mur_2 <- read_delim("mur_2.csv", delim = ";",escape_double = FALSE, 
                    col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")),
                    trim_ws = TRUE, skip = 1)
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
mur_2$Mur_Temp_2_2.mean <- gsub(",", ".", mur_2$Mur_Temp_2_2.mean)
mur_2$Mur_Temp_2_3.mean <- gsub(",", ".", mur_2$Mur_Temp_2_3.mean)
# Char en Double
mur_2$Mur_Temp_2_2.mean <- as.double(mur_2$Mur_Temp_2_2.mean)
mur_2$Mur_Temp_2_3.mean <- as.double(mur_2$Mur_Temp_2_3.mean)


#MUR_1
mur_1 <- read_delim("mur_1.csv", delim = ";",
                    escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")),
                    trim_ws = TRUE, skip = 1)
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
mur_1$Mur_Temp_1_1.mean <- gsub(",", ".", mur_1$Mur_Temp_1_1.mean)
mur_1$Mur_Temp_1_2.mean<- gsub(",", ".",mur_1$Mur_Temp_1_2.mean)
mur_1$Mur_Temp_1_3.mean<- gsub(",", ".", mur_1$Mur_Temp_1_3.mean)
# Char en Double
mur_1$Mur_Temp_1_1.mean <- as.double(mur_1$Mur_Temp_1_1.mean)
mur_1$Mur_Temp_1_2.mean <- as.double(mur_1$Mur_Temp_1_2.mean)
mur_1$Mur_Temp_1_3.mean <- as.double(mur_1$Mur_Temp_1_3.mean)

#MUR_SUD
mur_sud <- read_delim("mur_sud.csv", delim = ";",
                      escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")),
                      trim_ws = TRUE, skip = 1)
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
mur_sud$Mur_Temp_sud_1.mean <- gsub(",", ".", mur_sud$Mur_Temp_sud_1.mean)
mur_sud$Mur_Temp_sud_2.mean <- gsub(",", ".", mur_sud$Mur_Temp_sud_2.mean)
mur_sud$Mur_Temp_sud_3.mean <- gsub(",", ".", mur_sud$Mur_Temp_sud_3.mean)
# Char en Double
mur_sud$Mur_Temp_sud_1.mean<- as.double(mur_sud$Mur_Temp_sud_1.mean)
mur_sud$Mur_Temp_sud_2.mean <- as.double(mur_sud$Mur_Temp_sud_2.mean)
mur_sud$Mur_Temp_sud_3.mean <- as.double(mur_sud$Mur_Temp_sud_3.mean)



#HUMIDITE
humidite <- read_delim("humidite.csv", delim = ";",
                       escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H"),
                                                               Humidite_Interieur_Sud.mean = col_character()),
                       trim_ws = TRUE, skip = 1)
#Retirer les espaces
humidite$Humidite_Interieur_Sud.mean <- str_replace_all(humidite$Humidite_Interieur_Sud.mean, "\\s+", "")
humidite$Humidite_Interieur_Nord.mean <- str_replace_all(humidite$Humidite_Interieur_Nord.mean, "\\s+", "")
humidite$Humidite_RELATIVE_E4000.mean <- str_replace_all(humidite$Humidite_RELATIVE_E4000.mean, "\\s+", "")
humidite$Humidite_ABSOLUE_E4000.mean <- str_replace_all(humidite$Humidite_ABSOLUE_E4000.mean, "\\s+", "")
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
humidite$Humidite_Interieur_Sud.mean <- gsub(",", ".", humidite$Humidite_Interieur_Sud.mean)
humidite$Humidite_Interieur_Nord.mean <- gsub(",", ".", humidite$Humidite_Interieur_Nord.mean)
humidite$Humidite_RELATIVE_E4000.mean <- gsub(",", ".", humidite$Humidite_RELATIVE_E4000.mean)
humidite$Humidite_ABSOLUE_E4000.mean <- gsub(",", ".", humidite$Humidite_ABSOLUE_E4000.mean)
# Char en Double
humidite$Humidite_Interieur_Sud.mean <- as.double(humidite$Humidite_Interieur_Sud.mean)
humidite$Humidite_Interieur_Nord.mean <- as.double(humidite$Humidite_Interieur_Nord.mean)
humidite$Humidite_RELATIVE_E4000.mean <- as.double(humidite$Humidite_RELATIVE_E4000.mean)
humidite$Humidite_ABSOLUE_E4000.mean <- as.double(humidite$Humidite_ABSOLUE_E4000.mean)

#PYRANOMETRES
pyranometres <- read_delim("pyranometres.csv", 
                           delim = ";", 
                           locale = locale(decimal_mark = ","), 
                           escape_double = FALSE, 
                           col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")), 
                           trim_ws = TRUE, 
                           skip = 1)
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
pyranometres$Pyranometre_Est_Wm.mean<- gsub(",", ".", pyranometres$Pyranometre_Est_Wm.mean)
pyranometres$Pyranometre_Nord_Wm.mean <- gsub(",", ".", pyranometres$Pyranometre_Nord_Wm.mean )
pyranometres$Pyranometre_Ouest_Wm.mean <- gsub(",", ".", pyranometres$Pyranometre_Ouest_Wm.mean)
pyranometres$Pyranometre_Sud_Wm.mean <- gsub(",", ".", pyranometres$Pyranometre_Sud_Wm.mean)
# Char en Double
pyranometres$Pyranometre_Est_Wm.mean <- as.double(pyranometres$Pyranometre_Est_Wm.mean)
pyranometres$Pyranometre_Nord_Wm.mean  <- as.double(pyranometres$Pyranometre_Nord_Wm.mean )
pyranometres$Pyranometre_Ouest_Wm.mean <- as.double(pyranometres$Pyranometre_Ouest_Wm.mean)
pyranometres$Pyranometre_Sud_Wm.mean <- as.double(pyranometres$Pyranometre_Sud_Wm.mean)

#TOIT
toit <- read_delim("toit.csv", delim = ";",
                   escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")),
                   trim_ws = TRUE, skip = 1)
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
toit$Mur_Temp_toit_1.mean <- gsub(",", ".", toit$Mur_Temp_toit_1.mean)
toit$Mur_Temp_toit_2.mean <- gsub(",", ".", toit$Mur_Temp_toit_2.mean)
toit$Mur_Temp_toit_3.mean <- gsub(",", ".", toit$Mur_Temp_toit_3.mean)
# Char en Double
toit$Mur_Temp_toit_1.mean <- as.double(toit$Mur_Temp_toit_1.mean)
toit$Mur_Temp_toit_2.mean <- as.double(toit$Mur_Temp_toit_2.mean)
toit$Mur_Temp_toit_3.mean <- as.double(toit$Mur_Temp_toit_3.mean)




#ENERGIE
energie <- read_delim("energie.csv", delim = ";",
                      escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")),
                      trim_ws = TRUE, skip = 1)
#Retirer les espaces
energie$Energie_Eclairage <- str_replace_all(energie$Energie_Eclairage, "\\s+", "")
energie$Energie_VMC <- str_replace_all(energie$Energie_VMC, "\\s+", "")
energie$Energie_Prises <- str_replace_all(energie$Energie_Prises, "\\s+", "")
energie$Energie_Convecteurs <- str_replace_all(energie$Energie_Convecteurs, "\\s+", "")
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
energie$Energie_Eclairage <- gsub(",", ".", energie$Energie_Eclairage)
energie$Energie_VMC <- gsub(",", ".", energie$Energie_VMC)
energie$Energie_Prises <- gsub(",", ".", energie$Energie_Prises)
energie$Energie_Convecteurs <- gsub(",", ".", energie$Energie_Convecteurs)
# Char en Double
energie$Energie_Eclairage <- as.double(energie$Energie_Eclairage)
energie$Energie_VMC <- as.double(energie$Energie_VMC)
energie$Energie_Prises <- as.double(energie$Energie_Prises)
energie$Energie_Convecteurs <- as.double(energie$Energie_Convecteurs)

#TEMPERATURE
temperature_part1 <- read_delim("temperature_part1.csv", 
                                delim = ";", escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")), 
                                trim_ws = TRUE, skip = 1)
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
temperature_part1$Temperature_Interieur_Sud.mean <- gsub(",", ".", temperature_part1$Temperature_Interieur_Sud.mean)
temperature_part1$Temperature_Interieur_Nord.mean <- gsub(",", ".", temperature_part1$Temperature_Interieur_Nord.mean)
temperature_part1$Temperature_E4000.mean <- gsub(",", ".", temperature_part1$Temperature_E4000.mean)
temperature_part1$Temperature_ressentie_E4000.mean <- gsub(",", ".", temperature_part1$Temperature_ressentie_E4000.mean)
temperature_part1$Station_Meteo_Text <- gsub(",", ".", temperature_part1$Station_Meteo_Text)
temperature_part1$Mur_Temp_1_1.mean <- gsub(",", ".", temperature_part1$Mur_Temp_1_1.mean)
# Char en Double
temperature_part1$Temperature_Interieur_Sud.mean <- as.double(temperature_part1$Temperature_Interieur_Sud.mean)
temperature_part1$Temperature_Interieur_Nord.mean <- as.double(temperature_part1$Temperature_Interieur_Nord.mean)
temperature_part1$Temperature_E4000.mean <- as.double(temperature_part1$Temperature_E4000.mean)
temperature_part1$Temperature_ressentie_E4000.mean <- as.double(temperature_part1$Temperature_ressentie_E4000.mean)
temperature_part1$Station_Meteo_Text <- as.double(temperature_part1$Station_Meteo_Text)
temperature_part1$Mur_Temp_1_1.mean <- as.double(temperature_part1$Mur_Temp_1_1.mean)

#TEMPERATURE
temperature_part2 <- read_delim("temperature_part2.csv", 
                                delim = ";", escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")), 
                                trim_ws = TRUE, skip = 1)
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
temperature_part2$Temperature_Interieur_Sud.mean <- gsub(",", ".", temperature_part2$Temperature_Interieur_Sud.mean)
temperature_part2$Temperature_Interieur_Nord.mean <- gsub(",", ".", temperature_part2$Temperature_Interieur_Nord.mean)
temperature_part2$Temperature_E4000.mean <- gsub(",", ".", temperature_part2$Temperature_E4000.mean)
temperature_part2$Temperature_ressentie_E4000.mean <- gsub(",", ".", temperature_part2$Temperature_ressentie_E4000.mean)
temperature_part2$Station_Meteo_Text <- gsub(",", ".", temperature_part2$Station_Meteo_Text)
temperature_part2$Mur_Temp_1_1.mean <- gsub(",", ".", temperature_part2$Mur_Temp_1_1.mean)
# Char en Double
temperature_part2$Temperature_Interieur_Sud.mean <- as.double(temperature_part2$Temperature_Interieur_Sud.mean)
temperature_part2$Temperature_Interieur_Nord.mean <- as.double(temperature_part2$Temperature_Interieur_Nord.mean)
temperature_part2$Temperature_E4000.mean <- as.double(temperature_part2$Temperature_E4000.mean)
temperature_part2$Temperature_ressentie_E4000.mean <- as.double(temperature_part2$Temperature_ressentie_E4000.mean)
temperature_part2$Station_Meteo_Text <- as.double(temperature_part2$Station_Meteo_Text)
temperature_part2$Mur_Temp_1_1.mean <- as.double(temperature_part2$Mur_Temp_1_1.mean)

# Combiner les parties des fichiers qui sont en deux parties
temperature <- rbind(temperature_part2, temperature_part1)

#CO2
co2 <- read_delim("co2.csv", delim = ";",escape_double = FALSE, 
                  col_types = cols(Time = col_datetime(format = "%Y-%m-%d-%H")), 
                  trim_ws = TRUE, skip = 1)
# Convertir les virgules en points dans les colonnes contenant des valeurs numériques
co2$CO2_Interieur_Sud.mean <- gsub(",", ".", co2$CO2_Interieur_Sud.mean)
co2$CO2_Interieur_Nord.mean <- gsub(",", ".", co2$CO2_Interieur_Nord.mean)
co2$CO2_E4000.mean <- gsub(",", ".", co2$CO2_E4000.mean)
co2$`COV_E4000.mean  x100` <- gsub(",", ".", co2$`COV_E4000.mean  x100`)
# Char en Double
co2$CO2_Interieur_Sud.mean <- as.double(co2$CO2_Interieur_Sud.mean)
co2$CO2_Interieur_Nord.mean <- as.double(co2$CO2_Interieur_Nord.mean)
co2$CO2_E4000.mean <- as.double(co2$CO2_E4000.mean)
co2$`COV_E4000.mean  x100` <- as.double(co2$`COV_E4000.mean  x100`)



#Donnees meteo

# Charger le fichier .dat en tant que dataframe
meteo2020 <- read_delim("meteo2020.dat", delim = ",", escape_double = FALSE, 
                        col_types = cols(TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
                        trim_ws = TRUE, skip = 1)
#On supprime les lignes d'information
meteo2020 <- meteo2020[-c(1:2), ]
# Charger le fichier .dat en tant que dataframe
meteo2021 <- read_delim("meteo2021.dat", delim = ",", escape_double = FALSE, 
                        col_types = cols(TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
                        trim_ws = TRUE, skip = 1)
#On supprime les lignes d'information
meteo2021 <- meteo2021[-c(1:2), ]

# Combiner les parties des fichiers qui sont en deux parties
meteo <- rbind(meteo2020, meteo2021)
# Char en Double
meteo$RECORD <- as.integer(meteo$RECORD)
meteo$CR1000_Alim_Avg <- as.double(meteo$CR1000_Alim_Avg)
meteo$CR1000_Temp_Avg <- as.double(meteo$CR1000_Temp_Avg)
meteo$AirTemp_Avg <- as.double(meteo$AirTemp_Avg)
meteo$RH_Avg<-as.double(meteo$RH_Avg)
meteo$Patm_Avg <-as.integer(meteo$Patm_Avg)
meteo$WSpd_Avg <- as.double(meteo$WSpd_Avg)
meteo$WDir_Avg <- as.double(meteo$WDir_Avg)
meteo$WDir_Std <- as.double(meteo$WDir_Std)
meteo$WSpd_Max <- as.double(meteo$WSpd_Max)
meteo$Rain_mesure_Tot <- as.double(meteo$Rain_mesure_Tot)
meteo$Ray_Global_Avg <- as.double(meteo$Ray_Global_Avg)


