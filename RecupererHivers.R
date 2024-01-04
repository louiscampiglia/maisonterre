library(dplyr)
library(lubridate)


#DIVISER LES DONNEES PAR SAISONS pour récupérer les hivers
#résultats :  data_winter_date


#ENERGY
energyMonth <- energySansVMC %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
energy_winter_2018 <- energyMonth %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

energy_winter_2019 <- energyMonth %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

energy_winter_2020 <- energyMonth %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

energy_winter_2021 <- energyMonth %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

energy_winter_2022 <- energyMonth %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

energy_winter_2023 <- energyMonth %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))


#HUMIDITE

humiditeMonth <- humidite %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
humidite_winter_2018 <- humiditeMonth %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

humidite_winter_2019 <- humiditeMonth %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

humidite_winter_2020 <- humiditeMonth %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

humidite_winter_2021 <- humiditeMonth %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

humidite_winter_2022 <- humiditeMonth %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

humidite_winter_2023 <- humiditeMonth %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))

#MUR_1

mur_1Month <- mur_1 %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
mur_1_winter_2018 <- mur_1Month %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

mur_1_winter_2019 <- mur_1Month %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

mur_1_winter_2020 <- mur_1Month %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

mur_1_winter_2021 <- mur_1Month %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

mur_1_winter_2022 <- mur_1Month %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

mur_1_winter_2023 <- mur_1Month %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))

#MUR_2

mur_2Month <- mur_2 %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
mur_2_winter_2018 <- mur_2Month %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

mur_2_winter_2019 <- mur_2Month %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

mur_2_winter_2020 <- mur_2Month %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

mur_2_winter_2021 <- mur_2Month %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

mur_2_winter_2022 <- mur_2Month %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

mur_2_winter_2023 <- mur_2Month %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))


#MUR_SUD

mur_sudMonth <- mur_sud %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
mur_sud_winter_2018 <- mur_sudMonth %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

mur_sud_winter_2019 <- mur_sudMonth %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

mur_sud_winter_2020 <- mur_sudMonth %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

mur_sud_winter_2021 <- mur_sudMonth %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

mur_sud_winter_2022 <- mur_sudMonth %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

mur_sud_winter_2023 <- mur_sudMonth %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))


#PYRANOMETRES

pyranometresMonth <- pyranometres %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
pyranometres_winter_2018 <- pyranometresMonth %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

pyranometres_winter_2019 <- pyranometresMonth %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

pyranometres_winter_2020 <- pyranometresMonth %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

pyranometres_winter_2021 <- pyranometresMonth %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

pyranometres_winter_2022 <- pyranometresMonth %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

pyranometres_winter_2023 <- pyranometresMonth %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))


#TEMPERATURE

temperatureMonth <- temperature %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
temperature_winter_2018 <- temperatureMonth %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

temperature_winter_2019 <- temperatureMonth %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

temperature_winter_2020 <- temperatureMonth %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

temperature_winter_2021 <- temperatureMonth %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

temperature_winter_2022 <- temperatureMonth %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

temperature_winter_2023 <- temperatureMonth %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))

#TOIT

toitMonth <- temperature %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
toit_winter_2018 <- toitMonth %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

toit_winter_2019 <- toitMonth %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

toit_winter_2020 <- toitMonth %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

toit_winter_2021 <- toitMonth %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

toit_winter_2022 <- toitMonth %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

toit_winter_2023 <- toitMonth %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))


#CONVECTEURS

convecteursMonth <- convecteurs %>%
  mutate(Year = year(ï..Time), Month = month(ï..Time))

# Filtrer les données pour chaque hiver
convecteurs_winter_2018 <- convecteursMonth %>%
  filter(Year == 2018 & Month %in% c(12, 1, 2))

convecteurs_winter_2019 <- convecteursMonth %>%
  filter(Year == 2019 & Month %in% c(12, 1, 2))

convecteurs_winter_2020 <- convecteursMonth %>%
  filter(Year == 2020 & Month %in% c(12, 1, 2))

convecteurs_winter_2021 <- convecteursMonth %>%
  filter(Year == 2021 & Month %in% c(12, 1, 2))

convecteurs_winter_2022 <- convecteursMonth %>%
  filter(Year == 2022 & Month %in% c(12, 1, 2))

convecteurs_winter_2023 <- convecteursMonth %>%
  filter(Year == 2023 & Month %in% c(12, 1, 2))






