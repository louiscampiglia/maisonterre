library(dplyr)
library(lubridate)


# Convertir chaque élément de la liste en dataframe
week_data <- lapply(week_data, as.data.frame)
# Convertir chaque élément de la liste en dataframe
weekend_data <- lapply(weekend_data, as.data.frame)

# jours de semaine hiver
winter_data_week <- lapply(week_data, function(df) {
  df[month(df$Time) %in% c(12, 1, 2), ]
})

# weekedn hiver
winter_data_weekend <- lapply(weekend_data, function(df) {
  df[month(df$Time) %in% c(12,1,2), ]
})

