devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)

competitions <- FreeCompetitions()

la_liga <- competitions %>% 
  filter(competition_id == 11 & season_name == "2015/2016")

la_liga_games <- FreeMatches(la_liga)

la_liga_events <- free_allevents(MatchesDF = la_liga_games)
glimpse(la_liga_events)
table(la_liga_events$type.name)

la_liga_events_cleaned <- allclean(la_liga_events)
glimpse(la_liga_events_cleaned)


#Excluye columnas seleccionadas
library(dplyr)
la_liga_events_cleaned_to_store <- la_liga_events_cleaned %>% 
  select(-c(related_events, location, tactics.lineup, 
            pass.end_location, carry.end_location,
            shot.end_location, shot.freeze_frame, goalkeeper.end_location))


# Se agregan las medidas en metros de las coordenadas de la cancha

max_x_source <- 120
max_y_source <- 80
max_x_final <- 110 # 120 yardas equivalen aprox. 110 metros
max_y_final <- 73 # 80 yardas equivalen aprox. 73 metros

la_liga_events_cleaned_to_store <- la_liga_events_cleaned_to_store %>%
  mutate(pos_x_meter = location.x/max_x_source*max_x_final,
         pos_y_meter = location.y/max_y_source*max_y_final,
         pos_y_meter = 73 - pos_y_meter,
         pass_end_pos_x_meter = pass.end_location.x/max_x_source*max_x_final,
         pass_end_pos_y_meter = pass.end_location.y/max_y_source*max_y_final,
         pass_end_pos_y_meter = 73 - pass_end_pos_y_meter)

# Exportar/Guardar datos en CSV
library(readr)
write_csv(la_liga_events_cleaned_to_store, "shinny_app/data/statsbomb_liga_15_16_events.csv")
write_csv(la_liga_games, "shinny_app/data/statsbomb_liga_15_16_games.csv")