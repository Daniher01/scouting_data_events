devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)

competitions <- FreeCompetitions()

qatar_2022 <- competitions %>% 
  filter(competition_id == 43 & season_name == 2022)

qatar_2022_games <- FreeMatches(qatar_2022)

qatar_2022_events <- free_allevents(MatchesDF = qatar_2022_games)
glimpse(qatar_2022_events)
table(qatar_2022_events$type.name)

qatar_2022_events_cleaned <- allclean(qatar_2022_events)
glimpse(qatar_2022_events_cleaned)


#Excluye columnas seleccionadas
library(dplyr)
qatar_2022_events_cleaned_to_store <- qatar_2022_events_cleaned %>% 
  select(-c(related_events, location, tactics.lineup, 
            pass.end_location, carry.end_location,
            shot.end_location, shot.freeze_frame, goalkeeper.end_location))


# Se agregan las medidas en metros de las coordenadas de la cancha

max_x_source <- 120
max_y_source <- 80
max_x_final <- 110 # 120 yardas equivalen aprox. 110 metros
max_y_final <- 73 # 80 yardas equivalen aprox. 73 metros

qatar_2022_events_cleaned_to_store <- qatar_2022_events_cleaned_to_store %>%
  mutate(pos_x_meter = location.x/max_x_source*max_x_final,
         pos_y_meter = location.y/max_y_source*max_y_final,
         pos_y_meter = 73 - pos_y_meter,
         pass_end_pos_x_meter = pass.end_location.x/max_x_source*max_x_final,
         pass_end_pos_y_meter = pass.end_location.y/max_y_source*max_y_final,
         pass_end_pos_y_meter = 73 - pass_end_pos_y_meter,
         carry_end_pos_x_meter = carry.end_location.x/max_x_source*max_x_final,
         carry_end_pos_y_meter = carry.end_location.y/max_y_source*max_y_final,
         carry_end_pos_y_meter = 73 - carry_end_pos_y_meter)

# Exportar/Guardar datos en CSV
library(readr)
write_csv(qatar_2022_events_cleaned_to_store, "shinny_app/data/statsbomb_qatar_2022_events.csv")
write_csv(qatar_2022_games, "shinny_app/data/statsbomb_qatar_2022_games.csv")