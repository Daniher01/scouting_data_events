#------------------ obtencion de datos
install.packages("here")
install.packages("beepr")
devtools::install_github("abhiamishra/ggshakeR")

library(ggshakeR)
library(readr)
library(here)
library(beepr)


# Leer el archivo CSV utilizando una ruta relativa con 'here'
games <- read_csv(here("shinny_app", "data", "statsbomb_liga_15_16_games.csv"))
events <- read_csv(here("shinny_app", "data", "statsbomb_liga_15_16_events.csv"))


# ------------------ analisis del jugador --------------------------

minutos_jugados <- events %>%
  filter(!is.na(player.name)) %>%
  group_by(match_id, player.name) %>%
  summarise(max_time = max(ElapsedTime, na.rm = T)) %>%
  group_by(player.name) %>%
  summarise(minutos_totales = round(sum(max_time)/60, 1))  %>%
  arrange(desc(minutos_totales))

get_info_historica <- function(player_name){
  # obtener datos histicos posicion, nombre, edad, equipo
}

# -------------------------------- METRICAS OFENSIVAS

get_xg <- function(player_name){
  xG = events %>%
    filter(!is.na(shot.statsbomb_xg), player.name == player_name) %>%
    select(player.name, xG = shot.statsbomb_xg, play_pattern.name, shot.technique.name, shot.body_part.name, shot.outcome.name, pos_x_meter, pos_y_meter, match_id)
  
  xG_player = xG %>%
    group_by(player.name) %>%
    summarise(xG = sum(xG))
    
  
  return(list(xg_detalle = xG, xG_player = xG_player))
}

get_xA <- function(player_name){

  xGA_process = events %>%
    filter(type.name=="Shot") %>% #1
    select(shot.key_pass_id, xA = shot.statsbomb_xg) #2
  
  xA = inner_join(events, xGA_process, by = c("id" = "shot.key_pass_id")) %>% #3
    select(player.name, pass.shot_assist, pass.goal_assist, xA, pos_x_meter, pos_y_meter, pass_end_pos_x_meter, pass_end_pos_y_meter) %>%
    filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE, player.name == player_name) #4
  
  xA_player = xA %>%
    group_by(player.name) %>%
    summarise(xA = sum(xA))
  
  return(list(xA_detalle = xA, xA_player = xA_player))
    
}

get_dribles <- function(player_name){
  
  dribles = events %>%
    filter(!is.na(dribble.outcome.name), player.name == player_name) %>%
    group_by(player.name) %>%
    summarise(n = n(),
              n_complete = sum(ifelse(dribble.outcome.name == "Complete", 1, 0)),
              n_incomplete = n - n_complete,
              accurracy = round(n_complete/n*100, 2))
  
  return(dribles)
  
}


# PARA METRICAS OFENSIVAS  

# metricas_ofensivas = minutos_jugados %>%
#   left_join(xG, by = "player.name") %>%
#   left_join(xA, by = "player.name") %>%
#   left_join(goals, by = "player.name") %>%
#   mutate(across(-player.name, ~replace(., is.na(.), 0)))


# metricas_ofensivas_p90 = metricas_ofensivas %>%
#   mutate(across(-player.name, ~(.x/minutos_totales*90), .names = "{.col}_p90"))


# ---------------- METRICAS DE CONSTRUCCIÓN

get_pases_progresivos <- function(player_name){
  
  pases_progresivos <- events %>%
    mutate(progressive_pass = ifelse(type.name == "Pass" & 
                                       (110 - pass_end_pos_x_meter)/(110 - pos_x_meter) <= 0.75, "Yes", "No"),
           complete_prog_pass = ifelse(is.na(pass.outcome.name), "Yes", "No")) %>%
    filter(progressive_pass == "Yes", player.name == player_name) %>%
    select(player.name, progressive_pass, complete_prog_pass, pos_x_meter, pos_y_meter, pass_end_pos_x_meter, pass_end_pos_y_meter)
  
  pp_player = pases_progresivos %>%
    group_by(player.name) %>%
    summarise(n = n(),
              n_complete = sum(ifelse(complete_prog_pass == "Yes", 1, 0)),
              n_incomplete = n - n_complete,
              accuracy = round(n_complete/n*100, 1)) %>%
    arrange(desc(accuracy)) %>%
    filter(n >= 50) %>%
    select(player.name, pp = n, n_complete, n_incomplete, accuracy)
  
  return(list(pp_detalle = pases_progresivos, pp_player = pp_player))
  
}

get_balones_perdidos <- function(player_name){
  
  balones_perdidos = events %>%
    filter(type.name == "Pass", pass.outcome.name != "Complete" | type.name == "Miscontrol", player.name == player_name) %>%
    group_by(player.name) %>%
    summarise(balones_perdidos = n())
  
  return(balones_perdidos)
  
}

get_xT <- function(player_name){
  events_with_start_and_end_pos = c("Pass", "Carry")

  events_info_for_xt = events %>%
    filter(type.name %in% events_with_start_and_end_pos, player.name == "Toni Kroos") %>%
    rename("x" = "location.x",
           "y" = "location.y",
           "finalX" = "pass.end_location.x",
           "finalY" = "pass.end_location.y") %>%
    mutate(finalX_event = case_when(type.name == "Pass" ~ "pass_end_location_x",
                                    type.name == "Carry" ~ "carry_end_location_x"),
           finalY_event = case_when(type.name == "Pass" ~ "pass_end_location_y",
                                    type.name == "Carry" ~ "carry_end_location_y"))

  events_with_xt = calculate_threat(data = events_info_for_xt, type = "statsbomb") %>%
    mutate(xt = xTEnd - xTStart)
  beep("mario")

}

# ------------------ METRICAS DEFENSIVAS

# intercepciones
intercepciones = events %>%
  filter(type.name == "Interception")

# balones recuperados
recuperaciones = events %>%
  filter(type.name == "Ball Recovery") %>%
  group_by(player.name) %>%
  summarise(recuperaciones = n())

presiones = events %>%
  filter(type.name == "Pressure")



