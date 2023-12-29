#------------------ obtencion de datos
# install.packages("here")
# install.packages("beepr")
# devtools::install_github("abhiamishra/ggshakeR")

library(ggshakeR)
library(readr)
library(here)
library(beepr)
library(dplyr)
library(ggplot2)

source("cancha.R")

# usar para pruebas 
# games <- read_csv(here("shinny_app", "data", "statsbomb_qatar_2022_games.csv"))
# events <- read_csv(here("shinny_app", "data", "statsbomb_qatar_2022_events.csv"))

# usar para deploy
events <- read_csv("./data/statsbomb_qatar_2022_events.csv")
games <- read_csv("./data/statsbomb_qatar_2022_games.csv")


# agregar faceta de juego
events = events %>%
  left_join(games,
            select(match_id, match_date, home_team.home_team_name, away_team.away_team_name, competition_stage.name, home_score, away_score),
            by = "match_id") %>%
  mutate(to_facet = paste0(match_date, " vs ",
                           ifelse(home_team.home_team_name == team.name,
                                  away_team.away_team_name, home_team.home_team_name),
                           " (", competition_stage.name, ")"))

# agregar contexto de los goles
events = events %>%
  filter(period != 5) %>% # period = 5 corresponde a definición a penales
  mutate(is_goal = ifelse(is.na(shot.outcome.name) | shot.outcome.name != "Goal", 0, 1),
         is_own_goal_for = ifelse(type.name == "Own Goal For", 1, 0)) %>%
  arrange(match_id, ElapsedTime) %>%
  # agrega el marcados de manera acumulada en cada evento
  group_by(match_id) %>%
  mutate(home_team_score = cumsum(ifelse(team.name == home_team.home_team_name & (is_goal == 1 | is_own_goal_for == 1), 1, 0)),
         away_team_score = cumsum(ifelse(team.name == away_team.away_team_name & (is_goal == 1 | is_own_goal_for == 1), 1, 0))) %>%
  # agregar contexto si es ganando perdiendo o empatando
  mutate(contexto = case_when((team.name == home_team.home_team_name & home_team_score > away_team_score) |
                                              (team.name == away_team.away_team_name & home_team_score < away_team_score) ~ "ganando",

                                            (team.name == home_team.home_team_name & home_team_score < away_team_score) |
                                              (team.name == away_team.away_team_name & home_team_score > away_team_score) ~ "perdiendo",
                                            T ~ "empatando"),
         tiempo_juego = paste(minute, second, sep = ":")) %>%
  ungroup()


# -------------------- DATOS PARA INPUT
data_input <- function(){
  
  players = events %>% 
    distinct(player.name, .keep_all = TRUE) %>% 
    select(player.name, team.name) %>% 
    filter(!is.na(player.name)) %>%
    arrange(team.name)
  
  return(players)
}

# -------------------------------- METRICAS OFENSIVAS

get_xg <- function(player_name){
  xG = events %>%
    filter(!is.na(shot.statsbomb_xg), player.name == player_name) %>%
    mutate(xG = shot.statsbomb_xg) %>%
    left_join(games, by = "match_id")
  
  xG_player = xG %>%
    group_by(player.name) %>%
    summarise(xG = round(sum(xG),2) )%>% 
    filter(player.name == player_name)


  
  return(list(xg_detalle = xG, xG_player = xG_player))
}

get_xA <- function(player_name){

  xGA_process = events %>%
    filter(type.name=="Shot") %>% #1
    select(shot.key_pass_id, xA = shot.statsbomb_xg) #2
  
  xA = inner_join(events, xGA_process, by = c("id" = "shot.key_pass_id")) %>% #3

    filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE) %>%
    mutate(across(pass.goal_assist, ~replace(.x, is.na(.x), FALSE)),
           across(pass.shot_assist, ~replace(.x, is.na(.x), FALSE))) %>% 
    filter(player.name == player_name)
  
  
  xA_player = xA %>%
    group_by(player.name) %>%
    summarise(xA = round(sum(xA), 2)) %>% 
    filter(player.name == player_name)

  
  return(list(xA_detalle = xA, xA_player = xA_player))
    
}

get_shots <- function(player_name){
  
  shots = events %>%
    filter(type.name=="Shot") %>% #1
    group_by(player.name) %>%
    summarise(shots = n()) %>% 
    filter(player.name == player_name)
  
  return(shots)
  
}

get_assists <- function(player_name){

  assists = events %>%
    filter(type.name == "Pass", pass.goal_assist == TRUE) %>%
    group_by(player.name) %>%
    summarise(assists = n()) %>% 
    filter(player.name == player_name)
  
  return(assists)
}

get_goals <- function(player_name){
  
  goals = events %>%
    filter(type.name == "Shot", shot.outcome.name == "Goal") %>%
    group_by(player.name) %>%
    summarise(goals = n()) %>% 
    filter(player.name == player_name)
  
  return(goals)
}


# ---------------- METRICAS DE CONSTRUCCIÓN DE JUEGO

get_pases_progresivos <- function(player_name){
  
  pases_progresivos <- events %>%
    mutate(progressive_pass = ifelse(type.name == "Pass" & 
                                       (110 - pass_end_pos_x_meter)/(110 - pos_x_meter) <= 0.75, "Yes", "No"),
           complete_prog_pass = ifelse(is.na(pass.outcome.name), "Yes", "No"),
           pass.goal_assist = ifelse(!is.na(pass.goal_assist), "Yes", "No")) %>%
    filter(progressive_pass == "Yes", player.name == player_name)
  
  pp_player = pases_progresivos %>%
    group_by(player.name) %>%
    summarise(n = n(),
              n_complete = sum(ifelse(complete_prog_pass == "Yes", 1, 0)),
              n_incomplete = n - n_complete,
              accuracy = round(n_complete/n*100, 1)) %>%
    select(player.name, pp = n, n_complete, n_incomplete, accuracy) %>% 
    filter(player.name == player_name)

  
  return(list(pp_detalle = pases_progresivos, pp_player = pp_player))
  
}

get_dribles <- function(player_name){
  
  dribles = events %>%
    filter(!is.na(dribble.outcome.name)) %>%
    group_by(player.name) %>%
    summarise(n = n(),
              n_complete = sum(ifelse(dribble.outcome.name == "Complete", 1, 0)),
              n_incomplete = n - n_complete,
              accurracy = round(n_complete/n*100, 2)) %>% 
    filter(player.name == player_name)
  
  return(dribles)
  
}

get_xt <- function(player_name){

  
  events_with_start_and_end_pos = c("Pass", "Carry")

  events_info_for_xt = events %>%
    filter(type.name %in% events_with_start_and_end_pos) %>%
    rename("x" = "pos_x_meter",
           "y" = "pos_y_meter") %>%
    mutate(finalX = case_when(type.name == "Pass" ~ pass_end_pos_x_meter,
                              type.name == "Carry" ~ carry_end_pos_x_meter),
           finalY = case_when(type.name == "Pass" ~ pass_end_pos_y_meter,
                              type.name == "Carry" ~ carry_end_pos_y_meter))  %>%
    filter(player.name == player_name)

  events_with_xt = calculate_threat(data = events_info_for_xt, type = "statsbomb") %>%
    mutate(xt = xTEnd - xTStart) %>%
  filter(!is.na(xt))
  

  xT_player = events_with_xt %>%
    group_by(player.name) %>%
    summarise(xT = round(sum(xt), 2))
  

  return(list(xt_data = events_with_xt, xT_player = xT_player))

}

# ------------------ METRICAS DEFENSIVAS

get_intercepciones <- function(player_name){
  
  intercepciones = events %>%
    filter(type.name == "Interception") %>%
    group_by(player.name) %>%
    summarise(intercepciones = n())  %>% 
    filter(player.name == player_name)
  
  return(intercepciones)
  
}

get_recuperaciones <- function(player_name){
 
  recuperaciones = events %>%
    filter(type.name == "Ball Recovery", player.name == player_name)
  
  recuperaciones_player = recuperaciones  %>%
    group_by(player.name) %>%
    summarise(recuperaciones = n())
  
  return(list(recuperaciones_detalle = recuperaciones, recuperaciones_player = recuperaciones_player))
}

get_presiones <- function(player_name){
  
  presiones = events %>%
    filter(type.name == "Pressure") %>%
    group_by(player.name) %>%
    summarise(presiones = n()) %>% 
    filter(player.name == player_name)

  return(presiones)
}

# -------------- METRICAS P90
get_metricas_p90 <- function(player_name){
  
  
  # minutos jugados
  minutos_jugados <- events %>%
    group_by(match_id, player.name) %>%
    summarise(max_time = max(ElapsedTime, na.rm = T)) %>%
    group_by(player.name) %>%
    summarise(minutos_totales = round(sum(max_time)/60, 1)) 
  
  if(!is.null(player_name)){
    minutos_jugados = minutos_jugados %>% filter(player.name == player_name)
  }
  
  # ofensivas
  xG = get_xg(player_name)
  xG = xG$xG_player
  xA = get_xA(player_name)
  xA = xA$xA_player
  shots = get_shots(player_name)
  assists = get_assists(player_name)
  goals = get_goals(player_name)
  # construcción de juego
  pases_progresivos = get_pases_progresivos(player_name)
  pases_progresivos = pases_progresivos$pp_player %>% select(player.name, pases_progresivos = pp)
  dribles = get_dribles(player_name) 
  dribles = dribles %>% select(player.name, dribles = n)
  xT = get_xt(player_name)
  xT = xT$xT_player
  # defensivas
  intercepciones = get_intercepciones(player_name)
  recuperaciones = get_recuperaciones(player_name)
  recuperaciones = recuperaciones$recuperaciones_player
  presiones = get_presiones(player_name)
  

  metricas_p90 = minutos_jugados %>%
    left_join(xG, by = "player.name") %>%
    left_join(xA, by = "player.name") %>%
    left_join(shots, by = "player.name") %>%
    left_join(assists, by = "player.name") %>%
    left_join(goals, by = "player.name") %>%
    left_join(pases_progresivos, by = "player.name") %>%
    left_join(dribles, by = "player.name") %>%
    left_join(xT, by = "player.name") %>%
    left_join(intercepciones, by = "player.name") %>%
    left_join(recuperaciones, by = "player.name") %>%
    left_join(presiones, by = "player.name") %>%
    mutate(across(-player.name, ~replace(., is.na(.), 0))) %>%
    mutate(across(-c(player.name, minutos_totales), ~(round(.x/minutos_totales*90, 2)), .names = "{.col}_p90"))
  
  return(metricas_p90)
}



