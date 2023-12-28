#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

source("analisis_datos.R")
data <- data_input()

# Define server logic required to draw a histogram
function(input, output, session) {

  observeEvent(input$in_team, {
    # Filtrar las ligas basado en la temporada seleccionada
    players <- data %>% filter(team.name == input$in_team) %>% select(player.name)


    # Actualizar el input de la liga
    updateSelectInput(session, "in_player", choices = players)
  })
  
  # ---------- info a mostrar
  
  output$player_name <- renderUI({
    tags$h1(input$in_player)
  })

  
  output$info_tabla <- render_gt({
    data_info_player <- get_metricas_p90(input$in_player) %>%
      select("Minutos Jugados" = minutos_totales,
             "Goles" = goals,
             "Asistencias" = assists,
             "Tiros" = shots,
             xG, xA, xT,
             "Dribles" = dribles,
             "Pases Progresivos" = pases_progresivos,
             "Recuperaciones" = recuperaciones, 
             "Intercepciones" = intercepciones,
             "Presiones" = presiones)
    
    gt(data_info_player)  %>% 
      tab_header(
        title = md("Métricas del torneo")
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(columns = everything())
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = everything())
      )
  })
  
  output$info_p90_tabla <- render_gt({
    data_info_player <- get_metricas_p90(input$in_player) %>%
      select("Minutos Jugados" = minutos_totales,
             "Goles" = goals_p90,
             "Asistencias" = assists_p90,
             "Tiros" = shots_p90,
             "xG" = xG_p90, 
            "xA" = xA_p90, 
             "xT" = xT_p90,
             "Dribles" = dribles_p90,
             "Pases Progresivos" = pases_progresivos_p90,
             "Recuperaciones" = recuperaciones_p90, 
             "Intercepciones" = intercepciones_p90,
             "Presiones" = presiones_p90)
    
    gt(data_info_player)  %>% 
      tab_header(
        title = md("Métricas cada 90 minutos")
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(columns = everything())
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = everything())
      )
  })

}
