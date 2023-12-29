#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("ggiraph")

library(shiny)
library(dplyr)
library(ggiraph)

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
             "Tiros" = shots,
             "Goles" = goals,
             "Asistencias" = assists,
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
             "Tiros" = shots_p90,
             "Goles" = goals_p90,
             "Asistencias" = assists_p90,
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
  
  # ------------ mapas de tiro
  
  mapa_tiros <- function(player_name){
    tiros_data = get_xg(player_name)
    tiro_player = tiros_data$xg_detalle
    
    ghp = get_half_pitch(gp = ggplot(data = tiro_player) )
    
    ghp = ghp + geom_point(aes(x = pos_x_meter, y = pos_y_meter, size = xG, fill = shot.outcome.name, shape = shot.body_part.name), 
                           alpha = 0.7) +
      # capa de estética
      scale_size_continuous(range = c(2, 8), breaks = seq(0, 1, 0.2)) +
      scale_fill_manual(values = c("red", "yellow", "green", "blue", "orange")) +
      scale_shape_manual(values = c(23, 22, 21, 24)) +
      # capa de leyendas y textos
      theme(legend.position = "top",
            legend.margin = margin(r = 0.5, unit = "cm"),
            legend.box = "vertical") +
      # capa que permite sobreescribir la parte estetica a la leyenda de los datos
      guides(fill = guide_legend(override.aes = list(shape = 21, size = 5, stroke = 1, alpha = 0.7)),
             shape = guide_legend(override.aes = list(size = 5))) +
      # permite personalizar la leyenda y los textos
      labs(fill = "Resultado del tiro:",
           size = "xG:",
           shape = "Parte del cuerpo:",
           title = "",
           subtitle = "")
    
    return(ghp)
  }
  
  output$tiros <- renderPlot({
    
    mapa_tiros(input$in_player)
    
  })
  
  output$tiros_to_facet <- renderPlot({
    
    mapa_tiros = mapa_tiros(input$in_player)
    
    mapa_tiros +
      facet_wrap(~to_facet, nrow = 2)
    
  })
  
  output$info_tiros <- renderDT({
    
    tiros_data = get_xg(input$in_player)
    tiro_player = tiros_data$xg_detalle %>% select(
      xG = shot.statsbomb_xg, 
      "Resultado" = shot.outcome.name, 
      "Parte del cuerpo" =  shot.body_part.name, 
      "Jugada" = play_pattern.name,
      "Partido" = to_facet)
    
    datatable(tiro_player, options = list(pageLength = 10, lengthChange  = FALSE))
  })
  
  # ------ mapas de pases clave
  
  pases_clave <- function(player_name){
    asistencias_data = get_xA(player_name)
    asistencias_player = asistencias_data$xA_detalle
    
    gp = get_pitch(gp = ggplot(data = asistencias_player) )
    
    gp = gp + geom_segment(aes(x = pos_x_meter, y = pos_y_meter, 
                          xend = pass_end_pos_x_meter, yend = pass_end_pos_y_meter, col = pass.goal_assist),
                      size = 0.5, linetype = 1, 
                      arrow = arrow(length = unit(0.2,"cm"), type = "closed"), 
                      alpha = 0.7) +
      # capa de estética
      scale_color_brewer(palette = "Set1") +
      geom_point(aes(x = pos_x_meter, y = pos_y_meter, col = pass.goal_assist), 
                 size = 1, pch = 4, stroke = 1.5, alpha = 0.5) + 
      # capa de leyendas y textos
      theme(legend.position = "top",
            legend.margin = margin(r = 0.5, unit = "cm"),
            legend.box = "vertical") +
      # permite personalizar la leyenda y los textos
      labs(col = "¿Fue Asistencia?:",
           title = "",
           subtitle = "Pases clave: Todo aquel pase que pudo terminar en una asistencia o terminó en asistencia, asignandole asi un xA",
           caption = "")
    
    return(gp)
  }
  
  output$pases_clave <- renderPlot({
    
    pases_clave(input$in_player)
    
  })
  
  output$pases_clave_to_facet <- renderPlot({
    
    pases_clave = pases_clave(input$in_player)
    
    pases_clave +
      facet_wrap(~to_facet, nrow = 2)
    
  })
  
  output$info_pases_clave <- renderDT({
    
    asistencias_data = get_xA(input$in_player)
    asistencias_player = asistencias_data$xA_detalle %>% select(
      xA,
      "Asistencia" = pass.goal_assist, 
      "Pase Clave" =  pass.shot_assist, 
      "Jugada" = play_pattern.name,
      "Partido" = to_facet)
    
    datatable(asistencias_player, options = list(pageLength = 10, lengthChange  = FALSE))
  })
  
  # ------------ pases progresivos
  
  pases_progresivos <- function(player_name){
    pp_data = get_pases_progresivos(player_name)
    pp_detalle = pp_data$pp_detalle
    pp_player_info = pp_data$pp_player
    
    
    gp = get_pitch(gp = ggplot(data = pp_detalle) )
    
    gp = gp + geom_segment(aes(x = pos_x_meter, y = pos_y_meter,
                          xend = pass_end_pos_x_meter, yend = pass_end_pos_y_meter, col = complete_prog_pass),
                      size = 0.5, linetype = 1,
                      arrow = arrow(length = unit(0.2,"cm"), type = "closed"),
                      alpha = 0.7) +
      # capa de estética
      scale_color_brewer(palette = "Set1") +
      geom_point(aes(x = pos_x_meter, y = pos_y_meter, col = complete_prog_pass),
                 size = 1, pch = 4, stroke = 1.5, alpha = 0.5) +
      # capa de leyendas y textos
      theme(legend.position = "top",
            legend.margin = margin(r = 0.5, unit = "cm"),
            legend.box = "vertical") +
      # permite personalizar la leyenda y los textos
      labs(col = "¿Pase preciso?:",
           title = paste0(pp_player_info$pp, " pases progresivos intentados (",
                          pp_player_info$accuracy, "% de precisión)"),
           subtitle = "",
           caption = "Pase Progresivo: Todo pase cuya posición final está al menos 25% más adelante de su posición final con respecto al arco rival")
    
    return(gp)
  }
  
  output$pases_progresivos <- renderPlot({
    
    pases_progresivos(input$in_player)

  })
  
  output$pases_progresivos_to_facet <- renderPlot({
    
    pases_progresivos = pases_progresivos(input$in_player)
    
    pases_progresivos +
      facet_wrap(~to_facet, nrow = 2)
    
  })
  
  output$info_pp_clave <- renderDT({
    
    pp_data = get_pases_progresivos(input$in_player)
    pp_detalle = pp_data$pp_detalle %>% select(
      "Pase Preciso" = complete_prog_pass,
      "Tipo de pase" = pass.height.name, 
      "Asistencia" =  pass.goal_assist, 
      "Jugada" = play_pattern.name,
      "Partido" = to_facet)
    
    datatable(pp_detalle, options = list(pageLength = 10, lengthChange  = FALSE))
  })
  
  # ----------- mapa de recuperaciones
  
  recuperaciones <- function(player_name){
    
    recuperaciones = get_recuperaciones(player_name)
    recuperaciones_detalles = recuperaciones$recuperaciones_detalle
    recuperaciones_player = recuperaciones$recuperaciones_player
    
    gp <- get_pitch(gp = ggplot(data = recuperaciones_detalles), dims = dims, margin = 0.6, pitch_col = "grey50", pitch_fill = "black") +
      geom_density2d_filled(aes(x = pos_x_meter, y = pos_y_meter, fill = after_stat(level)),
                            alpha = 0.7,
                            contour_var = "ndensity", 
                            breaks = seq(0.1, 1.0, length.out = 10)) +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "YlOrRd", direction = -1) +
      geom_point(mapping = aes(x = pos_x_meter, y = pos_y_meter), fill = "blue", 
                 pch = 4, alpha = 0.7, col = "blue", stroke = 1.4) +
      ggtitle(label = paste0(recuperaciones_player$recuperaciones, " recuperaciones \n"))
    
    return(gp)
    
  }
  
  output$recuperaciones <- renderPlot({
    
    recuperaciones(input$in_player)
    
  })
  
  output$recuperaciones_to_facet <- renderPlot({
    
    recuperaciones = recuperaciones(input$in_player)
    
    recuperaciones +
      facet_wrap(~to_facet, nrow = 2)
    
  })
  
  output$info_recuperaciones <- renderDT({
    
    recuperaciones = get_recuperaciones(player_name)
    recuperaciones_detalles = recuperaciones$recuperaciones_detalle %>% select(
      "Jugada" = play_pattern.name,
      "Partido" = to_facet)
    
    datatable(recuperaciones_detalles, options = list(pageLength = 10, lengthChange  = FALSE))
  })

}
