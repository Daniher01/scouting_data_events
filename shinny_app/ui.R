#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(gt)

source("analisis_datos.R")
data <- data_input()

dashboardPage(
  dashboardHeader( title = "Rendimiento de jugadores Qatar 2022", titleWidth = "20%"),
  dashboardSidebar(width = "15%",
    
    selectInput("in_team",
                "Equipo",
                choices = unique(data$team.name)),
    
    selectInput("in_player",
                "Jugador:",
                choices = NULL)
  ),
  dashboardBody(
    fluidRow(
      column( width = 3,
        tags$img(src = "imagen_player.jpg", height = "50%", width = "50%"),
      ),
      column( width = 9,
              uiOutput("player_name"),
              tabBox( width = "100%",
                tabPanel("Métricas del torneo", tableOutput("info_tabla")),
                tabPanel("Métricas del torneo p90", tableOutput("info_p90_tabla"))
              )
      )
    ),
    tabBox(width = "100%",
           tabPanel("Mapa de Tiros", 
                    fluidRow(
                      box(width = 6,  DTOutput("info_tiros")),
                      box(width = 6, tabBox(width = "100%",
                                            tabPanel("Por Torneo" ,plotOutput("tiros")),
                                            tabPanel("Por Partido",  plotOutput("tiros_to_Facet"))
                                            )))),
           tabPanel("Pases Clave",
                    fluidRow(
                      box(width = 6,  DTOutput("info_pases_clave")),
                      box(width = 6,  plotOutput("pases_clave"))
                    )),
           tabPanel("Pases Progresivos",
                    fluidRow(
                      box(width = 6,  DTOutput("info_pp_clave")),
                      box(width = 6,  plotOutput("pases_progresivos"))
                    )),
           tabPanel("Grafico 4"),
           )
  )
)
