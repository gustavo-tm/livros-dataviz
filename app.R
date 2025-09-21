library(shiny)
library(tidyverse)
library(plotly)

source("funcoes.R")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Evolução de Leituras",
             # titlePanel("Teste"),
             plotlyOutput("plot_livros"),
             plotlyOutput("plot_paginas"),
             checkboxInput("remover_ranking_na", "Remover leituras não avalidas", value = F)),
    tabPanel("Livros favoritos"
    )
  )
)

server <- function(input, output, session) {
  output$plot_livros <- renderPlotly({
    plotar(livros, variavel = "Livros lidos", input$remover_ranking_na)
  })
  output$plot_paginas <- renderPlotly({
    plotar(livros, variavel = "Páginas lidas", input$remover_ranking_na)
  })

}



shinyApp(ui, server)

