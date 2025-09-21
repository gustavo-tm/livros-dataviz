library(shiny)
library(tidyverse)
library(plotly)

source("funcoes.R")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Livros favoritos",
             plotlyOutput("plot_generos", height = 600),
             uiOutput("genero_capas")),
    tabPanel("Evolução de Leituras",
             # titlePanel("Teste"),
             plotlyOutput("plot_livros"),
             plotlyOutput("plot_paginas"),
             checkboxInput("remover_ranking_na", "Remover leituras não avalidas", value = F))
  )
)

server <- function(input, output, session) {
  output$plot_livros <- renderPlotly({
    plotar_evolucaoLeituras(livros, variavel = "Livros lidos", input$remover_ranking_na)
  })
  output$plot_paginas <- renderPlotly({
    plotar_evolucaoLeituras(livros, variavel = "Páginas lidas", input$remover_ranking_na)
  })
  
  output$plot_generos <- renderPlotly({
    plotar_generos(livros)
  })
  
  genero_selecionado <- reactive({
    click <- event_data("plotly_click", source = "plot_generos")
    if (is.null(click)) return(NULL)
    
    livros |> 
      select(genero) |> 
      unnest(genero) |> 
      group_by(genero) |> 
      summarize(n = n()) |> 
      drop_na() |> 
      arrange(n) |> 
      slice(floor(click[["y"]])) |> 
      pull(genero)
  })
  
  output$genero_capas <- renderUI({
    if (is.null(genero_selecionado())) return(NULL)
    livros |> unnest(genero) |> filter(genero == !!genero_selecionado()) |> pull(edicao_capa) |> 
      map(\(urls) img(src = urls, height = 200, width = 150))
    
  })
}


shinyApp(ui, server)

