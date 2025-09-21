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
    tabPanel("Livros favoritos",
             selectInput("livro", "Qual livro quer ver?", 
                         livros |>  arrange(-ranking) |> pull(nome_ranqueado)),
             uiOutput("livro_favorito_capa"),
             textOutput("livro_favorito_sinopse")
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
  
  output$livro_favorito_capa <- renderUI({
    tags$img(src = livros |> filter(nome_ranqueado == input$livro) |> pull(edicao_capa_media))
  })
  
  output$livro_favorito_sinopse <- renderText({ 
    livros |> filter(nome_ranqueado == input$livro) |> pull(edicao_sinopse)
  })

}



shinyApp(ui, server)

