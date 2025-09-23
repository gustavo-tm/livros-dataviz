library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(leaflet)

source("funcoes.R")

ui <- fluidPage(
  
  
  tabsetPanel(
    
    tabPanel("Mapa dos autores lidos",
             leafletOutput("mapa_autores", height = 800)
    ),
    
    tabPanel("Categorias mais lidas",
             fluidRow(
               column(4,
                      radioButtons("plot_categorias_categoria", "Escolha a divisão de categorias",
                                   choiceNames = c("Gênero", "Tags"),
                                   choiceValues = c("genero", "tags"))),
               column(4,
                      radioButtons("plot_categorias_tipo", "Escolha qual tipo de gráfico",
                                   choiceNames = c("Gráfico de barras", "Wordcloud"),
                                   choiceValues = c("barplot", "wordcloud"))),
               column(4,
                      sliderInput("plot_categorias_minimo_ocorrencias", "Mínimo de ocorrências para aparecer no gráfico",
                                  value = 2, min = 1, max = 10),)
             ),
             
             conditionalPanel(
               condition = "input.plot_categorias_tipo == 'barplot'",
               plotlyOutput("plot_categorias_barplot", height = 600),
               textOutput("categoria_selecionada"),
               uiOutput("genero_capas")
             ),
             
             conditionalPanel(
               condition = "input.plot_categorias_tipo == 'wordcloud'",
               plotOutput("plot_categorias_wordcloud", height = 600)
             )
    ),

    tabPanel("Evolução de Leituras",
             plotlyOutput("plot_livros"),
             plotlyOutput("plot_paginas"),
             checkboxInput("remover_ranking_na", "Remover leituras não avalidas", value = F))
  )
)

server <- function(input, output, session) {
  
  # Evolução leituras
  output$plot_livros <- renderPlotly({
    plotar_evolucaoLeituras(livros, variavel = "Livros lidos", input$remover_ranking_na)
  })
  output$plot_paginas <- renderPlotly({
    plotar_evolucaoLeituras(livros, variavel = "Páginas lidas", input$remover_ranking_na)
  })
  
  # Categorias
  frequencias <- reactive({
    calcular_frequencias_categoria(
      livros,
      categoria = input$plot_categorias_categoria,
      frequencia_minima = input$plot_categorias_minimo_ocorrencias
    )
  })
  
  output$plot_categorias_barplot <- renderPlotly(plotar_categorias(frequencias(), tipo = "barplot"))
  output$plot_categorias_wordcloud <- renderPlot(plotar_categorias(frequencias(), tipo = "wordcloud"))
  
  
  categoria_selecionada <- reactive({
    click <- event_data("plotly_click", source = "plot_categorias")$key
    if (is.null(click)) return(NULL)
    click
  })
  
  output$categoria_selecionada <- renderText(categoria_selecionada())
  
  output$genero_capas <- renderUI({
    if (is.null(categoria_selecionada())) return(NULL)
    livros |> 
      select(y := !!input$plot_categorias_categoria, edicao_capa) |> 
      unnest(y) |> 
      filter(y == !!categoria_selecionada()) |> 
      pull(edicao_capa) |>
      map(\(urls) img(src = urls, height = 200, width = 150))
  })
  
  output$mapa_autores <- renderLeaflet({
    plotar_mapa(autor_complemento, locais_coordenadas)
  })
  
}


shinyApp(ui, server)

