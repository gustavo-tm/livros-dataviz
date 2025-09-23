library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(leaflet)
library(wordcloud)

ui <- fluidPage(
  
  
  tabsetPanel(
    
    tabPanel("Mapa dos autores lidos",
             leafletOutput("map", height = 800)
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
               htmlOutput("categoria_selecionada"),
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
  
  source("script/funcoes.R")
  livros_complemento <- readRDS("dados/livros_complemento.rds")
  autor_complemento <- readRDS("dados/autores_complemento.rds")
  locais_coordenadas <- readRDS("dados/locais_coordenadas.rds")
  
  livros <- readxl::read_excel("dados/usuario.xlsx") |> 
    mutate(dt_leitura = date(dt_leitura),
           ranking = ifelse(ranking == 0, NA, ranking * 2)) |> 
    select(data_leitura = dt_leitura,
           nota_usuario = ranking,
           livro_link = edicao_url,
           edicao_capa = edicao_capa_media) |> 
    left_join(livros_complemento)
  
  # Evolução leituras ----
  
  output$plot_livros <- renderPlotly({
    plotar_evolucaoLeituras(livros, variavel = "Livros lidos", input$remover_ranking_na)
  })
  output$plot_paginas <- renderPlotly({
    plotar_evolucaoLeituras(livros, variavel = "Páginas lidas", input$remover_ranking_na)
  })
  
  # Categorias ----
  
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
  
  output$categoria_selecionada <- renderText(HTML(paste0(
    "<span style='font-size:24px; font-weight:bold'>",
    categoria_selecionada(),
    "</span>")
  ))
  
  output$genero_capas <- renderUI({
    if (is.null(categoria_selecionada())) return(NULL)
    livros |> 
      select(y := !!input$plot_categorias_categoria, edicao_capa) |> 
      unnest(y) |> 
      filter(y == !!categoria_selecionada()) |> 
      pull(edicao_capa) |>
      map(\(urls) img(src = urls, height = 200, width = 150))
  })
  
  
  # MAPA ----
  
  localidades_agregadas <- agregar_localidades(autor_complemento, locais_coordenadas)
  
  output$map <- renderLeaflet({
    plotar_mapa(localidades_agregadas)
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    
    popup <- encontrar_ponto(localidades_agregadas, lat, lng)
    
    proxy <- leafletProxy("map")
    proxy  |> clearPopups() |> 
      addPopups(lng, lat, popup)
  })
  
  
  
}


shinyApp(ui, server)

