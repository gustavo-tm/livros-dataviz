livros <- readxl::read_excel("dados.xlsx") |> 
  mutate(dt_leitura = date(dt_leitura),
         ranking = ifelse(ranking == 0, NA, ranking * 2)) |> 
  select(data_leitura = dt_leitura,
         nota_usuario = ranking,
         livro_link = edicao_url,
         edicao_capa = edicao_capa_media) |> 
  left_join(readRDS("livros_complemento.RDS"))

plotar_evolucaoLeituras <- function(livros, variavel, remover_ranking_na){
  
  gg <- livros |>
    drop_na(data_leitura) |> 
    
    # Remover ou manter ranking NA
    (\(df) if(remover_ranking_na == TRUE){
      df |> drop_na(nota_usuario)
    }else{df})() |> 
    
    group_by(ano = year(data_leitura), mes = month(data_leitura), nota_usuario) |> 
    summarize(n = n(),
              paginas = sum(paginas)) |> ungroup() |>  
    mutate(mes_ano = make_date(year = ano, month = mes)) |> 
    complete(mes_ano = seq(min(mes_ano), max(mes_ano), by = "1 month"), nota_usuario, fill = list(n = 0, paginas = 0)) |> 
    arrange(mes_ano) |> 
    group_by(nota_usuario) |> 
    mutate(livros = cumsum(n),
           paginas = cumsum(paginas)) |> 
    ungroup() |> 
    rename("Páginas lidas" = paginas,
           "Livros lidos" = livros) |> 
    ggplot(aes(x = mes_ano, y = !!sym(variavel), fill = factor(nota_usuario, levels = rev(1:10)))) +
    geom_area() +
    scale_fill_manual("Nota atribuída",
                      values = list("10" = adjustcolor("gold", green.f = 1, offset = c(0, 0, .0, 0)), 
                                    "9" = adjustcolor("gold", green.f = .9, offset = c(0, 0, .05, 0)),
                                    "8" = adjustcolor("gold", green.f = .8, offset = c(0, 0, .1, 0)),
                                    "7" = adjustcolor("gold", green.f = .7, offset = c(0, 0, .15, 0)),
                                    "6" = adjustcolor("gold", green.f = .6, offset = c(0, 0, .2, 0)),
                                    "5" = adjustcolor("gold", green.f = .5, offset = c(0, 0, .25, 0))),
                      na.value = "grey90") +
    scale_x_date("",
                 date_breaks = "1 year",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y") +
    scale_y_continuous("") +
    labs(title = variavel) +
    theme_minimal()
  
  ggplotly(gg)
}

calcular_frequencias_categoria <- function(livros, categoria, frequencia_minima){
  frequencias <- livros |> 
    select(y := !!categoria) |> 
    unnest(y) |> 
    group_by(y) |> 
    summarize(n = n()) |> 
    filter(n >= frequencia_minima) |> 
    drop_na()
}

plotar_categorias <- function(frequencias, tipo){
  if(tipo == "barplot"){
    ggplotly(
      ggplot(frequencias) +
        geom_col(aes(y = reorder(y, n), x = n, key = y)) +
        theme_minimal() +
        labs(y = "", x = ""),
      source = "plot_categorias")
  }else if(TRUE){
    wordcloud(frequencias$y, frequencias$n, rot.per = 0, min.freq = 0, random.color = T)
  }
}

