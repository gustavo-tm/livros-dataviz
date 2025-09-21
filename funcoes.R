livros <- readxl::read_excel("dados.xlsx") |> 
  mutate(dt_leitura = date(dt_leitura),
         ranking = ifelse(ranking == 0, NA, ranking * 2),
         nome_ranqueado = paste0("(", ranking, ") ", edicao_titulo))

plotar <- function(livros, variavel, remover_ranking_na){
  
  gg <- livros |>
    drop_na(dt_leitura) |> 
    
    # Remover ou manter ranking NA
    (\(df) if(remover_ranking_na == TRUE){
      df |> drop_na(ranking)
    }else{df})() |> 
    
    group_by(ano = year(dt_leitura), mes = month(dt_leitura), ranking) |> 
    summarize(n = n(),
              paginas = sum(edicao_paginas)) |> ungroup() |>  
    mutate(mes_ano = make_date(year = ano, month = mes)) |> 
    complete(mes_ano = seq(min(mes_ano), max(mes_ano), by = "1 month"), ranking, fill = list(n = 0, paginas = 0)) |> 
    arrange(mes_ano) |> 
    group_by(ranking) |> 
    mutate(livros = cumsum(n),
           paginas = cumsum(paginas)) |> 
    ungroup() |> 
    rename("Páginas lidas" = paginas,
           "Livros lidos" = livros) |> 
    ggplot(aes(x = mes_ano, y = !!sym(variavel), fill = factor(ranking, levels = rev(1:10)))) +
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

mostrar_capa <- function(url){
  image_read(url)
}










