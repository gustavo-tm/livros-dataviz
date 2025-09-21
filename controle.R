library(tidyverse)

source("raspagem.R")

livros <- readxl::read_excel("dados.xlsx") |> 
  select(data_leitura = dt_leitura,
         nota = ranking,
         edicao_url,
         edicao_capa = edicao_capa_media)

urls <- livros |> pull(edicao_url)

livros_complemento <- bind_rows(map(urls, raspar_livro))

livros |> left_join(livros_complemento)

urls[31]
