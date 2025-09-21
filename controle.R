library(tidyverse)
library(future)
library(furrr)

source("raspagem.R")

livros <- readxl::read_excel("dados.xlsx") |> 
  select(data_leitura = dt_leitura,
         nota = ranking,
         edicao_url,
         edicao_capa = edicao_capa_media)

urls <- livros |> pull(edicao_url)
url <- urls[1]

future::plan(multicore, workers = 8)
livros_complemento <- bind_rows(future_map(urls, raspar_livro))

saveRDS(livros_complemento, "livros_complemento.RDS")

write_csv(livros_complemento, "livros_complemento.csv")
livros |> left_join(livros_complemento)

raspar_livro(urls[31])






