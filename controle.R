library(tidyverse)
library(future)
library(furrr)


source("raspagem.R")
future::plan(multicore, workers = availableCores() - 1)

livros <- readxl::read_excel("dados.xlsx") |> 
  select(data_leitura = dt_leitura,
         nota = ranking,
         edicao_url,
         edicao_capa = edicao_capa_media)

urls <- livros |> pull(edicao_url)
livros_complemento <- bind_rows(future_map(urls, raspar_livro))

urls_autores <- livros_complemento |> 
  pull(autor_link) |> unique()
autor_complemento <- bind_rows(future_map(urls_autores, raspar_autor))

locais_coordenadas <- autor_complemento |> 
  distinct(autor_pais,  autor_cidade) |> 
  localizar_autor()

saveRDS(livros_complemento, "livros_complemento.rds")
saveRDS(autor_complemento, "autores_complemento.rds")
saveRDS(locais_coordenadas, "locais_coordenadas.rds")

livros_complemento <- readRDS("livros_complemento.rds")
autor_complemento <- readRDS("autores_complemento.rds")
locais_coordenadas <- readRDS("locais_coordenadas.rds")


autor_complemento |> 
  left_join(locais_coordenadas) |> 
  st_as_sf(crs = "epsg:4326") |> 
  mapview()


autor_complemento |> 
  group_by(autor_pais, autor_cidade) |> 
  summarize(n = n(),
            autores = list(autor_nome))  |> ungroup() |> 
  mutate(across(c(autor_cidade, autor_pais), ~ replace_na(.x, ""))) |> 
  left_join(locais_coordenadas) |> View()

autor_locais |> drop_na() |>  st_as_sf(coords  = c("long", "lat"), crs = "epsg:4326") |> View()
  mapview::mapview()

  locais_coordenadas |> View()
