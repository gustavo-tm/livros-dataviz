library(tidyverse)
library(rvest)


# Página do livro ----
raspar_livro <- function(url){
  tryCatch(expr = {
    html <- read_html(paste0("https://www.skoob.com.br", url))
    
    barra_lateral <- html |> html_elements("#pg-livro-menu-principal") 
    
    titulo <- html_elements(barra_lateral, ".sidebar-titulo")
    titulo <- titulo |> html_text2()
    
    autor <- html_elements(barra_lateral, "a")[2]
    autor.nome <- autor |> html_text2()
    autor.link <- autor |> html_attr("href")
    
    descricao <- html_elements(barra_lateral, ".sidebar-desc")
    descricao.ano <- descricao |> html_text2() |> str_split_i('Ano: ', i = 2) |> str_extract("^\\d+") |> as.numeric()
    descricao.paginas <- descricao |> html_text2() |> str_split_i('Páginas: ', i = 2) |> str_extract("^\\d+") |> as.numeric()
    descricao.idioma <- descricao |> html_text2() |> str_split_i('Idioma: ', i = 2) |> str_extract("^\\w+")
    
    barra_lateral_menu <- html_elements(barra_lateral, "#ul-menu-vertical-badges")
    edicoes <- html_elements(barra_lateral_menu, "a")[2] |> html_attr("href")
    
    barra_principal <- html |> html_elements("#pg-livro-principal-container") 
    
    perfil <- barra_principal |> html_elements("p[itemprop='description']") |> html_text2() |> gsub("[\n\r]", "", x = _) |> str_split_1("\\.")
    perfil.sinopse <- perfil[1:length(perfil)-1] |> paste0(collapse = "")
    perfil.genero <- perfil[length(perfil)] |> str_split_1(" / ")
    
    tags <- barra_principal |> html_elements("#tags_populares") |> html_elements("a") |> html_text2()
    
    tibble(livro_link = url,
           titulo = titulo,
           autor = autor.nome,
           autor_link =  autor.link,
           ano = descricao.ano,
           paginas = descricao.paginas,
           idioma = descricao.idioma,
           edicoes_link = edicoes,
           sinopse = perfil.sinopse,
           genero = list(perfil.genero),
           tags = list(tags))
  },
  error = {
    tibble(NULL)
  })
  
}

