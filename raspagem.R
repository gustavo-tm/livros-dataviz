library(tidyverse)
library(rvest)


# Página do livro ----
raspar_livro <- function(url){
  tryCatch(
    {
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
      
      status <- barra_principal |> html_elements("#livro-perfil-status")
      status.rating  <- status |> html_elements(".rating") |> html_text2() |> as.numeric() |> (\(.) . * 2)()
      status.avaliacoes  <- status |> html_elements("#pg-livro-box-rating-avaliadores-numero") |> html_text2() |> str_extract("^\\d+") |> as.numeric()
      status.leitores <- status |> html_elements(".text_blue") |> html_text2() |> str_remove_all("\\.")
      status.leram <- status.leitores[6] |> as.numeric()
      status.lendo <- status.leitores[5] |> as.numeric()
      status.queremler <- status.leitores[4] |> as.numeric()
      status.abandonaram <- status.leitores[2] |> as.numeric()
      status.relendo <- status.leitores[3] |> as.numeric()
      
      perfil <- tryCatch(
        barra_principal |> html_elements("p[itemprop='description']") |> html_text2() |> gsub("[\n\r]", "", x = _) |> str_split_1("\\."),
        error = function(e){NA} 
      )
      perfil.sinopse <- tryCatch(
        perfil[1:length(perfil)-1] |> paste0(collapse = ""),
        error = function(e){NA} 
      )
      perfil.genero <- tryCatch(
        barra_principal |> html_elements("p[itemprop='description']") |> html_elements("span") |> (\(.) .[1])() |> html_text2() |> str_split_1(" / "),
        error = function(e){NA} 
      )
      
      tags <- tryCatch(
        barra_principal |> html_elements("#tags_populares") |> html_elements("a") |> html_text2(),
        error = function(e){NA} 
      )
      
      percent_homem <- tryCatch(
        barra_principal |> html_elements(".pg-livro-icone-male-label") |> html_text2() |> str_replace("%", "") |> as.numeric() |> (\(.) ./100)(),
        error = function(e){NA} 
      )
      
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
             tags = list(tags),
             nota = status.rating,
             avaliacoes = status.avaliacoes,
             percent_homem = percent_homem,
             leitores_leram = status.leram,
             leitores_queremler = status.queremler,
             leitores_lendo = status.lendo,
             leitores_abandonaram = status.abandonaram,
             leitores_lerendo = status.relendo
      )
    },
  error = function(e){tibble()})
}

raspar_autor <- function(url){
  tryCatch(
    {
      html <- read_html(paste0("https://www.skoob.com.br", url))
      
      nome <- tryCatch(
        html |> html_elements("#box-nome") |>  html_elements("h1") |> html_text2(),
        error = function(e){NA} 
      )
        
      local <- html_elements(html, ".adr") |> html_text2()
      local.pais <- tryCatch(
        local |> str_split_i(" - ", 1),
        error = function(e){NA} 
      )
      local.cidade <- tryCatch(
        local |> str_split_i(" - ", 2),
        error = function(e){""} 
      )
      
      nascimento <- tryCatch(
        html_elements(html, "#box-generos") |> html_text2() |> str_split_i("Nascimento: ", 2) |> str_extract("^\\d+/\\d+/\\d+"),
        error = function(e){NA} 
      )
      
      tibble(
        autor_link = url,
        autor_nome = nome,
        autor_pais = local.pais,
        autor_cidade = local.cidade,
        autor_nascimento = nascimento
      )
      
    },
    error = function(e){tibble()})
}

# country_name(autores$autor_pais, to = "ISO3", verbose =  T)
# 
# local.geo <- geo("Grã-Bretanha")
# local.geo$lat
# 

# 
# c("b", NA) |> str_replace_na("")

localizar_autor <- function(locais){
  
  locais <- locais |> 
    mutate(across(everything(), ~ replace_na(.x, "")),
           local = str_c(autor_pais, autor_cidade, sep = ", ") |>
             str_remove(", $"))
  
  locais.geo <- locais |> 
    mutate(geometria = tidygeocoder::geo(local) |>  
             st_as_sf(coords  = c("long", "lat"), crs = "epsg:4326", na.fail = FALSE) |> 
             pull(geometry)) |> 
    filter(!st_is_empty(geometria))
  
  locais.geo.pais <- locais |>
    anti_join(locais.geo) |> 
    mutate(geometria = tidygeocoder::geo(autor_pais) |>  
             st_as_sf(coords  = c("long", "lat"), crs = "epsg:4326", na.fail = FALSE) |> 
             pull(geometry))

  bind_rows(locais.geo, locais.geo.pais) 
  
}
# 
# 
# locais <- autor_complemento |> 
#   distinct(autor_pais,  autor_cidade)
# 
# locais <- locais |> 
#   mutate(across(everything(), ~ replace_na(.x, "")),
#          local = str_c(autor_pais, autor_cidade, sep = ", ") |>
#            # str_trim() |> 
#            str_remove(", $"))
# 
# 
# 
# locais |> 
#   mutate(geometria = tidygeocoder::geo(local) |>  
#            st_as_sf(coords  = c("long", "lat"), crs = "epsg:4326", na.fail = FALSE) |> 
#            pull(geometry))
