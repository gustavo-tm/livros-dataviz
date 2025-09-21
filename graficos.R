library(tidyverse)
library(lubridate)
library(magick)

livros |> 
  group_by(edicao_autor) |> 
  summarize(n = n()) |> 
  arrange(-n)





livros |> 
  arrange(-ranking) |> 
  pull(edicao_capa_grande) |> 
  head(1) |> 
  readJPEG(getURLContent(x))


image_read("https://img.skoob.com.br/-1r3uP8OZy0ESqWSpsa2JckkDLU=/600x0/center/top/filters:format(jpeg)/https://skoob.s3.amazonaws.com/livros/11801596/KLARA_E_O_SOL_168220828411801596SK-V11682208285B.jpg")
