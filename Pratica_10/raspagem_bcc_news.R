library(rvest)
library(dplyr)
library(stringr)

url <- "https://www.bbc.com/portuguese/articles/cql3lwgl3ldo"

html <- read_html(url)
html

titulo <- html |>
  html_elements("h1") |>
  html_text2()
titulo

# gera os paragrafos do texto da página
noticia <- html |>
  html_elements("p") |>
  html_text2()


#isso aqui me gera as classes
noticia <- html |>
  html_elements("p")
noticia
  
# gera os paragrafos do texto da página
noticia <- html |>
  html_elements("p.bbc-hhl7in") |>
  html_text2()
noticia

noticia <- paste(noticia, collapse = "")
noticia

artigos <- data.frame(titulo, noticia)
artigos

library(tidytext)

?unnest_tokens
# me retorna as palavras que aparecem com mais frequência

#unnest_tokens(output = words, input = noticia): Essa função do pacote tidytext quebra o texto da coluna noticia em palavras individuais (tokens).

#output = words: Define que o resultado (as palavras) será armazenado na nova coluna chamada words.
#input = noticia: Define que o texto a ser tokenizado (quebrado em palavras) vem da coluna noticia.
#Esse comando transforma o texto da coluna noticia em palavras separadas, onde cada linha do novo data.frame terá uma palavra extraída da notícia.

# count(words)
# Após a tokenização, a função count() conta quantas vezes cada palavra aparece na nova coluna words, ou seja, quantas vezes cada palavra se repete no texto.
# arrange(desc(n)) organiza o data.frame resultante em ordem decrescente de frequência. O argumento desc(n) garante que as palavras mais frequentes (ou seja, com maior valor de n) apareçam primeiro.

artigos |>
  unnest_tokens(output = words, input = noticia)|>
  count(words) |>
  arrange(desc(n))

