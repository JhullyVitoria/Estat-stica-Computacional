library(rvest)
library(dplyr)
library(ggplot2)

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetiza%C3%A7%C3%A3o"

html <- read_html(url)
html


tabelas <- html |>
            html_elements("table") |>
            html_table()
tabelas

alfabetizacao <- tabelas[[3]]
alfabetizacao

alfabetizacao <- alfabetizacao[c(2,3)]
names(alfabetizacao) <- c("estado", "taxa")
names(alfabetizacao)
alfabetizacao

library(stringr)

str_replace_all(string = "pedro145", pattern = "\\d", replacement = "")

part1 <- str_replace_all(string = alfabetizacao$taxa, pattern = ",", replacement = ".")
part1
part2 <- str_replace_all(string = part1, pattern = "%", replacement = "")
part2

parte_final <- as.numeric(part2)
parte_final
parte_final <- parte_final/100
parte_final

alfabetizacao$taxa <- parte_final
alfabetizacao

library(geobr)
minas <- read_state(code_state = "MG")
minas

library(ggplot2)

ggplot(data = minas) + geom_sf(fill = "orange") + theme_void()

municipio_mg <- read_municipality(code_muni = "MG")
ggplot(data = municipio_mg) + geom_sf() + theme_void()

estados <- read_state()
estados

estados$name_state
order(estados$name_state)

estados[2, ]

estados <- estados[order(estados$name_state), ]
estados

alfabetizacao <- alfabetizacao[order(alfabetizacao$estado), ]
alfabetizacao

estados$taxa <- alfabetizacao$taxa
estados

ggplot(data = estados, aes(fill = taxa)) + geom_sf() + scale_fill_gradient(high = "#132B43", low = "#56B1F7")

?scale_fill_continuous
