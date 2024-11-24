library(ggplot2)

grilo <- read.csv(file = "grilo.txt", header = TRUE, sep = ",")
summary(grilo)

ggplot(data = grilo, aes(x = frequencia)) + geom_histogram(bins = 10)

cor(grilo)

ggplot(data = grilo, aes(x = temperatura, y = frequencia)) + geom_point() + theme_minimal() + geom_smooth(method = "lm")

modelo_linear <- lm(formula = frequencia~temperatura, data = grilo)
modelo_linear

summary(grilo$temperatura)

w <- data.frame(temperatura = c(21, 23.6, 30.9))
predict(modelo_linear, newdata = w)

library(palmerpenguins)
dados <- penguins

dados <- na.omit(dados)
dados

cor(dados$bill_length_mm, dados$bill_depth_mm)

ggplot(data = dados, aes(x = flipper_length_mm, y = body_mass_g, color = species)) + geom_point() + theme_minimal() + geom_smooth(method = "lm")

library(dplyr)
dados |>
  filter(species == "Gentoo") |>
  select(flipper_length_mm, body_mass_g) |>
  cor()

modelo2 <- lm(formula = flipper_length_mm~body_mass_g + bill_length_mm, data = dados)
modelo2

summary(modelo2)

# Correlação fraca: 0.1 a 0.5
# Correlação moderada: 0.5 a 0.7
# Correlação forte: 0.7 a 1.0
