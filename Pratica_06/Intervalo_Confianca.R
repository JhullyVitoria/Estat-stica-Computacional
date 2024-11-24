media <- mean(sample(1:6, size = 500, replace = TRUE))

resultados <- c()
for (j in 1:1000) {
  resultados[j] <- mean(sample(1:6, size = 500, replace = TRUE))
}
hist(resultados)

dados <- data.frame(resultados)
dados

library(ggplot2)

ggplot(dados, aes(x = resultados)) + geom_density(fill = "lightblue4") + theme_minimal()

populacao <- sample(0:1, size = 100000, replace = TRUE, prob = c(0.9, 0.1))

valor_real <- mean(populacao)
valor_real

amostra <- sample(populacao, size = 500, replace = TRUE)

media <- mean(amostra)
media
parte_inferior <- media - 1.96*(sqrt(media * (1 - media))/sqrt(500))
parte_superior <- media + 1.96*(sqrt(media * (1 - media))/sqrt(500))

(parte_inferior + parte_superior)/2 #resultado é exatamente igual a média

inferiores <- c()
superiores <- c()

for (k in 1:100) {
  amostra <- sample(populacao, size = 500, replace = TRUE)
  media <- mean(amostra)
  inferiores[k] <- media - 1.96*(sqrt(media * (1 - media))/sqrt(500))
  superiores[k] <- media + 1.96*(sqrt(media * (1 - media))/sqrt(500))
}

inferiores[23]
superiores[23]

intervalos <- data.frame(inferiores, superiores, contador = 1:100)

#ggplot(data = intervalos) +
#  geom_segment(aes(x = inferiores, xend = superiores, y = contador, yend = contador, col = categoria)) +
 # geom_vline(xintercept = valor_real, col = ("red"))

categoria <- ifelse( intervalos$inferiores > valor_real | intervalos$superiores < valor_real, 0, 1)

intervalos$categoria <- as.factor(categoria)

ggplot(data = intervalos) +
  geom_segment(aes(x = inferiores, xend = superiores, y = contador, yend = contador, col = categoria)) +
  geom_vline(xintercept = valor_real, col = ("red"))

# Resumo: O que o código faz
# Simula a distribuição de médias amostrais para uma amostra de 500 lançamentos de dados.
# Simula uma população de 100.000 elementos (0 e 1) com uma proporção de 10% de "1"s.
# Realiza amostragens da população para calcular a média amostral e intervalos de confiança.
# Repete o processo de amostragem 100 vezes para calcular os intervalos de confiança.
# Visualiza os intervalos de confiança e verifica se eles contêm o valor real da população.
# No final, o gráfico mostra quais intervalos capturam ou não o verdadeiro valor médio da 
# população, permitindo uma análise visual de como os intervalos de confiança variam e quantos incluem o valor real.