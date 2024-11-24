
# questao 01
peixe <- read.table(file = "peixe_boi.txt", header = TRUE, sep = ";")

str(peixe)

# Gráfico de Dispersão
library(ggplot2)
ggplot(data = peixe, aes(x = barcos, y = mortes)) + 
  geom_point() + geom_smooth(method = "lm")

# o gráfico mostra que estas duas variaveis tem uma relação positiva, pois a medida que a quantidade de barcos aumentam o número de mortes também tendem a aumentar.

# b)
# calcula o coeficiente de correlação:

cor(peixe$barcos, peixe$mortes)
# resultado: 0.9525368
# esse resultado indica uma relação positiva muito forte entre o número de mortes de peixes-boi e o número de barcos.Indicando uma relação linear clara e forte.

cor.test(peixe$barcos, peixe$mortes)
# O teste de correlação confirma que a correlação é altamente significativa, com um p-valor extremamente pequeno (menor que 0.001). Isso indica que a correlação é estatisticamente significativa.
# O intervalo de confiança de 95% para a correlação está entre 0.9082223 0.9757264, o que confirma a forte relação linear entre as variáveis.

# c)
# vamos determinar a reta de regressão e verificar se é um bom modelo 
modelo <- lm (data = peixe, formula = mortes ~ barcos)
modelo
# equação da reta:
# log(mortes) = Intercepto + Inclinação x log(barcos)
# Coefficients:
#  (Intercept)       barcos  
#    -44.7218       0.1322 
# equação da reta: log(mortes) =  -44.7218  + 0.1322  x log(barcos)

summary(modelo)
# a partir do resumo do modelo, percebemos que os coeficientes são significativos, ou seja, 
# isto é, os coefientes são diferentes de zero; esta análise é feita a partir daquelas estrelinhas 
# que aparecem ao lado dos coeficientes; 3 estrelinhas significa que o p-valor do teste de hipoteses
# para o coeficiente é menor que 0.001; neste teste, a hipotese nula é que o coeficiente é zero e a 
# hipotese alternativa é que o coeficiente é diferente de zero; se o p-valor for menor que 0.05, 
# rejeitamos a hipotese nula e concluimos que o coeficiente é diferente de zero; neste caso, os dois 
# coeficientes foram testados e são diferentes de zero.

ggplot(data = peixe, aes(x = log(barcos), y = log(mortes))) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x)

# d)
modelo$residuals
hist(modelo$residuals)
shapiro.test(modelo$residuals)
# O p-valor = 0.9458 é maior que 0.05, indicando que não há evidências para rejeitar a hipótese de normalidade. Isso significa que os resíduos podem ser considerados normalmente distribuídos.

# e)
dado <- data.frame(barcos = 800.000)

previsao_mortes <- predict(modelo, newdata = dado)

# Exibir a previsão
previsao_mortes

# Podemos confiar nessa previsão porque utilizamos como base dessa previsão o modelo que fizemos anteriormente e com base nos resultados de shapiro.test, cor(), cor.test(). Eles reforçam que esse o modelo é confiável.

# f) Não devemos utilizar a equação de regressão para prever o número de mortes de peixe-boi, pois o valor de barcos está além do intervalo observado no conjunto de dados. Isso configuraria uma extrapolação do modelo, o que não é recomendado.

# questao 02
musica <- read.table(file = "musicas.txt", header = TRUE, sep = ";")
str(musica)

dados_padronizados <- scale(musica[,-c(7, 8)])

# modelo kmeans com k = 3
modelo_kmeans <- kmeans(dados_padronizados, centers = 3, nstart = 10)

#extração do vetor de aglomerados
aglomerados <- modelo_kmeans$cluster

musica$cluster_k3 <- aglomerados
musica$cluster_k3 <- as.factor(musica$cluster_k3)

ggplot(data = musica, aes(x = cluster_k3, fill = artista)) + geom_bar() + theme_minimal()
# cluster 1 = possui uma distribuição mais diversificada de artistas, sendo a maior parte dominada por Cartola, mas tabém inclui pequenas contribuições de Pato Fu e Racionais MC's.
# cluster 2 e 3 = mostram uma separação clara e exclusiva Pato Fu(cluster 2) e Racionais MC's(cluster 3), sugerindo que o modelo de clustering identificou alguma característica compartilhada nas músicas de cada um desses artistas dentro de seus respectivos clusters.
