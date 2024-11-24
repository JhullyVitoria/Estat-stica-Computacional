
# questao 01)

compra_album <- function(N) {
  album <- rep(0, N)  
  pacotes_comprados <- 0
  
  while (any(album == 0)) {
    pacote <- sample(1:N, 4, replace = TRUE)  
    album[pacote] <- 1  
    pacotes_comprados <- pacotes_comprados + 1
  }
  
  return(pacotes_comprados) 
}

# a) N = 20 
num_simulacoes <- 100000
resultados <- replicate(num_simulacoes, compra_album(20))

#média dos resultados
media_total <- mean(resultados)
cat("media total de pacotes comprados p/ N = 20:", media_total, "\n")

# media total de pacotes comprados p/ N = 20: 18.37408 

# média dos resultados > 5
media_maior_que_5 <- mean(resultados[resultados > 5])
cat("media dos pacotes comprados > 5 p/ N = 20:", media_maior_que_5, "\n")

#media dos pacotes comprados > 5 p/ N = 20: 18.37408 

# A média dos resultados da simulação media_total representa o número médio de pacotes que uma pessoa precisaria comprar para completar um álbum com n = 20 figurinhas, ou seja, em torno de 18 aproximadamente.

# A média dos resultados que são maiores do que 5 , representa o número médio de pacotes necessários para completar o álbum em simulações onde foram precisos mais de 5 pacotes.

# o resultado das duas médias serem iguais é esperado, pois os valores menores ou iguais a 5 representam uma pequena fração das simulações. Mesmo que sejam removidos, seu impacto na média é insignificante, pois representam apenas uma pequena parte do total.

# b)

media_valores <- function(N, num_simulacoes = 100000) {
  resultados <- replicate(num_simulacoes, compra_album(N))
  return(mean(resultados))
}

# Testar a função para N = 5, 10, 15, 20
media5 <- media_valores(5)
cat("Média para N = 5:", media5, "\n")

media10 <- media_valores(10)
cat("Média para N = 10:", media10, "\n")

media15 <- media_valores(15)
cat("Média para N = 15:", media15, "\n")

media20 <- media_valores(20)
cat("Média para N = 20:", media20, "\n")

# Resultados das medias acima:
# Média para N = 5: 3.23079 
# Média para N = 10: 7.69591 
# Média para N = 15: 12.80367 
# Média para N = 20: 18.3542 

# Os resultados das médias para os diferentes valores de n mostram a relação entre o número de figurinhas únicas necessárias para completar o álbum e o número médio de pacotes comprados para poder completar ele.
# n = 5 é uma média de 3 pacotes aproximadamente, n = 10 média de 7 pacotes aproximadamente, n = 15 média de 12 pacotes aproximadamente e, n = 20 média de 18 pacotes aproximadamente.

# questao 02)
papagaio <- read.table(file = "papagaio.txt", header = TRUE, sep = ",")
str(papagaio)

papagaio$especie <- as.factor(papagaio$especie)

# a)
library(ggplot2)

ggplot(data = papagaio, aes(x = peso, y = envergadura, color = especie)) + geom_point()
# esse gráfico de dispersão nos mostra que a espécie arctica parece ser a menor em relação ao peso e envergadura. Já a espécie corniculata ela está em um meio termo. E a espécie cirrhata é a que possui maior peso e envergadura, ou seja, a maior dentre elas.

ggplot(data = papagaio, aes(x = peso, y = tamanho, color = especie)) + geom_point()
# o gráfico gerado nos mostra que a especie arctica continua sendo menor em peso e agora em tamanho, com peso variando entre 200 a 400 gramas e tamanho de 20 a 40 centímetros aproximadamente. o cirrhata é a maior dentre elas, variando de 550 a 1200 gramas em peso e de 25 a 55 centímetros em tamanho aproximadamente.Além disso, esse gráfico mostra uma correlação positiva entre peso e tamanho, nas três espécies, pois quanto maior o peso, maior é o tamanho.

ggplot(data = papagaio, aes(x = envergadura, y = tamanho, color = especie)) + geom_point()
# a especie arctica possui envergadura em torno de 40 a 60 centímetros e tamanho entre 20 a 40 centimetros aproximadamente, a cirrhata tem envergaduras maiores, em torno de 48 a 82 centímetos aproximadamente e tamanho entre 30 a 52 aproximadamente, já a corniculata continua em um meio termo, variando entre 42 a 73 de envergadura e 23 a 53 de tamanho.

# b)
corniculata <- papagaio[papagaio$especie == "corniculata",]

corniculata_data <- na.omit(corniculata)

modelo <- lm(peso ~ tamanho + envergadura, data = corniculata_data)

novo_valor <- predict(modelo, newdata = corniculata[is.na(corniculata$peso), ])

# c)
library(class)

set.seed(123)
indices_treino <- sample(1:nrow(papagaio), 0.70*nrow(papagaio))
treino <- papagaio[indices_treino,]
teste <- papagaio[-indices_treino,]

#criando um modelo KNN
treino_padronizado <- scale(treino[,-4])
teste_padronizado <- scale(teste[,-4])

#criando um modelo KNN com k = 1
modelo_knn1 <- knn(train = treino_padronizado, test = teste_padronizado, cl = treino$especie, k = 1)
table(modelo_knn1, teste$especie)
mean(modelo_knn1 == teste$especie) #acuracia

# Acurácia do modelo KNN com K = 1 é 0.6533333

#criando um modelo KNN com k = 3
modelo_knn3 <- knn(train = treino_padronizado, test = teste_padronizado, cl = treino$especie, k = 3)
table(modelo_knn3, teste$especie)
mean(modelo_knn3 == teste$especie) #acuracia
# Acurácia do modelo KNN com k = 3 é 0.6733333

#criando um modelo KNN com k = 4
modelo_knn4 <- knn(train = treino_padronizado, test = teste_padronizado, cl = treino$especie, k = 4)
table(modelo_knn4, teste$especie)
mean(modelo_knn4 == teste$especie) #acuracia
# Acurácia do modelo KNN com k = 4 é 0.6933333

# O modelo com KNN com k = 4 se saiu melhor em relação a acurácia e teve uma melhor distribuição de classificações, reduzindo a confusão entre as espécies, especialmente entre corniculata e cirrhata. O aumento de k nesse caso, proporcionou uma maior robustez ao modelo, diminuindo o impacto de outliers ou classificações errôneas, sendo assim, melhorando a performance geral.
