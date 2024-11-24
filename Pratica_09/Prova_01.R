
# questao 01)

#a)
cores <- c("azuis", "vermelhas", "brancas", "marrons")
numeros <- c(1:13)
baralho <- expand.grid(numero = numeros, cor = cores)

#a)
# Inicializar contador para a quantidade de vezes que exatamente duas cartas azuis são sorteadas
experimento1 <- function(n){
  duas_azuis <- 0
  # Realizar n simulações usando a função replicate
  resultados <- replicate(n, {
    # Sortear 3 cartas sem reposição
    sorteio <- sample(baralho$cor,size = 3, replace = FALSE)
    # Contar quantas cartas azuis foram sorteadas
    sum(sorteio == "azuis")
  })
  # Calcular a proporção de vezes em que exatamente duas cartas azuis foram sorteadas
  proporcao_duas_azuis <- mean(resultados == 2)
  return(proporcao_duas_azuis)
}

# Definir o número de simulações
n <- 100000

# Executar a função com o parâmetro n
resultado <- experimento1(n)

# Mostrar o resultado
print(round(resultado, 5))

#b)
experimento <- function() {
  n <- 0
  n2 <- 0
  while (n < 4) {
    Carta <- sample(baralho$numero, size = 1, replace = TRUE)
    n2 <- n2 + 1
    if (Carta == 7) {
      n <- n + 1
    }
  }
  return(n2)
}

quantidades <- c()

for (i in 1:100000) {
  quantidades[i] <- experimento()
}

mean(quantidades)
#Este resultado nos diz quantas cartas em media devemos retirar do baralho até que consigamos obter o numero 7 quatro vezes


# questão 02)

# a)
dados <- read.csv(file = "churn.txt", header = TRUE, sep = ";")
str(dados)
dados <- dados[, -c(1, 2, 3)]

str(dados)

summary(dados)

#dados$Surname <- as.factor(dados$Surname)
dados$Geography <- as.factor(dados$Geography)
dados$Gender <- as.factor(dados$Gender)

# b)
library(rpart)
library(rpart.plot)

n <- round(0.75*nrow(dados)) #75% dos dados para treino e 25% para teste
indices_treino <- sample(1:nrow(dados), size = n, replace = FALSE) 

treino <- dados[indices_treino,] #dados de treino
teste <- dados[-indices_treino,] #dados de teste

modelo.arvore <- rpart(formula = Exited ~ ., data = treino, method = "class")

rpart.plot(modelo.arvore, extra = 101)

previsao <- predict(modelo.arvore, newdata = teste, type = "class")

mean(previsao == teste$Exited) 
# Resultado: 0.8604
# Esse resultado da acurácia do modelo nos diz que o modelo de classificação acertou 86% das previsões. Ou seja, para 86% das observações do conjunto de teste, o modelo previu corretamente se um cliente sairia ou não.

#matriz de confusão
tabela <- table(previsao, teste$Exited) 
tabela

# Resultado: 
#     previsao    0    1
#             0 1941  302
#             1   47  210

# 1941: Verdadeiros Negativos (TN) – O modelo previu corretamente que 1941 clientes não saíram (classe 0).
#302: Falsos Negativos (FN) – O modelo previu que 302 clientes não sairiam (classe 0), mas eles realmente saíram (classe 1).
#47: Falsos Positivos (FP) – O modelo previu que 47 clientes sairiam (classe 1), mas eles realmente não saíram (classe 0).
#210: Verdadeiros Positivos (TP) – O modelo previu corretamente que 210 clientes saíram (classe 1).

# c)

clientes_country <- split(dados, dados$Geography)

library(rpart)
library(rpart.plot)

# Inicializando uma lista para armazenar os resultados e outra para armazenar os modelos
resultados <- list()
modelos_arvore <- list()

for(pais in names(clientes_country)) {
  
  # Dados do país atual
  dados_pais <- clientes_country[[pais]]
  
  # Dividindo os dados em treino (75%) e teste (25%)
  set.seed(252)
  n <- round(0.75 * nrow(dados_pais))
  indices_treino <- sample(1:nrow(dados_pais), size = n, replace = FALSE)
  
  treino <- dados_pais[indices_treino, ]
  teste <- dados_pais[-indices_treino, ]
  
  # Criando o modelo de árvore de decisão
  modelo <- rpart(Exited ~ ., data = treino, method = "class")
  
  # Salvando o modelo e os conjuntos de dados
  modelos_arvore[[pais]] <- list(modelo = modelo, treino = treino, teste = teste)
  
  # Plotando a árvore de decisão
  rpart.plot(modelo, extra = 101, main = paste("Árvore de Decisão -", pais))
  
  # Fazendo previsões no conjunto de teste
  previsao <- predict(modelo, newdata = teste, type = "class")
  
  # Calculando a acurácia
  acuracia <- mean(previsao == teste$Exited)
  
  # Criando a matriz de confusão
  matriz_confusao <- table(previsao, teste$Exited)
  
  # Armazenando os resultados
  resultados[[pais]] <- list(
    modelo = modelo,
    acuracia = acuracia,
    matriz_confusao = matriz_confusao
  )
  
  # Exibindo os resultados
  print(paste("País:", pais))
  print(paste("Acurácia:", round(acuracia, 4)))
  print(matriz_confusao)
}


# Resultados:

# [1] "País: France"
#[1] "Acurácia: 0.8868"

#previsao    0    1
        #0 1031  118
        #1   24   81
#[1] "País: Germany"
#[1] "Acurácia: 0.7911"

#previsao   0   1
        #0 397 109
        #1  22  99

#[1] "País: Spain"
#[1] "Acurácia: 0.8675"

#previsao   0   1
        #0 501  67
        #1  15  36
#Os modelos de árvore de decisão têm desempenho variado entre os países, com a França e a Espanha apresentando melhores acurácias que a Alemanha. As matrizes de confusão mostram que os erros ocorrem principalmente ao prever clientes que não sairiam como se sairiam, e vice-versa.
