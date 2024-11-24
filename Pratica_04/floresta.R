library(randomForest)
library(rpart)

dados <- read.csv(file = "cancer.csv", header = TRUE)
str(dados)

dados$diagnosis <- as.factor(dados$diagnosis)

n <- round(0.8*nrow(dados))
n

set.seed(1732)
indices_treino <- sample(1:nrow(dados), size = n, replace = FALSE) #amostra aleatória com os elementos que farão parte do treino

treino <- dados[indices_treino,] #dados de treino
teste <- dados[-indices_treino,] #dados de teste

modelo.arvore <- rpart(formula = diagnosis~ ., data = treino, method = "class") 

#rpart.plot(modelo.arvore, extra = 101) 

previsao <- predict(modelo.arvore, newdata = teste, type = "class") 

mean(previsao == teste$diagnosis) 

tabela <- table(previsao, teste$diagnosis)

floresta <- randomForest(formula = diagnosis~., data = treino, ntree = 200)
floresta

previsao.floresta <- predict(floresta, newdata = teste, type = "class") 

previsao
mean(previsao.floresta == teste$diagnosis)
