library(rpart)
library(rpart.plot)

dados <- read.csv(file = "cancer.csv", header = TRUE)

n <- round(0.8*nrow(dados)) 
indices_treino <- sample(1:nrow(dados), size = n, replace = FALSE) 
treino <- dados[indices_treino,] 
teste <- dados[-indices_treino,] 

modelo.arvore <- rpart(formula = diagnosis~ ., data = treino, method = "class")  
rpart.plot(modelo.arvore, extra = 101) 

previsao <- predict(modelo.arvore, newdata = teste, type = "class") 
mean(previsao == teste$diagnosis) 

tabela <- table(previsao, teste$diagnosis) #matriz de confusão