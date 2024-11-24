
# questao 01
diabetes <- read.table(file = "diabetes.txt", header = TRUE, sep = ";")

str(diabetes)
diabetes <- diabetes[, -1]

diabetes$Diabetic <- as.factor(diabetes$Diabetic)

set.seed(002034)

#Passo 01: Dividir em treino e teste

n <- round(0.80*nrow(diabetes)) #80% dos dados para treino e 20% para teste
indices_treino <- sample(1:nrow(diabetes), size = n, replace = FALSE) 

treino <- diabetes[indices_treino,] #dados de treino
teste <- diabetes[-indices_treino,] #dados de teste

# a) 
library(ggplot2)

ggplot(data = treino, mapping = aes(x = Diabetic)) + geom_bar()
# De acordo com o gráfico gerado, percebemos que o número de pessoas que possuem diabetes desse conjunto de dados é menor do que o número de pessoas que não possuem. Há cerca de 8.000 pessoas que não possuem diabetes e cerca de 4.000 que possuem

ggplot(data = treino,aes(x=Age,fill=Diabetic))+ geom_histogram()+ theme_minimal()
# Analisando o gráfico gerado, notamos que as idades que são mais afetadas pela diabete são principalmente entre 20 e 25 anos, com outras concentrações de casos entre 35 e 45 anos e entre 52 e 60 anos aproximadamente.

ggplot(data = treino, aes(x = Pregnancies, fill = Diabetic)) + geom_histogram() + theme_minimal()
# Entre as pessoas com um número maior de gestações, há uma proporção mais alta de casos de diabetes, o que pode indicar uma relação entre o número de gestações e a prevalência de diabetes.

# b)
library(rpart)
library(rpart.plot)
# Criando o modelo de árvore de decisão
modelo <- rpart(Diabetic ~ ., data = treino, method = "class")

# Plotando a árvore de decisão
rpart.plot(modelo, extra = 101)

previsao.modelo <- predict(modelo, newdata = teste, type = "class") 

# calculando acuracia sem função
acuracia <- mean(previsao.modelo == teste$Diabetic)
acuracia

diagnostico_paciente <- function(Pregnancies, BMI, Age, SerumInsulin) {
  if (Pregnancies < 2) {
    return(0)
  } else {
    if (BMI < 22) {
      return(0)
    } 
    else if (BMI >= 22) {  
      if (SerumInsulin < 52) {
        return(0)
      } else {
        if (Age < 36) {
          return(0)
        } else {
          return(1)
        }
      }
    }
  }
}

respostas <- c()

# Aplicando a função a cada linha do conjunto de teste
for (i in 1:nrow(teste)) {
  # 4 argumentos da funcao
  respostas[i] <- diagnostico_paciente(teste[i, 1], teste[i, 6], teste[i, 8], teste[i, 5])
}

# Calculando a acurácia
acuracia <- mean(respostas == teste$Diabetic)
acuracia
# 0.7933333
# A previsão deste modelo é de 79%

# c) modelo de floresta aleatória
library(randomForest)
floresta <- randomForest(formula = Diabetic~., data = treino, ntree = 200)
floresta

previsao.floresta <- predict(floresta, newdata = teste, type = "class") 

# acurácia do modelo
mean(previsao.floresta == teste$Diabetic)
# 0.9463333
# a acurácia do modelo é 94%

# d) a probabilidade do modelo de árvore de decisão acertar é de 79% e a probabilidade do modelo de floresta aleatória acertar é de 94%.

# e) há uma maior chance de classificarmos corretamente os pacientes se utilizarmos o modelo de floresta aletória.Pois, ao comparar os dois modelos vemos que a acurácia do modelo da floresta foi maior que a da árvore.

# questao 2
cerebelo <- read.csv(file = "cerebelo.csv", header = TRUE, sep = ",")

str(cerebelo)
summary(cerebelo)

cerebelo$Species <- as.factor(cerebelo$Species)

ggplot(data = cerebelo, aes(x = Body_g, y = Cerebellum_g)) + 
  geom_point() + geom_smooth(method = "lm")


ggplot(data = cerebelo, aes(x = log(Body_g), y = log(Cerebellum_g))) + 
  geom_point() + geom_smooth(method = "lm")

# No primeiro gráfico, que mostra o peso do corpo em relação ao peso do cerebelo, a relação entre as variáveis parece um pouco dispersa. Isso sugere uma correlação entre o peso do corpo e o peso do cerebelo, mas a variação nos dados implica que essa relação direta pode não ser tão forte.
# No segundo gráfico, onde os valores de peso do corpo e do cerebelo foram transformados para a escala logarítmica, a relação entre as variáveis se torna muito mais linear e forte.
# Esses gráficos sugerem que, embora exista uma correlação entre o peso do corpo e o peso do cerebelo, ela é mais bem modelada em uma escala logarítmica. Isso implica que à medida que o peso do corpo aumenta, o peso do cerebelo aumenta a uma taxa proporcional em uma escala logarítmica.

# b)
 # calcula o coeficiente de correlação:
cor(cerebelo$Cerebellum_g, cerebelo$Body_g)
# 0.348658

# c)
cor(log(cerebelo$Cerebellum_g), log(cerebelo$Body_g))
# 0.9495087

# d)
# resultado da correlação sem o logaritmo
# 0.348658
# A correlação de 0.348658 sugere uma relação positiva fraca entre o peso do cerebelo e o peso do corpo. Isso significa que, antes da transformação logarítmica, os dados são bastante dispersos e a relação entre essas variáveis é pouco consistente.

# resultado da correlação com o logaritmo
# 0.9495087
# Após a transformação logarítmica, a correlação sobe para 0.9495087, indicando uma relação positiva muito forte entre o logaritmo do peso do cerebelo e o logaritmo do peso do corpo. Isso sugere que, na escala logarítmica, os dados estão muito mais alinhados, indicando uma relação linear clara e forte.

# e)
# vamos determinar a reta de regressão e verificar se é um bom modelo 

modelo <- lm (data = cerebelo, formula = log(Cerebellum_g) ~ log(Body_g))
modelo
# equação da reta:
# log(Cerebellumg) = Intercepto + Inclinação x log(Bodyg)
# Coefficients:
#(Intercept)  log(cerebelo$Body_g)  
#-4.9717                0.7839
# equação da reta: log(Cerebellumg) = -49717 + 0.7839 x log(Bodyg)

summary(modelo)
#o p valor é muito pequeno, ou seja, menor que 0.05 eu rejeito H0. A correlação não é zero.

# Gráfico de Dispersão e a reta da equação fornecida
ggplot(data = cerebelo, aes(x = Body_g, y = Cerebellum_g)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "blue") + 
  geom_abline(slope = 0.7839, intercept = -49717, color = "red", linetype = "dashed")

# f)
modelo$residuals
hist(modelo$residuals)
shapiro.test(modelo$residuals)
# O p-valor = 0.9999 é maior que 0.05, indicando que não há evidências para rejeitar a hipótese de normalidade. Isso significa que os resíduos podem ser considerados normalmente distribuídos.

# g)
log_previsao <- predict(modelo, newdata = data.frame(Body_g = 100000))
log_previsao

# Transformando de volta para a escala original (gramas)
predito_em_gr <- exp(log_previsao)

# Exibindo o valor predito do peso do cerebelo em gramas
predito_em_gr

# questão 03)
olive <- read.table(file = "olive.txt", header = TRUE, sep = ",")

str(olive)
summary(olive)

dados_padronizados <- scale(olive[,-1])

# b)
# modelo kmeans com k = 3
modelo_kmeans <- kmeans(dados_padronizados, centers = 3, nstart = 10)

#extração do vetor de aglomerados
aglomerados <- modelo_kmeans$cluster
# atribui a variavel cluster_k3 ao conj. de dados(olive)
olive$cluster_k3 <- aglomerados
olive$cluster_k3 <- as.factor(olive$cluster_k3)

ggplot(data = olive, aes(x = cluster_k3, fill = region)) + geom_bar() + theme_minimal()

# cluster 1 = a grande maioria dos azeites deste cluster é proveniente da reegião Southern Italy
# cluster 2 = este cluster é dominado por azeites da região Northern Italy, com uma pequena quantidade de azeites de Southern Italy
# cluster 3= os azeites neste cluster são principalmente de Sardinia e Southern Italy, com quase nenhuma presença de Northern Italy.
#

# c)
# modelo kmeans com k = 4
modelo_kmeans <- kmeans(dados_padronizados, centers = 4, nstart = 10)

#extração do vetor de aglomerados
aglomerados <- modelo_kmeans$cluster
# atribui a variavel cluster_k4 ao conj. de dados(olive)
olive$cluster_k4 <- aglomerados
olive$cluster_k4 <- as.factor(olive$cluster_k4)

ggplot(data = olive, aes(x = cluster_k4, fill = region)) + geom_bar() + theme_minimal()

# Cluster 1 = contém uma mistura de azeites, predominantemente da Nothern Italy, com uma pequena quantidade de azeites da Southern Italy.
# Cluster 2 = é composto majoritariamente por azeites da Southern Italy, com uma pequena quantidade da Northern Italy.
# Cluster 3 = é dominado quase exclusivamente por azeites da Southern Italy.
# Cluster 4 = contém azeites principalmente da Sardenha, com uma pequena contribuição da Northern Italy.
# O gráfico indica que os azeites do Southern Italy têm uma forte presença nos clusters 2 e 3, enquanto os azeites da Sardinia estão bem representados no cluster 4.


# modelo kmeans com k = 5
modelo_kmeans <- kmeans(dados_padronizados, centers = 5, nstart = 10)

#extração do vetor de aglomerados
aglomerados <- modelo_kmeans$cluster
# atribui a variavel cluster_k5 ao conj. de dados(olive)
olive$cluster_k5 <- aglomerados
olive$cluster_k5 <- as.factor(olive$cluster_k5)

ggplot(data = olive, aes(x = cluster_k5, fill = region)) + geom_bar() + theme_minimal()
# Southern Italy: os azeites dessa região mostram uma composição mais homogênea e distintiva, evidenciada pelo grande número de amostras no cluster 3.
# Northern Italy e Sardinia: Essas regiões compartilham semelhanças na composição lipídica dos azeites, resultando em um agrupamento conjunto no cluster 5.
# Diferenças Regionais: A distribuição dos azeites entre os clusters reforça que existem variações químicas regionais nos azeites italianos, possivelmente relacionadas a condições de cultivo, tipos de azeitonas e processos de produção.
