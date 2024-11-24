library(ggplot2) 

#passo 1: embaralhar as observações do cojunto e, em seguida, dividir este conjunto embaralhado em treino e teste.

iris <- iris[sample(nrow(iris)),] 
n <- round(0.8*nrow(iris)) 

treino <- iris[1:n,] #treino contém as primeiras n observações
teste <- iris[-(1:n),] #teste contém as observações restantes

ggplot(data = treino, mapping = aes(x = Species)) +
  geom_bar() 

ggplot(data = treino, mapping = aes(x = Petal.Length))+
  geom_histogram(bins = 20, fill = "darkgray")+
  theme_minimal() 

ggplot(data = treino, mapping = aes(y = Petal.Length))+
  geom_boxplot()+
  facet_wrap(~Species) 

ggplot(data = treino, mapping = aes(y = Sepal.Length))+
  geom_boxplot()+
  facet_wrap(~Species) 

ggplot(data = treino, aes(x = Petal.Length, y = Petal.Width, color = Species))+
  geom_point() 


#passo 2: construir o modelo de classificação de flores iris com base na árvore de decisão.

resultados <- c() #vetor para armazenar as previsões do modelo
for(j in 1:nrow(teste)){
  if(teste$Petal.Length[j] < 2.5){
    resultados[j] <- "setosa"
  }else{
    if(teste$Petal.Width[j] < 1.75){
      resultados[j] <- "versicolor"
    }else{
      resultados[j] <- "virginica"
    }
  }
}
mean(teste$Species == resultados) #acurácia do modelo