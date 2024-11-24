library(ggplot2)
library(palmerpenguins)
library(class)

data(penguins)
pinguins <- penguins
str(pinguins)

pinguins <- pinguins[, -c(2, 7, 8)]
pinguins <- na.omit(pinguins)

# As linhas de código dividem o conjunto de dados pinguins em dois subconjuntos: treino (80%) e teste (20%).

# Em seguida, os dados (exceto a primeira coluna) são padronizados em ambos os subconjuntos, para que cada variável tenha média 0 e desvio padrão 1.

n <- round(0.8*nrow(pinguins))
n
indices_treino <- sample(1:nrow(pinguins), size = n, replace = FALSE)

treino <- pinguins[indices_treino, ]
teste <- pinguins[-indices_treino, ]
treino_padronizado <- scale(treino[, -1])
teste_padronizado <- scale(teste[, -1])

classe_treino <- treino$species
classe_teste <- teste$species

modelo1 <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = 1)
mean(modelo1 == teste$species)
  
cor(treino[, -1]) # correlacao

taxa_acerto <- c()
for (k in 1:10) {
  modelo <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = k)
  
  taxa_acerto[k] <- mean(modelo == teste$species)
  
}
df <- data.frame(k = 1:10, taxa_acerto)
df
ggplot(data = df, aes(x = k, y = taxa_acerto)) + geom_line()

# --- Arquivo Cancer ---

dados <- read.csv(file = "cancer.csv", header = TRUE)
str(dados)

n1 <- round(0.8*nrow(dados))
n1
indice_treino <- sample(1:nrow(dados), size = n1, replace = FALSE)

treino1 <- dados[indice_treino,]
treino1
teste1  <- dados[-indice_treino,]
teste1

treino1_padronizado <- scale(treino1[, -1])
teste1_padronizado <- scale(teste1[, -1])

classe_treino1 <- treino1$diagnosis
classe_teste1  <- teste1$diagnosis

modelo_cancer <- knn(train = treino1_padronizado, test = teste1_padronizado, cl = classe_treino1, k = 2)

mean(modelo_cancer == classe_teste1)
table(modelo_cancer, classe_teste1)
