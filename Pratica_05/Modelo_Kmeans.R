# Modelo K-Means
# K é o número de aglomerado que eu quero aglomerar

library(jpeg)

bob_esponja <- readJPEG("Bob_patrick.jpg")
str(bob_esponja)
#  num [altura: 1:1280, largura: 1:720, R,G,B: 1:3] 0.196 0.196 0.192 0.192 0.192 ...


R <- as.vector(bob_esponja[,,1])
G <- as.vector(bob_esponja[,,2])
B <- as.vector(bob_esponja[,,3])

# x <- rep(1:largura, each = altura)
x <- rep(1:1920, each = 1080)
x

# x <- rep(altura:1, times = largura)
y <- rep(1080:1, times = 1920)

dados <- data.frame(x,y,R,G,B)
head(dados)

?kmeans

#dados[, 3:5] seleciona as colunas de R, G e B para agrupar.
#centers = 3 define o número de clusters (3 cores principais).
#nstart = 20 significa que o algoritmo será executado 20 vezes com diferentes pontos iniciais, escolhendo o melhor resultado.

clusterizacao <- kmeans(x = dados[, 3:5], centers = 3, nstart = 20)

clusterizacao$centers
cores <- rgb(clusterizacao$centers)
cluster <- as.factor(clusterizacao$cluster)
dados$cluster <- cluster
head(dados)

library(ggplot2)
ggplot(data = dados, aes(x = x, y = y, col = cluster)) + geom_point() + scale_color_manual(values = cores) + theme_void()
