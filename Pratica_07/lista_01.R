# Exercicio 01

# a)
v1 <- c(10:30)
v1

# b)
v2 <- c(30:10)
v2

# c)
v3 <- c(10:30, 30:10)
v3

# Exercicio 02

# a)
vetor <- seq(2, 8, by = 2)
rep(vetor, 10)

# b)
rep(vetor, length.out = 41)

# Exercicio 03

# a)
n <- c(20:30)
resultado <- sum(n^2 + 4*n)
resultado

# b)
n1 <- c(10:20)
resultado_n1 <- sum((3^n1)/n1 + (2^n1)/(n1^2))
resultado_n1

# Exercicio 04

vet <- c()
urna <- sample(1:100, size = 40, replace = TRUE)
vet <- urna
vet

# a)
qtd_pares <- sum(urna%%2 == 0)
qtd_pares

# b)
maior_setenta <- sum(urna>70)
maior_setenta

# c)
posicao_impar <- which(urna%%2 == 1)
posicao_impar

# Exercicio 05
lancamento_dado <- function(){
  
  quatro <- 0
  qtd_necessaria <- 0
  
  repeat{
   dado <- sample(1:6, size = 1, replace = TRUE)
   
    qtd_necessaria <- qtd_necessaria + 1
    
    if(dado == 4){
      quatro <- quatro + 1    
    }

    if(quatro == 2){
      return (qtd_necessaria)
    }
  }
  
}
lancamento_dado()


# Exercicio 06
quantidades <- numeric(10000)

for(i in 1:10000){
  quantidades[i] <- lancamento_dado()
}

media_quantidades <- mean(quantidades)
media_quantidades

# Resultado da minha média: 12.0651
# De acordo com o resultado, podemos concluir que, a partir das 10.000 simulações é necessário que se realize em média de 12 lançamentos para que o  número 4 saia pela segunda vez.

# Exercicio 07

fibonacci <- function(n){
  
 vetor <- n
 vetor[1] <- 1
 vetor[2] <- 1
 
  if(n >= 3){
    for (j in 3:n) {
      vetor[j] <- vetor[j-1] + vetor[j-2]
    }
    return(vetor)
  }
  else{
    print("Tente um valor maior ou igual a 3!")
  }
}

fibonacci(10)

# Exercicio 08
library(lenght)
sorteio_amigo_oculto <- function(){
  
  participantes = c("Dwight Schrute", "Jim Halpert", "Kevin Malone", "Creed Bratton", "Michael Scott")
  
  sorteio <- sample(participantes, size = lenght(participantes), replace = FALSE)
  
  for(i in 1:lenght(participantes)){
    cat(participantes[i], "tirou", sorteio[i], "\n")
    if(sorteio[i] == participantes[i]){
      return (0);
    }
    else
      return(1)
  }
}
sorteio_amigo_oculto()

proporcao <- numeric(100000)

for(i in 1:100000){
  proporcao[i] <- sorteio_amigo_oculto()
}

media_quant <- mean(proporcao == 0)
media_quant

# Exercicio 09

jogo_craps <- function(){
dado_1 <- sample(1:6, size = 1, replace = TRUE)
dado_2 <- sample(1:6, size = 1, replace = TRUE)

soma_dados = dado_1 + dado_2

if(soma_dados == 7 || soma_dados == 11){
  
  return(1)
  
}else if(soma_dados == 2 || soma_dados == 3 || soma_dados == 12){
  
  return(0)
  
}else{
  
  repeat{
    dado_x <- sample(1:6, size = 1, replace = TRUE)
    dado_y <- sample(1:6, size = 1, replace = TRUE)
    
    if(dado_x + dado_y == 7){
      return(0)
    break
    }else if(dado_x + dado_y == soma_dados){
      return(1)
      break
    }
  }
}
}

experimento <- numeric(100000)

for(i in 1:100000){
  experimento[i] <- jogo_craps()
}

media_quantidade <- mean(experimento == 1)
media_quantidade

# Exercicio 10

# a)
N <- 20

passeio_Luke <- function(L){
  
  caminho <- L
  
  while(TRUE){
    
  moeda <- sample(1:2, size = 1, replace = TRUE)
  
    if (moeda == 1){
      caminho <- caminho - 1
    }else{
      caminho <- caminho + 1
    }
    
    if(caminho == 0){
      #print("Caiu no precipicio :(")
      return(0)
    }
    else if(caminho == N){
      #print("Você chegou em casa :)")
      return(1)
    }
  }
}
passeio_Luke(17)

# b)
replica_passeioLuke <- function(L){
  
  vetor_passeio <- numeric(10000)
  
  for(i in 1:10000){
     vetor_passeio[i] <- passeio_Luke(L)
  }
  proporcao_chegou_casa <- mean(vetor_passeio)
  return(proporcao_chegou_casa)
}
replica_passeioLuke(17)

# c)
library(ggplot2)

# criando um dataframe para o ggplot
L = 1:19
prop_luke <- numeric(length(L)) # vetor com 19 posicoes

#loop p/ calcular a proporcao p/ cada valor de L de 1 a 19
for(i in L){
  prop_luke[i] <- replica_passeioLuke(i)
}
df <- data.frame(L_valor = L, proporcao = prop_luke)
ggplot(data = df, aes(x = L, y = prop_luke))

# print(prop_luke)


# Exercicio 11

dado_link <- c("L", "R", "U", "D")

lancamento <- function(n){
  
  caminho <- c(0, 0)
  
for(i in 1:n){
  lanca_dado <- sample(dado_link, size = 1)

  if(lanca_dado == "L"){
    caminho[1] <- caminho[1]-1;
  }else if(lanca_dado == "R"){
    caminho[1] <- caminho[1]+1;
  }else if(lanca_dado == "U"){
    caminho[2] <- caminho[2]+1;
  }else if(lanca_dado == "D"){
    caminho[2] <- caminho[2]-1;
  }
  
}
  return(caminho)
}
lancamento(8)

# b)
simula_experimento <- function(n_experimento, passos){
  
  contador_origem <- 0
  
  for(i in 1:n_experimento){
    caminho_final <- lancamento(passos)
    
    #verifica se o caminho final é na origem (0,0)
    if(caminho_final[1] == 0 & caminho_final[2] == 0){
      contador_origem <- contador_origem + 1
    }
  }

#Calcula a proporção de vezes que retorna a origem
proporcao <- contador_origem/n_experimento
return (proporcao)

}

#Simular 10.000 vezes com 8 passos
simula_experimento(10000, 8)

# Resultado da Simulação: 0.0737
# Esse resultado sugere que há uma chance de 7,37% aproximanadamente de link retornar ao ponto de origem depois de 8 passos aleatórios em um espaço bidimensional. Ou seja, é relativamente raro que após 8 passos em direções aleátorias, link volte para a mesma posição inicial.

# c)
calc_proporcao <- function(N){
  num_experimento <- 10000
  
  if (N%%2 == 1){
    print("impossível retornar a origem depois de um número ímpar de passos")
  }else if(N%%2 == 0){
    return(cat("Proporção de vezes em que Link retornou depois de", N, "passos:", simula_experimento(num_experimento, N)))
  }
}
calc_proporcao(8)


# Exercicio 12

# Anotações:
# A função tail() em R é usada para extrair os últimos elementos de um vetor.
# A função all() verifica se todas as condições dentro dela são verdadeiras.

Steven <- c(0, 1, 0)
Garnit <- c(0, 0, 1)

jogo_moeda <- function(){
  
  lancamentos <- 0
  
  repeat{
    # cara = 1; coroa = 0
    lanca_moeda <- sample(0:1, size = 1)
    lancamentos <- c(lancamentos, lanca_moeda) # acumula os lancamentos
    
    if(length(lancamentos) >= 3){
     # Pega os ultimos 3 lançamentos
       ult_tres <- tail(lancamentos, 3)
  
      # Verifica se Steven ou Garnit ganhou
      if(all(ult_tres == Steven)){
        return("Steven")
      }else if(all(ult_tres == Garnit))
        return("Garnit")
    }

  }
}

experimento <- function(n){
  
  pontos_Garnit <- 0
  
  for(i in 1:n){
  
    replicacao <- jogo_moeda()
    
    if(replicacao == "Garnit"){
      pontos_Garnit <- pontos_Garnit + 1
    }
  }
  prop <- pontos_Garnit/n
  return(prop)
}
experimento(10000)

# Resultado: 0.6671
# O resultado obtido mostra que Garnit venceu cerca de 66, 71% das 10.000 simulações do jogo. Isso sugere que a sequência sequência escolhida tem uma vantagem probabilística sobre a sequência escolhida por Steven.

# Exercício 13

dados <- read.csv(file = "dados.txt", header = TRUE, sep = ";")
str(dados)
summary(dados)

dados$Genero <- as.factor(dados$Genero)
dados$Genero

# a)
library(ggplot2)
ggplot(data = dados, aes(x = Genero, fill = Genero)) + geom_bar() + theme_minimal()

# a diferença marcante na quantidade de vítimas por gênero pode ser analisada como parte do comportamento padrão de shipman. Pode ser que ele visse mulheres idosas como alvos mais vulneráveis ou havia algum fator pessoal que explicasse essa escolha.

# b)
ggplot(data = dados, aes(x = Idade, fill = Genero)) + geom_histogram(bins = 8, color = "black") + theme_minimal()

# Ao analisar o gráfico podemos perceber que a maioria das vítmas de Shipmam estavam na faixa etária entre 70 e 90 anos. Ou seja, ele focava principalmente em idosos, já que muitas dessas mortes poderiam ser atribuídas a causas naturais devido a idade avançada.Além disso, o fato de ter mais mulheres do que homens na maioria das faixas etárias indica que shipman tinha uma preferência ppor vítimas femininas.O foco em vítimas idosas também pode estar associado a vulnerabilidade desse grupo.

# c)

ggplot(data = dados, aes(y = Idade)) + geom_boxplot() + theme_minimal()

# mediana: a linha central do bloxplot indica que a mediana das idades das vítimas de Shipman é de aproximadamente 80 anos.Ou seja, metade das vítimas tinham em torno de 80 anos ou mais.
# intervalo interquartil(IQR): a caixa do gráfico mostra o IQR, que abrange cerca de 50% das idades centrais, que estão entre 73 e 84  anos. Isso reforça que shipman tinha como alvo pessoas idosas e na faixa etária de 73 a 84 anos.
# outliers: o bloxplot exibe diversos outliers(pontos isolados do whisker(linhas verticais) inferior), que representam vítimas com idades mais jovens, em torno de 40 e 60 anos. Esses outliers são consideravelmente menores do que a faixa etária principal, sugerindo que, embora shipman tivesse uma preferência por pacientes velhos, algumas das bítimas eram mais novas do que a maioria.

# d)
dados$LocalDaMorte
dados$LocalDaMorte <- as.factor(dados$LocalDaMorte)

ggplot(data = dados, aes(x = LocalDaMorte, fill = LocalDaMorte)) + geom_bar() + theme_minimal()
# analisando o gráfico gerado, podemos dizer que a maioria dos pacientes que foram mortos por Shipman morreram em sua própria casa, cerca de 200 pacientes. Isso pode ter acontecido devido o paciente estar sem supervisão de alguma pessoa além do médico. E apenas um número muito pequeno de pacientes morreram no hospital ou em uma casa de repouso.

# e)
dados$AnoDaMorte
dados$AnoDaMorte <- as.factor(dados$AnoDaMorte)
ggplot(data = dados, aes(x = AnoDaMorte, fill = AnoDaMorte)) + geom_bar() + theme_minimal()

# f)
# O padrão das vítimas de Harold Shipman revela que a maioria delas eram mulheres com idade em torno de 80 anos, sugerindo um foco em pacientes idosos do sexo feminino. Além disso, a maior parte das mortes ocorreu nas próprias casas das vítimas, onde Shipman tinha mais liberdade para agir sem supervisão. O ano de 1997 é significativo, pois marca o auge de suas atividades criminosas. Embora também existam algumas vítimas mais jovens e em outros locais, o perfil predominante é de mulheres idosas que viviam sozinhas ou em ambientes menos supervisionados.

# Exercicio 14

primatas <- read.csv(file = "primatas.txt", header = TRUE, sep = ":")

# a)
str(primatas)
summary(primatas)

# b)
library(ggplot2)

primatas$especie
primatas$especie <- as.factor(primatas$especie)

ggplot(data = primatas, aes(x = especie, fill = especie)) + geom_bar()+theme_minimal()

primatas$genero
primatas$genero <- as.factor(primatas$genero)

ggplot(data = primatas, aes(x = genero, fill = especie)) + geom_bar()+theme_minimal()


# c)
ggplot(data = primatas[primatas$especie == "bonobo", ], aes(x = peso, fill = genero)) + geom_bar()+theme_minimal() + labs(title = "Bonobo")
# fêmea(a maioria) pesa de 30 a 38 e macho(a maioria) pesa de 39 a 55 aproximadamente

ggplot(data = primatas[primatas$especie == "chimpanze", ], aes(x = peso, fill = genero)) + geom_bar()+theme_minimal()+ labs(title = "Chimpanze")
# fêmea(maioria) pesa de 40 a 47 e macho(maioria) pesa de 55 a 62

# d)
#primatas$altura <- as.factor(primatas$altura)
#primatas$altura

ggplot(data = primatas[primatas$genero == "femea", ], aes(x = peso, fill = especie)) + geom_bar()+theme_minimal() + labs(title = "Comparação das Fêmeas do Chimpanze e Bonobo")
# femea do bonobo é mais leve que femea chimpanze, bonobo: 30 a 37kg, chimpanze: 38 a 50

ggplot(data = primatas[primatas$genero == "macho", ], aes(x = peso, fill = especie)) + geom_bar()+theme_minimal() + labs(title = "Comparação dos Machos do Chimpanze e Bonobo")
# macho do bonobo é mais leve que macho chimpanze, bonobo: 38 a 52kg, chimpanze: 55 a 65

# e)
# De acordo com os gráficos gerados anteriormente, podemos perceber que há um mesmo número de machos e fêmeas nas duas espécies. Também foi mostrado que os bonobos são mais leves que os chimpanzes. Além disso, as fêmeas de cada espécie costuma ser mais leve do que os machos.

# f)
ggplot(data = primatas, aes(x = altura, fill = especie)) + geom_bar()+theme_minimal()
ggplot(data = primatas, aes(x = altura, fill = genero)) + geom_bar()+theme_minimal()
ggplot(data = primatas[primatas$genero == "femea", ], aes(x = altura, fill = especie)) + geom_bar()+theme_minimal()

# Aleatorizando o dataframe
primatas<-primatas[sample(nrow(primatas)),]

# Separando em teste e treinamento
n<-round(nrow(primatas)*0.8)
treinamento<-primatas[1:n,]
teste<-primatas[(n+1):nrow(primatas),]

plot(x = treinamento$altura, y = treinamento$peso, type = "n")

points(x=treinamento[treinamento$especie=="bonobo", "altura"], y=treinamento[treinamento$especie=="bonobo", "peso"], pch=16, col="red")
points(x=treinamento[treinamento$especie=="chimpanze", "altura"], y=treinamento[treinamento$especie=="chimpanze", "peso"], pch=16, col="blue")
abline(h=37)
abline(v=127)
abline(h=55)


previsao<-c()
for(i in 1:nrow(teste)){
  if((teste$peso[i]>=37)&&(teste$altura[i]<=127)){
    previsao<-c(previsao, "chimpanze")
  }else if(teste$peso[i]>=55){
    previsao<-c(previsao, "chimpanze")
  }else{
    previsao<-c(previsao, "bonobo")
  }
}

paste("A precisão do modelo de previsão foi de",round(mean(previsao==teste$especie),2), "%.")

