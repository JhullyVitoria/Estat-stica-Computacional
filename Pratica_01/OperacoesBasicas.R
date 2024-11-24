

#operações básicas
2+5 
2 - 3 
2 * 3 
40/5 
3 ** 2 
10 %% 3 

#atribuindo valores
a <- 3*2
4*3 -> b

class(a) #a classe de um objeto
class(1)

d <- TRUE #booleano, lógico;
class(d)

d + a 
FALSE + a
TRUE + TRUE
TRUE + FALSE/FALSE
TRUE/FALSE

v1 <- 4+3
V1 <- 3+90 #case sensitive, diferencia maiúsculas de minúsculas.

x <- "teste" #string, chamadas no r de character
class(x)

#criando vetores
x1 <- c(3,10,78)
length(x1) 
sum(x1) 
mean(x1)

c(1,6, "teste") #um vetor admite apenas um tipo de dado, então o vetor será de character, neste caso.

x1[2] #acessando a segunda posição do vetor
x1[c(2,3,2,1)] #acessando as posições 2, 3, 2 e 1 do vetor

x2 <- c(x1, 67,90) 
x2 
x2 < 70 
sum(x2<70)
sum(x2[x2<70]) 

x2 > 54 
x2 == 10 
x2 != 10 
!TRUE 
TRUE | FALSE 
TRUE & FALSE

?sample 

dado <- sample(x = 1:6, size = 1000000, replace = TRUE) 
mean(dado == 3)
table(dado) 
barplot(table(dado)) 
