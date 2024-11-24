library(ggplot2)

titanic <- read.table(file = "titanic.txt", header = TRUE, sep = ",")
head(titanic) 
str(titanic) 

titanic$Survived <- as.factor(titanic$Survived) 
titanic$Pclass <- as.factor(titanic$Pclass) 
titanic$Sex <- as.factor(titanic$Sex) 
titanic <- titanic[,-1] 

str(titanic)

ggplot(data = titanic, mapping = aes(x = Survived)) +
  geom_bar()


ggplot(data = titanic, aes(x = Survived)) +
  geom_bar() +
  labs(title = "Quantidade de pessoas que morreram e sobreviveram", x = "Sobreviveu?", y = "Frequência") +
  theme_minimal()


ggplot(data = titanic, mapping = aes(x = Sex)) +
  geom_bar()+
  theme_minimal()+
  labs(title = "Quantidade de homens e mulheres", x = "Sexo", y = "Frequência")


ggplot(data = titanic, mapping = aes(x = Sex, fill = Survived)) +
  geom_bar()+
  theme_minimal()+
  labs(title = "Quantidade de homens e mulheres que morreram e sobreviveram", x = "Sexo", y = "Frequência")


ggplot(data = titanic, mapping = aes(x = Sex, fill = Survived)) +
  geom_bar()+
  theme_minimal()+
  scale_fill_manual(values = c("#1b9e77"
, "#d95f02"), labels = c("0" = "não",  "1" = "sim"))+
  labs(title = "Quantidade de homens e mulheres que morreram e sobreviveram", x = "Sexo", y = "Frequência", fill = "Sobreviveu?")


ggplot(data = titanic, mapping = aes(x = Pclass, fill = Survived)) +
  geom_bar()+
  theme_minimal()+
  scale_fill_manual(values = c("#1b9e77", "#d95f02"), labels = c("0" = "não",  "1" = "sim"))+
  labs(title = "Quantidade de pessoas que morreram e sobreviveram de acordo com a classe", x = "Classe", y = "Frequência", fill = "Sobreviveu?")


ggplot(data = titanic, mapping = aes(x = Pclass, fill = Survived)) +
  geom_bar()+
  theme_minimal()+
  scale_fill_manual(values = c("#1b9e77", "#d95f02"), labels = c("0" = "não",  "1" = "sim"))+
  labs(title = "Quantidade de pessoas que morreram e sobreviveram de acordo com a classe e o sexo", x = "Classe", y = "Frequência", fill = "Sobreviveu?")+
  facet_wrap(~Sex)


ggplot(data = titanic, mapping = aes(x = Pclass, fill = Survived)) +
  geom_bar()+
  theme_minimal()+
  scale_fill_manual(values = c("#1b9e77", "#d95f02"), labels = c("0" = "não",  "1" = "sim"))+
  labs(title = "Quantidade de pessoas que morreram e sobreviveram de acordo com a classe e o sexo", x = "Classe", y = "Frequência", fill = "Sobreviveu?")+
  facet_wrap(~Sex, labeller = as_labeller(c("female" = "mulheres", "male" = "homens")))
