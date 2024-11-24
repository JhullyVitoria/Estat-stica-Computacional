library(dplyr)
library(ggplot2)

femur <- read.csv("femur.csv")
str(femur)

homens <- femur|>
  filter(genero == "Male")
str(homens)

cor(homens$altura, homens$femur)

ggplot(data = homens, aes(x = femur, y = altura))+
  geom_point()+
  geom_smooth(method = "lm")

#cor.test: teste de hipotese para verificar se o coef. de correlação é zero 
#H0: coefiente de correlação e zero 
#ha: coeficiente de correlação não é zero 

#caso seja menor de 5% (0.05) rejeita Ho 
cor.test(homens$altura, homens$femur)

#PASSO 2: vamos determinar a reta de regressão e verificar se é um bom modelo 

modelo <- lm (data = homens, formula = altura ~ femur)
modelo

summary(modelo)

#PASSO 3: Analisar os residuos (isto é, analisar os erros) 

hist(modelo$residuals)

#Vamos realizar um teste de hipotese para verificar se os residuos seguem uma distribuiçõ normal 
#H0: os residios seguem uma distribuição normal 
#Ha: ps residuos não seguem uma distribuição normal 

shapiro.test(modelo$residuals)

