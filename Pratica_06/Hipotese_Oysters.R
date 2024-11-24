?prop.test

populacao <- sample(0:1, size = 100000, replace = TRUE, prob = c(0.9, 0.1))
amostra <- sample(populacao, size = 200, replace = TRUE)
mean(amostra)

prop.test(x = sum(amostra == 1), n = 200, p = 0.1, alternative = "two.sided")

# Carregamento e vizualização dos dados
library(Stat2Data)
data(Oysters)
View(Oysters)

library(ggplot2)

# Gráfico de Dispersão
ggplot(data = Oysters, aes(x = TwoD, y = Volume)) + geom_point() + geom_smooth(method = "lm")

cor(Oysters$TwoD, Oysters$Volume)
# O cor.test() realiza um teste de hipótese para verificar se 
# a correlação entre as variáveis é estatisticamente significativa
cor.test(Oysters$TwoD, Oysters$Volume)

modelo_linear <- lm(formula = Volume~TwoD, data = Oysters)
modelo_linear

summary(modelo_linear)
modelo_linear$residuals
hist(modelo_linear$residuals)
shapiro.test(modelo_linear$residuals)
#residuals é erro


# Anotações sobre o resultado do código:

# cor.test(Oysters$TwoD, Oysters$Volume):
# O teste de correlação confirma que a correlação é altamente significativa, com um p-valor de 6.77e-13, ou seja, extremamente pequeno (menor que 0.001). Isso indica que a correlação é estatisticamente significativa.
# O intervalo de confiança de 95% para a correlação está entre 0.8369 e 0.9615, o que confirma a forte relação linear entre as variáveis.

# modelo_linear <- lm(formula = Volume~TwoD, data = Oysters):
# Este comando ajusta um modelo de regressão linear, onde "Volume" é a variável dependente e "TwoD" é a variável independente.
# Os coeficientes do modelo são: 
# Intercepto: 0.3366994 (representa o valor predito de "Volume" quando "TwoD" é 0).
# Coeficiente de "TwoD": 0.0002649 (indica que, para cada aumento unitário em "TwoD", o "Volume" aumenta, em média, 0.0002649).

# shapiro.test(modelo_linear$residuals):
# O teste de Shapiro-Wilk foi realizado para verificar se os resíduos seguem uma distribuição normal.
# O p-valor = 0.9208 é maior que 0.05, indicando que não há evidências para rejeitar a hipótese de normalidade. Isso significa que os resíduos podem ser considerados normalmente distribuídos.

# A regressão linear é uma técnica estatística usada para modelar a relação entre uma ou mais variáveis preditoras (independentes) e uma variável resposta (dependente), assumindo que essa relação pode ser representada como uma linha reta (linear). O objetivo da regressão linear é encontrar a melhor linha reta que descreva essa relação e possa ser usada para prever os valores da variável dependente com base nos valores das variáveis preditoras.

# se p valor é muito pequeno, ou seja menor que 0.05 eu rejeito H0. A correlação não é zero.
# se p-valor é alto não rejeito H0, ou seja, maior que 0.05


# hipotese nula coeficiente é igual a zero
#hipotese alternativa o coeficiente é diferente de zero