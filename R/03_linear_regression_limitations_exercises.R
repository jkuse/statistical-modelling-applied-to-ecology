### linear regression limitations
### exercises  

library(car)
# 3.1 Tendencia da populacao norte americana
data(USPop)
str(USPop)

# Explore os dados, ajuste um modelo linear aos dados e calcule o incremento anual 
# da populacao. O crescimento foi significativo? Plote graficos diagnosticos e 
# discuta possiveis problemas deste modelo.

summary(USPop)
head(uspop)
hist(USPop$population, nclass = 8)

qqnorm(USPop$population)
qqline(USPop$population)
plot(USPop$population) ### seems quadratic
dotchart(USPop$population)
stripchart(USPop$population)

## fitting a linear model
pop_per_year <- lm(population ~ year, data=USPop)
coef(pop_per_year)
confint(pop_per_year)
summary(pop_per_year)
### population in US grows in each year, for each year, population grow 1.288
### since the confidence interval DO NOT present zeros, we can assume that growth was 
### significant
par(mfrow = c(2,2))
plot(pop_per_year)
par(mfrow = c(1,1))

### ploting data per what the lm predicts
plot(population ~ year, data = USPop)
abline(pop_per_year, col = "dark red")

### there are values with high cook's distance, meaning that they a leveraging the model
### looking a QQ-plot, it doesn't follows a normal dist
### there is not starry sky pattern in "residuals vs fitted"
### line in "residuals vs fitted" shows no linearity

# Voce tambem pode calcular a taxa de crescimento instantanea da
# populacao (r) ajustando um modelo linear com a variavel resposta transformada
# no logaritmo natural (funcao 'log'). Neste caso a taxa de crescimento per capita
# sera a inclinacao da reta (beta 1). Ajuste este modelo aos dados.

inst_growth <- lm(log(population) ~ year, data = USPop)
coef(inst_growth)
confint(inst_growth)
summary(inst_growth)

par(mfrow = c(2,2))
plot(inst_growth)
par(mfrow = c(1,1))

plot(population ~ year, data = USPop)
abline(inst_growth, col = "dark red")
# Este modelo corresponde a um modelo de crescimento exponencial.
# Gere uma previsao de quantos habitantes tera os EUA em 2050 e 2100.
# Dica: Lembre-se que para gerar a previsao voce tera que transformar novamente 
# o valor predito da variavel resposta para a escala real 
# (funcao anti-log natural = exponencial - funcao 'exp')

year <- data.frame(year = 2100)
exp(predict(inst_growth, year))

# 3.2 Comparando medidas florais de tres especies
data(iris)
?iris
summary(aov(iris[,1]~iris[,5]))

summary(lm(iris[,1]~iris[,5]))
vars <- iris[, -5]
plot(vars)
cor(vars)


# Queremos saber se as medidas florais variam entre tres especies.
# Explore os dados. Veja se existe correlacao entre as medidas florais.
# Escolha variaveis florais nao correlacionadas para testar a hipotese de que 
# as medidas florais variam entre as especies. Compare os resultados da 
# regressao com uma Analise de Variancia (funcao 'aov'). Qual sua conclusao?

aov(Sepal.Width ~ Species, data = iris)
aov(Petal.Width ~ Species, data = iris) ### didn't get it!! check answers

# 3.3 Causas de acidentes em auto-estradas

# Vamos usar um banco de dados do pacote 'car'
library(car)
data(Highway1)
help(Highway1)
??Highway1
head(Highway1)

# Nossa pergunta: quais os fatores que interferem na taxa de acidentes?
# Explore os dados. Construa diferentes modelos e interprete-os. 
# As variaveis sao significativas? Qual o melhor modelo? Diagnostique se sao
# modelos validos. O que voce pode concluir com os dados?

hist(Highway1$rate, prob = T)
curve(dnorm(x, mean = mean(Highway1$rate), sd = sd(Highway1$rate)),
      add = T,
      col = "red")

qqPlot(Highway1$rate)
qqline(Highway1$rate) ## not a normal

qqPlot(Highway1$rate, distribution = "pois", lambda=mean(Highway1$rate))

plot(Highway1$rate)
stripchart(Highway1$rate)

var_high <- Highway1[,-1]
var_high <- var_high[,-11]
pairs(var_high)
cor(var_high) ### correlation between lane and adt and itg and adt

