##################################################################
## Introducao a Modelagem Estatistica no R                      ##
## Codigo Aula 2 - Pratica - Analise Exploratoria de Dados      ##
## PPGECO - UFSC - 20222                                        ##
## Preparado por: Fabio G. Daura-Jorge                          ##
##################################################################

### Codigo do material exposto na aula 2 (ppt) ###
### Adendos de multiplas fontes


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. Conferindo a presenca de dados faltantes ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# No R, dados faltantes sao representados por NA

# Criamos um vetor de dados contendo NAs
dados <- c(1, 5, 3, 15, 4, NA, 0, 12, 3, NA)
dados

# Algumas funcoes nao aceitam NAs
mean(dados)
median(dados)

# Mas podem ignorar NA usando o argumento 'na.rm = TRUE'
mean(dados, na.rm=TRUE) 
median(dados, na.rm=TRUE)

# Funcao 'summary' identifica presenca de NA
summary(dados)

# Para perguntar ao R se tem NA, use a funcao 'is.na'
is.na(dados)  # Retorna um vetor de verdadeiros e falsos

# Podemos filtrar os dados do vetor eliminando NAs
dados[!is.na(dados)]

# Podemos substituir NA por zero (se fosse o caso...) usando atribuicao '<-' ou "="
dados
dados[is.na(dados)] <- 0
dados

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. Identificando excesso de zeros ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Criamos um vetor de dados contendo muitos zeros
dados <- c(0, 0, 1, 0, 2, 0, 5, 9, 0, 10, 4, 0, 5, 4, 2, 0, 30, 0, 0, 3, 3, 0)
dados

# Formas diferentes de conferir se existe excesso de zeros
table(dados)
plot(table(dados))
barplot(table(dados))
hist(dados)
sum(dados==0)/length(dados)  # Proporcao dos dados que sao iguais a zero


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Identificando outliers ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Visualizando os dados brutos
plot(dados)

# Podemos identificar observacoes de forma interativas com a funcao "identify"
# Entra no modo interativo e permite identificar o outlier
# clicando no ponto do grafico
# Pressione 'esc' e a funcao retornara a posicao da observacao
identify(dados) 
dados[17]

# Outras formas de identificar outliers
dotchart(dados) # Mostra os dados sequencialmente (ordem) de baixo pra cima no eixo y
stripchart(dados, method="stack") 

# usando a funcao 'beeswarm'
if(!require(beeswarm)){install.packages("beeswarm");library(beeswarm)} # Instalando pacotes requeridos

beeswarm::beeswarm(dados)
beeswarm::beeswarm(dados, col="red", pch=16, method="swarm")

# boxplot
boxplot(dados)

# Podemos extrair os valores do boxplot com:
boxplot(dados)$stat


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 4. Explorando a distribuicao de uma variavel ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Vamos usar dados do peso de galinhas e tipo de alimentacao
data(chickwts)

# Histogramas
hist(chickwts$weight)

# aumentamos o numero de classes com o argumento 'nclass'
hist(chickwts$weight, nclass=15) 
hist(chickwts$weight, nclass=30)

# Também podemos plotar o histograma classico com area = 1  (densidade de probabilidades)
hist(chickwts$weight, prob = T) 

# Podemos adicionar os dados brutos no eixo x com a funcao 'rug'
hist(chickwts$weight, main="", col="gray", xlab="Peso das galinhas")
rug(chickwts$weight)

# Cleveland dotplot
dotchart(chickwts$weight) # weight no eixo x
plot(chickwts$weight)  # Compare com a funcao generica 'plot'

# Beeswarm
beeswarm::beeswarm(chickwts$weight)

# Densidade kernel
plot(density(chickwts$weight)) # Area abaixo da curva tambem soma 1

# Densidade kernel sobreposta com histograma
hist(chickwts$weight, prob = TRUE) 
lines(density(chickwts$weight))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 5. Dados seguem distribuicao normal? ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Podemos verificar a distribuicao da variavel peso em um histograma
hist(chickwts$weight, prob = TRUE) 

# Podemos gerar uma distribuicao normal teorica a partir da media e desvio padrao do peso observado 
# Vamos sobrepor nossa distribuicao teorica com o histograma dos pesos observados 
curve(dnorm(x, mean = mean(chickwts$weight), sd = sd(chickwts$weight)),
      add=T, col="red")

# Grafico quantil-quantil - QQ-Plot
qqnorm(chickwts$weight)
qqline(chickwts$weight, col="blue", lwd=2)

# Grafico quantil-quantil na unha
quantile(chickwts$weight, probs=seq(0,1,0.01)) -> quant_sample
rnorm(1000, mean=mean(chickwts$weight), sd=sd(chickwts$weight)) -> random
quantile(random, probs=seq(0,1,0.01)) -> quant_theoretical
plot(quant_theoretical, quant_sample)
abline(0,1, col="red", lwd=2)

# QQ-plots com outras distribuicoes
if(!require(car)){install.packages("car");library(car)}

qqPlot(chickwts$weight)
qqPlot(chickwts$weight, distribution="pois", lambda=mean(chickwts$weight))
qqPlot(chickwts$weight, distribution="unif")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 6. Relacao entre duas variaveis ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Vamos usar dados sobre velocidade e distancia de parada de automoveis
data(cars)

# Plotando um diagrama de dispersao com a funcao generica 'plot'
plot(cars$speed, cars$dist)
plot(cars$dist ~ cars$speed) # Ou no formato de formula

# Diagrama de dispersao com linha de suavizacao
scatter.smooth(cars$dist ~ cars$speed, 
               ylab="Distancia de parada",
               xlab="Velocidade")

# Ajustando o grau de suavizacao
scatter.smooth(cars$dist ~ cars$speed, span=0.1) # pouco suavizado
scatter.smooth(cars$dist ~ cars$speed, span=0.5)
scatter.smooth(cars$dist ~ cars$speed, span=5) # muito suavizado


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 7. Graficos condicionados ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Vamos usar os dados da qualidade do ar de Nova Iorque
data(airquality)

# Boxplots condicionados
boxplot(airquality$Temp ~ airquality$Month)
boxplot(airquality$Temp ~ airquality$Month, varwidth = TRUE) # Veja no help o que o argumento 'varwidth' faz

# Boxplot com notch
boxplot(airquality$Temp ~ airquality$Month, notch = TRUE)

# Diagramas de dispersao condicionados
coplot(Ozone ~ Temp | Month, data=airquality)

# Ou com a funcao do pacote 'lattice'
require(lattice)
xyplot(Ozone ~ Temp | Month, data=airquality)
xyplot(Ozone ~ Temp | Month, 
       data=airquality, type = c("p", "r")) # com linha de regressao

xyplot(Ozone ~ Temp | Month, 
       data=airquality, type = c("p", "smooth")) # com linha de suavizacao


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 8. Paineis de diagramas de dispersao ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

data(Mandel)
?Mandel

# Pairplot
pairs(Mandel)

# Funcao encontrada no help da funcao 'pairs' para plotar histogramas
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# Agora plotamos com histograma nos paineis diagonais
pairs(Mandel, diag.panel=panel.hist)

# Outra funcao para fornecer os coeficientes de correlacao
# com a fonte proporcional ao indice
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Agora plotamos com indices de correlacao e histogramas
pairs(Mandel, diag.panel=panel.hist, lower.panel=panel.cor)

# Incluimos tambem a linha de suavizacao
pairs(Mandel, panel=panel.smooth, diag.panel=panel.hist, lower.panel=panel.cor)

### explanatory variables are strongly correlated

#~~~~~~~~~~~~~~~~~#
# 9. EXERCICIOS ---- 
#~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 9.1. Quarteto de Anscombe ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Utilize o banco de dados 'anscombe'
data(anscombe)
help(anscombe)
anscombe

# Ajuste uma regressao linear a cada par de colunas y1~x1, y2~x2...
lm1 <- with(anscombe, lm(y1 ~ x1))
lm2 <- with(anscombe, lm(y2 ~ x2))
lm3 <- with(anscombe, lm(y3 ~ x3))
lm4 <- with(anscombe, lm(y4 ~ x4))
lms <- list(lm1,lm2,lm3,lm4)

# Compare os valores estimados de interceptos e inclinacao da reta
coef(lm1)
coef(lm2)
coef(lm3)
coef(lm4)

# for (i in lms) {
#   coef_val <- as.list(coef(i))
# }
# coef_val

# Compare as medias e desvios padrao de cada variavel
apply(anscombe,2,mean)
apply(anscombe,2,sd)

# Agora plote cada par de variaveis
par(mfrow = c(2,2))
plot(lm1)
plot(lm2)
plot(lm3)
plot(lm4)
# O que voce pode concluir?


#~~~~~~~~~~~~#
# 9.2. Iris ----
#~~~~~~~~~~~~#

# Vamos usar um banco de dados usado por Fisher (1936)
# Sao medidas florais de tres especies
data(iris)
help(iris)

# Usando as tecnicas mostradas em aula de Analise Exploratoria de Dados, 
# Responda as seguintes perguntas:

# Como eh a distribuicao das medidas de cada variavel?
head(iris)
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
qqnorm(iris$Sepal.Length, main= "Sepal Length")
qqline(iris$Sepal.Length, col="blue", lwd=2)

qqnorm(iris$Sepal.Width, main = "Sepal Width")
qqline(iris$Sepal.Width, col="blue", lwd=2)

qqnorm(iris$Petal.Length, main = "Petal Length")
qqline(iris$Petal.Length, col="blue", lwd=2)

qqnorm(iris$Petal.Width, main = "Petal Width")
qqline(iris$Petal.Width, col="blue", lwd=2)


par(mfrow = c(1,1))
### what can be seen? not normally distributed
### try to do a for loop for this

# Tem outliers?
# Tem distribuicao normal?
# As variaveis estao correlacionadas?
# Existe diferenca das medidas entre as tres especies? Compare as medias,
# plote graficos
