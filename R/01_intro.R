##################################################################
## Introducao a Modelagem Estatistica no R                      ##
## Codigo Aula 1 - Pratica - Introducao a Modelagem Estatistica ##
## PPGECO - UFSC - 20212                                        ##
## Preparado por: Fabio G. Daura-Jorge		             ##
##################################################################

### Codigo do material exposto na aula 1 (ppt) ###
### Adendos de multiplas fontes ###

## Apresentando o Modelo Linear no R

# Se você está utilizando RStudio, na parte inferior direita da janela do script, há uma opção para interpretar este .txt como um script do R (.R).

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. Forma basica: relacao linear entre duas variaveis continuas ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Antes de comecar, vale revisar alguns pontos quentes...
# Em uma regressao, 'assumimos' sempre uma relacao causa-efeito
# O valor da variavel X 'causa' o valor da variavel Y 
# Ou seja, pode-se dizer que y = f(x), sendo y = B0 + B1x,
# Essa equacao descreve o grafico de uma linha reta...
# O modelo tem dois parametros mais o erro aleatorio:
# B0 = intercepto = eh o valor da funcao quando x = 0
# B1 = inclinacao = eh uma taxa medida por variacao de x pela variacao de y

# Veja abaixo:
plot(x = c(2,5), y = c(16,10), # valores de x e y
     type='n', # plot sem os valores
     ylab='y', xlab='x', # atirbui nome aos eixos y e x
     ylim=c(0,20), xlim=c(0,6)) # define os limites dos eixos y e x

points(c(2,5), c(16,10), pch=16) # adiciona pontos (pch = 16) nas coordenadas x e y
lines(c(2,5),c(16,10))
lines(c(2,2),c(16,10))
lines(c(2,5),c(10,10))
abline(20,-2)


#~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. Ajustando um modelo ----
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Como ajustar um modelo aos dados..?
# Um modelo linear classico eh y = B0 + B1x + e, sendo 'e' o erro aleatorio que segue uma distribuicao normal
# Veja a figura desenhanda abaixo:
plot(c(2,5), c(16,10), type='n', ylab='y', xlab='x', ylim=c(0,40), xlim=c(0,6))
points(x = c(0,1,2,3,4,5,6), y = c(40,23,35,19,5,18,2), pch=16)
abline(20,-2, col = "red") # first number is the intercept B0, second is the B1
abline(30,-4, col = "blue")
abline(40,-6, col = "black")

# Qual reta representa o modelo melhor ajustado aos dados?

# Para ajustar uma reta aos dados, sao tres passos:
# 1.Fazer a reta passar pelo ponto central composto pela media de X e Y.
# 2.Calcular cada residuo (distancia do dado a reta ao quadrado)
# 3.Somar todos os residuos ao quadrado (soma dos residuos ao quadrado...
# A reta de melhor ajuste eh aquela com a menor soma dos quadrados. 
# Tarefa ardua para calcular na mao, facil no R (ver abaixo).


#~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Testando hipoteses ----
#~~~~~~~~~~~~~~~~~~~~~~~~~#

# y = B0 + e -> H0 - Inclinacao eh nula
# y = B0 + B1x + e -> H1 - Inclinacao eh marcante (forte influencia de x e y)
# Amanha a tarde veremos como descobrir se a inclinacao eh ou nao diferente de 0
# Por ora, vamos focar nos parametros

# Os parametros B0 e B1 podem ser estimados na mao
# Mas vamos deixar o R fazer pelo metodo dos minimos quadrados
# O R tambem calcula o coeficiente de determinacao, vulgo r^2
# Esse r^2 mede o quanto da variacao em y eh promovida por x e quanto eh por acaso.
# Basicamente faz uma correla??o entre os observados e preditos
# Quanto maior o r^2, mais x influenciou na variacao de y

# Que tal avaliar abundancia de fitoplancton (em uma unidade qualquer) em relacao a um gradiente de profundidade?
Fito <- c(12,10,8,11,6,7,2,3,3)
Prof <- c(0:8)
plot(Fito ~ Prof, pch=16)
abline(lm(Fito ~ Prof)) # Abline vai plotar a reta de regressao com os valores preditos pelo modelo (lm)
fitted <- predict(lm(Fito ~ Prof)) # A funcao predict vai me gerar os valores preditos pelo modelo
fitted 

# Veja os residuos?
lines(x = c(0,0), y = c(12,11.7555))

for (i in 1:length(fitted)){
  lines(x = c(Prof[i], Prof[i]), y = c(Fito[i],fitted[i]))
} 

# Agora veja a regressao classica no R e seus outputs
M1 <- lm(Fito ~ Prof) # destaque para a funcao lm (que estima os parametros pelo metodo dos minimos quadrados)

# Interpretar... veja os parametros, como foram estimados?
summary(M1)


#~~~~~~~~~~~~~#
# 4. Predizer ----
#~~~~~~~~~~~~~#

# A ideia eh muitas vezes nao so testar uma relacao...
# Mas usar o modelo como um instrumento preditivo...

## Imagine uma profundidade de 5.5 metros, qual deveria ser a abundancia de fito?
predict(M1, list(Prof=5.5))

# Se quiser saber em varias profundidades?
predict(M1, list(Prof=c(3.3,4.4,5.5,6.6)))

# Para isso, eh preciso estimar os parametros


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 5. Uma canja: estimar parametros por Maxima Verossimilhanca ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# A Verossimilhanca estimando parametros...
# Ver dados de razao sexual de uma especie qualquer:
N <- 10 # 10 individuos
n <- 4 # 4 machos

# Qual a probabilidade de ter um macho? Teoricamente 0.4, 4 machos em 10.

# A funcao de maxima verossimilhanca chega em mesmo valor
maxVerossim <- function(N,n,p){
  log(dbinom(n, N, p))
} 

# Essa eh a funcao considerando uma distribuicao binomial
# A probabilidade de sucesso (macho) pode variar de 0 a 1.
ps <- seq(from = 0, to = 1, by = 0.01)
ps

# Resultado grafico da minha funcao para diferentes valores de p 
plot(x = ps, y = maxVerossim(N,n,ps), 
     type = "l", 
     ylab = "log-likelihood", 
     xlab = "p") 

# Esse eh o p teorico 4/10 
abline(v = 0.4, lty = "dashed", col = 'red')

# A funcao optimize vai identificar o valor maximo da funcao de maxima verossimilhanca e o valor de p para tal
g <- optimize(maxVerossim, lower = 0, upper = 1, N = 10, n = 4, maximum = TRUE) 
g$maximum # Bingo!!!


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 6. Regressao multipla e selecao de modelos (intro) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Aqui praticamos Modelagem Estatistica...

# Inserir dados OZONIO, que avalia a relacao dos indices de Ozonio com outras variaveis:
# Variaveis explanatorias: Temperatura, Radiacao e Vento...
Ozonio <- read.table(file.choose(), header = TRUE)
Ozonio

# verifique a estrutura do data.frame
str(Ozonio)

# Veja: variaveis quantitativas...
# Plotar um grafico que mostra a relacao entre elas...
pairs(Ozonio, panel = panel.smooth)

# Como descobrir qual variavel melhor explica a variacao da variavel resposta..?
# E se tiver mais que uma...?
# O modelo diz o seguinte: y = B0 + B1X1 + B2X2 + erro, sendo X1 e X2 as variaveis explanatorias... 

# Brincar de modelar:
M1 <- lm(ozone ~ temp * wind * rad, data = Ozonio) # O que isso significa?

# Esse modelo considera as tres variaveis e a interacao entre elas...
summary(M1) ### there was no significant p-value

## remember that when you multiply, you get the interaction between variables, when
## you sum them, you only put all of them in the model, but without considering
## possible effects that they could have when combined

# Outro modelo...
M2 <- lm(ozone ~ temp + wind + rad, data = Ozonio) 

# Esse modelo nao considera a interacao entre variaveis...
summary(M2) 
summary.aov(M2)
summary.lm(M2)

# Mais um...
M3 <- lm(ozone ~ temp + wind, data = Ozonio)
summary(M3)

### R squared will always be bigger when you have more variables, which means that
### your adjusted value is higher, but we need to take in account the precision as well

# E agora, qual o melhor modelo..?
# CRITERIO DE INFORMACAO DE AKAIKE. Seleciona o modelo + parcimonioso...
AIC(M1, M2, M3) # Funcao AIC? Aqui modificado para o metodo dos minimos quadrados 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 6. Predizer com o modelo selecionado ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Escolhido o modelo, prever o valor de ozonio para um determinado valor das variaveis explicativas.
oz <- predict(M1, list(temp=62, rad=99, wind=8.6))
oz # :) isso eh modelagem. 
# Usar dados, para ajustar um modelo e estimar parametros, e pelos parametros descrever e prever relacoes

# Mas o modelo selecionado nesse exemplo tem problemas...
par(mfrow=c(2,2)) # separa os gráficos em duas linhas e duas colunas
plot(M1) 
par(mfrow = c(1,1))
# Esse conjunto de graficos mostra que nao estamos obedecendo premissas

# Mas o que devo verificar com esses gráficos?
if(!require(performance)){install.packages('performance')};library(performance)
performance::check_model(M1)

# Ok... eh preciso voltar para mais um pouco de teoria

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# 7. Codigo do material exposto em aula (ppt) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# entrar dados Prod_Yield.txt - produtividade primária e pesca
Prod_Yield <- read.table(file.choose(), header = TRUE) 

par(mfrow=c(1,1)) # retorna para o padrão
plot(Y ~ Clorofila, data = Prod_Yield, pch=16)
abline(lm(Y~Clorofila, data=Prod_Yield))

# O modelo linear
mod1 <- lm( Y~ Clorofila, data = Prod_Yield)
summary(mod1)

# extraindo os valores preditos
fitted<-predict(lm(Y ~ Clorofila, data = Prod_Yield))
fitted

# prever o valor de clorofila para os valores abaixo
predict(mod1, list(Clorofila = c(0.2,0.3,1.6,1.9)))