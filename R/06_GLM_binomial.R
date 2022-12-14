########################################################################
## Introducao a Modelagem Estatistica no R                            ##
## Codigo Aula 6 - Pratica - GLM                                      ##
## PPGECO - UFSC - 20222                                              ##
## Preparado por: Fabio G. Daura-Jorge                                ##
########################################################################

### Codigo do material exposto na aula 6 (ppt) ###
### Exercicios ###
### Adaptado de Zuur et al. 2009 ###

## Apresentando GLM (Binomial)
?family

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. GLM Binomial - Regressao logistica ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de ocorrencia de lesoes de Tuberculose em Javali selvagem na Espanha
Boar <- read.table(file.choose(), header = TRUE, dec = ".")
head(Boar)
str(Boar)
Boar$SEX <- as.factor(Boar$SEX)
Boar$AgeClass <- as.factor(Boar$AgeClass)
str(Boar)
# Variavel resposta: Tb (ocorrencia de lesoes semelhantes a Tuberculose) 
# Variaveis explanatorias: Sexo; Classe de idade, Tamanho

# Modelar a relacao Tb ~ Tamanho
# Explorando os dados:
par(mfrow=c(1,2))
boxplot(LengthCT~Tb, xlab="Tamanho", ylab="Tb", data = Boar)
plot(Tb~LengthCT, xlab="Tamanho", ylab="Tb",data=Boar)

# Veja a diferenca plotando com a funcao 'jitter'
plot(Tb~LengthCT, xlab="Tamanho", ylab="Tb",data=Boar) 
plot(jitter(Tb)~LengthCT, xlab="Tamanho", ylab="Tb",data=Boar)
par(mfrow=c(1,1))

#~~~~~~~~~~~~~~~~~~~#
# 2. O Modelo ----
#~~~~~~~~~~~~~~~~~~~#

# Funcao glm, familia binomial, link logit 
B1 <- glm(Tb~LengthCT, family = binomial(link=logit), data = Boar)
summary(B1)

B2 <- glm(Tb~LengthCT +SEX, family = binomial(link=logit), data = Boar)
summary(B2)

B3 <- glm(Tb~SEX, family = binomial(link=logit), data = Boar)
summary(B3)

B4 <- glm(Tb~LengthCT + SEX + AgeClass, family = binomial(link=logit), data = Boar)
summary(B4)

B5 <- glm(Tb~LengthCT + AgeClass, family = binomial(link=logit), data = Boar)
summary(B5)

library(MuMIn)
model.sel(B1, B2, B3, B4, B5)
# Observar: Parametros; AIC; Desvios.
### models B2 and B4 can be used
# Pergunta: Quanto a variavel explicativa explica da variacao da variavel resposta?


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Valores preditos ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Plotando o grafico de preditos
summary(Boar$LengthCT)
MyData <- data.frame(LengthCT = seq(from = 46.5, to = 165, by = 1))
Pred <- predict(B1,newdata = MyData, type = "response")
plot(Tb~LengthCT,xlab="Tamanho", ylab="Probabilidade de Tb",data=Boar)
lines(MyData$LengthCT,Pred)

# Ou usando a funcao jitter
plot(jitter(Tb)~LengthCT,xlab="Tamanho", ylab="Probabilidade de Tb",data=Boar)
lines(MyData$LengthCT,Pred)


#~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 5. Validando o modelo ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Testando se o modelo ajustado explica mais que o modelo nulo
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = B2, n = 1000)
residuals(simulationOutput)

# Testando dispersao
testDispersion(simulationOutput, type = "PearsonChisq")

# Plot principal para usar
plot(simulationOutput)

### with plot function
par(mfrow = c(2,2))
plot(B2)
par(mfrow = c(1,1))

# Null Deviance - residual deviance
dev.dif <- 700.76 - 663.56
# Null df - df.model
df.dif <- 507 - 506 ## degrees of freedom
# Calculando o p de uma distribuicao de Qui-quadrado
pchisq(dev.dif, df.dif, lower.tail=F) # Improvavel que vejamos esta reducao
# em deviance ao acaso

# Pseudo R quadrado

# 1 - (Residual Deviance/Null Deviance)
1 - (663.56/700.76)

#ou
#install.packages("pscl")
require(pscl)
?pR2
pR2(B1)
pR2(B2)

#~~~~~~~~~~~~~~~~~~~~~~~~#
# 6. Estudo de caso ----
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados parasitas em bacalhau no Pacifico Norte
ParasiteCod <- read.table(file.choose(), header = TRUE, dec = ".")
head(ParasiteCod)

# Variavel resposta: Prevalencia 
# Variaveis explanatorias: Ano, Profundidade, Peso, Comprimento, Sexo, Estagio, Idade, Area

# Algumas preparacoes para que as variaveis explanatorias sejam fatores
ParasiteCod$fArea <- factor(ParasiteCod$Area)
ParasiteCod$fYear <- factor(ParasiteCod$Year)
ParasiteCod$fSex <- factor(ParasiteCod$Sex)
# Na analise exploratoria: busque colinearidades


# O Modelo (por ora, considerando apenas 3 variaveis)
P1 <- glm(Prevalence ~ fArea * fYear + Length,
          family = binomial, data = ParasiteCod)
summary(P1)
# Observar: Parametros; AIC; Desvios. Lembre que sobredispersao nao ocorre em Bernoulli/Binomial
# Pergunta: Que modelo eh esse? Descreva 

plot(Prevalence ~ Length, data = ParasiteCod)
par(mfrow = c(1,1))
## it is a binomial model, that it is using two explanatory variables in an interactive way

P2 <- glm(Prevalence ~ fArea * fYear + Length + Sex + Stage + Age + fSex + Weight + Depth,
          family = binomial, data = ParasiteCod)
summary(P2)

# Construa novos modelos e busque o mais parcimonioso com a funcao drop1(Modelo, test = "Chi")
drop1(P1, test="Chi")

drop1(P2, test="Chi")

P3 <- glm(Prevalence ~ fArea * fYear + Length + Stage + Age + fSex + Weight + Depth,
          family = binomial, data = ParasiteCod)
summary(P3)

drop1(P3, test="Chi")

P4 <- glm(Prevalence ~ fArea * fYear + Length + Age + fSex + Weight + Depth,
          family = binomial, data = ParasiteCod)
summary(P4)

drop1(P4, test="Chi")

P5 <- glm(Prevalence ~ fArea * fYear + Length + fSex + Weight + Depth,
          family = binomial, data = ParasiteCod)
summary(P5)

drop1(P5, test="Chi")

P6 <- glm(Prevalence ~ fArea * fYear + Length + Weight + Depth,
          family = binomial, data = ParasiteCod)
summary(P6)

drop1(P6, test="Chi")

model.sel(P1, P2, P3, P4, P5, P6)
### in order, all models that could be used would be P5, P6 and P4 (all have delta AIC < 2)

# ADENDO: explore o pacote sjPlot para plotar os preditos do modelo
if(!require(sjPlot)){install.packages("sjPlot");library(sjPlot)} # veja uma forma rapida de instalar e ativar um pacote requerido
if(!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
# Vou ajustar um modelo teste par exemplificat o sjPlot
Pexemplo <- glm(Prevalence ~ Length*fArea,
                family = binomial, data = ParasiteCod)
# Plotando com ggplot2 e sjPlot os preditos do modelo
(Plot_Pexemplo <- plot_model(Pexemplo, type = "int", terms = c("fArea", "Length"), show.legend = T, title = "", 
                             axis.title = c("Length", "Prevalence"), axis.lim = c(0,1)) +
    theme(panel.background=element_rect(fill = "white", colour = "gray10")))
# Interprete esse grafico...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 7. GLM em dados proporcionais ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de ocorrencia de tuberculose em javali, veado e um parasita de veado Nematoide E. cervi.
Tbdeer <- read.table(file.choose(), header = TRUE, dec = ".") # dados Tbdeer.txt
head(Tbdeer)

# Variavel resposta: DeerPosCervi (veados com parasita)/DeerSampleCervi (Amostrados) 
# Variaveis explanatorias: OpenLand (area aberta); Scrubland (area arbustiva); 
# PinePlantation(pinos plantacao); QuercusPlants (numero de Carvalho); 
# QuercusTrees (numero de arvores de Carvalho); 
# WildBoarIndex (Indice de abundancia de javali)
# ReedDeerIndex (indice de abundancia de veados); EstateSize (Tamanho da area); 
# Fenced (area cercada)

# Algumas preparacoes:
# Um novo objeto precisa ser criado para definir a variavel resposta (proporcao de veados positivos para E.cervi)
DeerNegCervi <- Tbdeer$DeerSampledCervi - Tbdeer$DeerPosCervi
# Area cercada binaria (incluir como fator)
Tbdeer$fFenced <- factor(Tbdeer$Fenced)

# O modelo (Incluindo descritores de habitat): 
Deer1=glm(cbind(DeerPosCervi,DeerNegCervi)~OpenLand+ # perceba a construcao da variavel resposta
            ScrubLand+PinePlantation+QuercusPlants+
            QuercusTrees+ReedDeerIndex+EstateSize+fFenced,
          family=binomial, data = Tbdeer)
summary(Deer1)

# Calcular VIF (Variance inflation factor) para avaliar colinearidade entre variaveis
library(car)
vif(Deer1) # PinePlantation talvez pode ser retirada

Deer2=glm(cbind(DeerPosCervi,DeerNegCervi)~OpenLand+
            ScrubLand+QuercusPlants+
            QuercusTrees+ReedDeerIndex+EstateSize+fFenced,
          family=binomial, data = Tbdeer)

summary(Deer2)
### you apply the vif function to the model, and it will gibe you the vif value for the explanatory variables
vif(Deer2)

# Existe sobredispersao? y

# Metodo mais simples
# Divide a Residual deviance pelo Residual degrees of freedom
157.79/15
# Outras alternativas
require(AICcmodavg)
c_hat(Deer2)
c_hat(Deer2, method = "farrington") # Veja no help outras metricas
?c_hat
# Observar: Parametros; AIC; Desvios. Aqui sobredispersao pode ocorrer
# Pergunta: Que modelo eh esse? Descreva!
# Tem sobredispersao dos dados? Caso sim, use quasibinomial. ->  how to know that?
# O que muda depois de ajustar um modelo quasibinomial?
# E construa novos modelos, buscando o mais parcimonioso com a 
# funcao drop1(Modelo, test = "Chi" ou test ="F", dependendo
# se eh da famalia binomial ou quasibinomial, respectivamente) 
# Valide o modelo com a funcao plot(modelo) ou explore DHarma (ser??---????)

### here we can use plot and dharma to validate


### GLM multinomial
install.packages("nnet")
library(nnet)

install.packages("glmmTBM")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 8. Exercicio obrigatorio - GLM Binomial ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Veja a descricao no PDF "Solea Exercicio"

# Dados: Solea solea
# Descricao: Presenca ausencia do linguado Solea solea no 
# estuario Tangus em Portugal;
# Desenho amostral: 65 pontos de coleta com rede de arrasto 
# em 4 pontos do estuario. Os dados foram convertidos para presenca-ausencia
# devido a alta variabilidade na abundancia captura.

# Variavel resposta a considerar: Solea_solea
# Variaveis explicativas a considerar: 
# season, month e station: variaveis nominais
# depth: Ok
# temp: Ok
# sal: salinidade
# transp: Transparencia
# gravel: % cascalho no sedimento
# large sand: % sedimento grosso
# med fine sand: % sedimento medio e fino
# mud: % de lodo

# Objetivo: Quais fatores ambientais influenciam a distribuicao da especie

# Missao: Explorar os dados (avaliar colinearidade); Construir um conjunto 
# de modelos (GLM) para dados de contagem ; Selecionar o modelo; 
# Validar o modelos; Discutir resultados 
# (Qual sua conclusao? Eh poss?vel melhorar a analise? Qual sua sugestao?)

# Atencao: Talvez GLM nao seja a opcao otima para analisar esses dados, 
# mas por hora, vamos ficar com por aqui.