########################################################################
## Introducao a Modelagem Estatistica no R                            ##
## Codigo Aula 9 - Pratica - Mixed Model                              ##
## PPGECO - UFSC - 20222                                              ##
## Preparado por: Fabio G. Daura-Jorge                                ##
########################################################################

### Codigo do material exposto na aula 9 (ppt) ###
### Exercicios ###
### Adaptado de Zuur et al. 2009 ###

## Apresentando Modelos Mistos


#~~~~~~~~~~~~~~~~~~~~#
# 1. Preparacao ----
#~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de riqueza da fauna bentonica e relacao com variaveis abioticas (ja conhecemos esses dados)
RIKZRichness<-read.table(file.choose(), header = TRUE)
head(RIKZRichness)
qqnorm(RIKZRichness$Richness)
qqline(RIKZRichness$Richness)
# Variavel resposta: Richness
# Variaveis explicativas (trabalhar apenas com NAP, Exposure e Beach) 
# NAP: altura de coleta da amostra em relacao ao nivel da mar
# Exposure: indice de exposicao a acao de ondas
# Beach: Ok
RIKZRichness$fBeach<-factor(RIKZRichness$Beach) # definir fatores
RIKZRichness$fexposure<-factor(RIKZRichness$exposure) # definir fatores
str(RIKZRichness)
library(nlme) # para aplicar a funcao lme


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. O modelo de intercepto aleatorio ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# O modelo
Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach, data=RIKZRichness, method = "REML")
# Interprete: o que estamos fazendo? O que define um intercepto aleatorio?
summary(Mlme1)
# Qual o intercepto e inclinacao da parte fixa do modelo? 
### intercept value = 6.58 and slope = -2.56
# Qual o desvio no intercepto provocado pelo efeito aleatorio?
### 2.94
# Monte e interprete o grafico:
F0<-fitted(Mlme1,level=0)
F1<-fitted(Mlme1,level=1)
I<-order(RIKZRichness$NAP)
NAPs<-sort(RIKZRichness$NAP)
plot(NAPs,F0[I],lwd=4,type="l",ylim=c(0,22),
     ylab="Riqueza",xlab="NAP")
for (i in 1:9){
  x1<-RIKZRichness$NAP[RIKZRichness$Beach==i]
  y1<-F1[RIKZRichness$Beach==i]
  K<-order(x1)
  lines(sort(x1),y1[K])
}
text(RIKZRichness$NAP,RIKZRichness$Richness,RIKZRichness$Beach,cex=0.9)
# O que ele esta demonstrando?
# Rode uma regress?o linear que seria equivalente a este modelo misto, porem com apenas componentes fixos
Lm1 <- lm(Richness ~ NAP + factor(Beach), data=RIKZRichness)
F2 <- fitted(Lm1, level=0)
plot(NAPs,F2[I],lwd=4,type="l",ylim=c(0,22),
     ylab="Riqueza",xlab="NAP")
summary(Lm1)

# Qual a grande desvantagem aqui?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. O modelo de intercepto e inclinacao aleatorio ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Mlme2 <- lme(Richness ~ NAP,
             random = ~1 + NAP | fBeach, data = RIKZRichness)
# Interprete: o que estamos fazendo? O que define um intercepto e inclinacao aleatorio?
summary(Mlme2)
# Qual o intercepto e inclinacao da parte fixa do modelo?
# Qual o desvio no intercepto e na inclinacao provocado pelo efeito aleatorio? (StdDev intercept and NAP)
# Qual a relacao entre a variacao do intercepto e a variacao da inclinacao (Corr (intr))

# Monte e interprete o grafico:
F0<-fitted(Mlme2,level=0)
F1<-fitted(Mlme2,level=1)
I<-order(RIKZRichness$NAP)
NAPs<-sort(RIKZRichness$NAP)
plot(NAPs,F0[I],lwd=4,type="l",ylim=c(0,22),
     ylab="Riqueza",xlab="NAP")
for (i in 1:9){
  x1<-RIKZRichness$NAP[RIKZRichness$Beach==i]
  y1<-F1[RIKZRichness$Beach==i]
  K<-order(x1)
  lines(sort(x1),y1[K])
}
text(RIKZRichness$NAP,RIKZRichness$Richness,RIKZRichness$Beach,cex=0.9)
# O que ele esta demonstrando?
# Rode a regressao liner apenas com componentes fixos equivalentes e mostra qual a grande desvantagem

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. O modelo de efeitos aleatorios ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Mlme3 <- lme(Richness ~ 1, random = ~1 | fBeach,
             data = RIKZRichness)
summary(Mlme3)
# Interprete: o que estamos fazendo? O que representa esse modelo?

# Ah, um modelo so com efeito fixo e comparavel por AIC seria:

Mlme0 <- gls(Richness ~ NAP, # generalized least square, com REML
             data = RIKZRichness)
summary(Mlme0)

# Como comparar esses modelos?
# A unica diferenca entre eles eh o fator aleatorio
# Poderiamos utilizar um teste de hipotese:
anova(Mlme0, Mlme1, Mlme2)
# AIC seleciona um e BIC outro, alem do p ser significativo
# Nao podemos usar o teste de hipotese para comparar esses modelos
# Pois a diferenca entre eles nao esta em um B1, mas sim na variancia do B1 (0 ou > 0)
# Enfim, isso tem implicacoes. Se o p valor for muito alto ou muito baixo, sem problemas
# Se estiver na borda, cuidado...
# O p certo seria:

0.5*(1-pchisq(12.7207,1)) # para comparar Mlme0 e Mlme1
0.5*((1-pchisq(7.09,1))+(1-pchisq(7.09,2))) # para comparar Mlme1 e Mlme2

# Agora inclua a variavel exposicao
# Crie um modelo com NAP e exposicao fixos interagindo
# Mais um com NAP e exposicao fixos interagindo e praia com intercepto aleatorio
# Mais um com NAP e exposicao fixos interagindo e praia com intercepto e inclinacao aleatorio
# Mais um com NAP e exposicao fixos sem interacao e praia com intercepto e inclinacao aleatorio

## Modelos Mistos tem varias nuances e devemos tomar alguns cuidados adicionais
## Por ora cumprimos nosso objetivo, mas para usa-lo adequadamente mergulhe fundo em suas peculiaridades
## Principalmente em relacao ao processo de selecao de modelos...
## Lembre que tem um protocolo:

#Passo 1: Iniciar com um modelo proximo ao otimo em termos de componentes fixos (o maior numero de variaveis possivel);
#Passo 2: Procurar a estrutura otima aleatoria;
#Passo 3: Ent?o, com a estrutura aleatoria otima, reduzir componentes fixos com o motodo ML;
#Passo 4: Rodar o modelo final do passo e com REML


## Quem sabe vc nao faz o modulo dois dessa disciplina no futuro... :)
## OBRIGADO PELA PARTICIPACAO





## OPAOPAOPA... nao vao embora ainda... vamos jogar a isca para esses proximos modulos... Uma pitada de GLMM..

# Abaixo dados de presen?a de um nematoda (E.cervi) em Veado Vermelho; 
# A pergunta era avaliar se tamanho, sexo e fazenda influenciariam na presenca do parasita.
# Certamente, exploramos via uma modelo binomial
# Veja os dados
DeerEcervi<-read.table(file.choose(), header = TRUE)
head(DeerEcervi)

# Preparacoes...
DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
DeerEcervi$Ecervi.01[DeerEcervi$Ecervi>0]<-1
DeerEcervi$fSex <- factor(DeerEcervi$Sex)
DeerEcervi$CLength <- DeerEcervi$Length - mean(DeerEcervi$Length)
DeerEcervi$fFarm <- factor(DeerEcervi$Farm)
head(DeerEcervi)

# Como seria o modelo GLM (relembrando)
DE.glm<-glm(Ecervi.01 ~ CLength*fSex+fFarm, data = DeerEcervi,
            family = binomial)
drop1(DE.glm, test = "Chi")
summary(DE.glm)

# Ser? que estamos interessados no efeito da Fazenda (Farm) na variavel resposta?

# Como fazer um GLMM para esse caso, colocando fazenda como aleatorio? 

# Algumas opcoes...

library(MASS) # MASS traz a funcao glmmPQL

DE.PQL<-glmmPQL(Ecervi.01 ~ CLength * fSex,
                random = ~ 1 | fFarm, family = binomial, data = DeerEcervi)
summary(DE.PQL)

library(lme4) # lme4 traz a funcao lmer

DE.lme4<-glmer(Ecervi.01 ~ CLength * fSex +(1|fFarm),
               family = binomial, data = DeerEcervi)
summary(DE.lme4)

install.packages("glmmTMB")
library(glmmTMB) # glmmTMB traz a funcao glmmTMB

DE.glmmTMB4<-glmmTMB(Ecervi.01 ~ CLength * fSex +(1|fFarm),
                     family = binomial, data = DeerEcervi)
summary(DE.glmmTMB4)


# Explore o pacote DHARMA para validacao: 
if(!require(DHARMa)){install.packages("DHARMa");library(DHARMa)}
simRes <- DHARMa::simulateResiduals(fittedModel = DE.glmmTMB4, 
                                    n = 1000)
plot(simRes) # good


# UFA, ta bom... valeu d+
# Esperamos que tenha ajudado, e nao assustado...
# Fomos...