########################################################################
## Introducao a Modelagem Estatistica no R                            ##
## Codigo Aulas 4 e 5 - Pratica - GLM                                 ## 
## PPGECO - UFSC - 20222                                              ##
## Preparado por: Fabio G. Daura-Jorge                                ##
########################################################################

### Codigo do material exposto na aula 4 e 5 (ppt) ###
### Exercicios ###
### Adaptado de Zuur et al. 2009 ###

## Apresentando GLM (Poisson & Binomial Negativa)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. GLM Poisson - Preparacao ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de mortalidade de anfibios em 52 pontos ao longo de uma estrada em Portugal
RoadKills <- read.table(file.choose(), header = TRUE, dec = ",") # Amphibian_road_Kills.txt
head(RoadKills)
# Variavel resposta (TOT.N - carcacas)
# Variaveis explanatorias: OPEN.L (Area aberta - ha); OLIVE (Oliveiras - ha); MONT.S ( Montados c/Arbustos - ha);
# MONT (Montados/sem arbusto - ha); POLIC (Policultura); SHRUBS (Arbustos); URBAN (Area Urbana); WAT.RES (Reservatorio)
# L.WAT.C (Comprimento do curso dagua); L.D.ROAD (Estrada de barro); L.P.ROAD (Estrada pavimentada); 
# D.WAT.COUR (Distancia de reservatorio); D.WAT.COURSES (Distancia do curso dagua); D.PARK (Distancia do Parque);
# N.PATCH (Numero de manchas de habitat); P.EDGE (Perimetro); L.SDI (Indice de diversidade) 
# -> ver tab.16.1 (Zuur et al 2009) -> todos esses dados s?o indices - continuos

# Para economizar espaco... 
RK <- RoadKills
rm(RoadKills)
# Por simplificacao, considerar uma unica variavel explicativa por ora
plot(TOT.N~D.PARK,xlab="Distancia do parque",
     ylab="Mortes na estrada", data=RK) # o que esse plot sugere em termos de distribuicao?

qqPlot(RK$TOT.N, distribution = "pois", lambda = mean(RK$TOT.N))
qqPlot(RK$TOT.N, distribution = "nbinom", lambda = mean(RK$TOT.N))

hist(RK$TOT.N)
par(mfrow = c(1,1))
#~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. O Modelo ----
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Funcao glm, familia poisson;
M1<-glm(TOT.N~D.PARK,family=poisson,data=RK)
summary(M1)
# Observar: Parametros; AIC; Desvios.
# Pergunta: Quanto a variavel explicativa explica da variacao da variavel resposta?

#~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Valores preditos ----
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Veja o resultado e nao se preocupe com o codigo
MyData=data.frame(D.PARK=seq(from=0,to=25000,by=1000))
G<-predict(M1,newdata=MyData,type="link",se=T) # Funcao predict
F<-exp(G$fit)
FSEUP<-exp(G$fit+1.96*G$se.fit)
FSELOW<-exp(G$fit-1.96*G$se.fit)
lines(MyData$D.PARK,F,lty=1)
lines(MyData$D.PARK,FSEUP,lty=2)
lines(MyData$D.PARK,FSELOW,lty=2)


#~~~~~~~~~~~~~~~~~~~~~~~~#
# 4. Selecao de modelos ----
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Considerar 9 variaveis explanatorias, depois de uma AED, ja excluimos algumas correlacionadas
# Algumas tem valores altos, transforma-las para reduzir a influencia desses valores
RK$SQ.POLIC<-sqrt(RK$POLIC) #Policultura
RK$SQ.WATRES<-sqrt(RK$WAT.RES) #Reservatorio
RK$SQ.URBAN<-sqrt(RK$URBAN) #Area urbana
RK$SQ.OLIVE<-sqrt(RK$OLIVE) #Oliveiras
RK$SQ.LPROAD<-sqrt(RK$L.P.ROAD) #Estrada pavimentada
RK$SQ.SHRUB<-sqrt(RK$SHRUB) #Arbustos
RK$SQ.DWATCOUR<-sqrt(RK$D.WAT.COUR) #Distancia de curso dagua

# Novo modelo com as 9 variaveis
M2<-glm(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
          SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
          SQ.DWATCOUR+D.PARK,family=poisson,data=RK)
summary(M2)
# Observar: Parametros; AIC; Desvios.
# Pergunta 1: Quanto o conjunto de variaveis explicativas explica da variavel resposta?
pseudoR2 <- ((1071.44-270.23)/1071.44)*100
pseudoR2

## explains 74%, that is more than first model 
summary(M1)
pseudoR1 <- ((1071.4 - 390.9)/1071.4)*100
pseudoR1
# Pergunta 2: Preciso de todas elas? Ao menos duas nao sao significativas...

# Sera que nao tem colinearidade entre variaveis?
# Calcular VIF (Variance inflation factor)
install.packages("car", dependencies=TRUE)
library(car)
vif(M2) # Tudo ok!!! VIF precisa ser menor que 3
vars <- c(RK$SQ.DWATCOUR, RK$SQ.SHRUB, RK$SQ.LPROAD, RK$SQ.OLIVE, RK$SQ.URBAN, RK$SQ.WATRES, RK$SQ.POLIC,
          RK$L.SDI, RK$P.EDGE, RK$N.PATCH, RK$D.PARK)
cor(vars)
# Um comando que automatiza a selecao pelo AIC
step(M2) # definir forward ou backward ou both (ver help da funcao) 
# Interpretar o resultado

# Comando drop1
drop1(M2, test="Chi")

# Exclui uma variavel por vez
# Sempre que exclui uma variavel, o desvio aumenta
# Testa ("Chi") se esse aumento do desvio eh significativo ao ponto de justificar a inclusao da variavel retirada
# M2 tem desvio de 270.23, se tirar OPEN.L o desvio aumenta para 273.93.
# O aumento nao foi significativo, ou seja, posso retirar OPEN.L no proximo passo 
# (embora talvez seja melhor retirar SQ.DWATCOUR... it is up to you!!!)

# Proximo passo
M3 <- glm(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC + D.PARK +
            SQ.SHRUB + SQ.WATRES + L.WAT.C + SQ.LPROAD,
          family = poisson, data = RK)
summary(M3)

# Experimentando o comando anova
anova(M2, M3, test = "Chi")
# Compara diretamente dois modelos e a significancia da diferenca dos desvios
# O Output apresenta a diferenca dos desvios e se esta eh ou nao significante
# Esse comando eh perigoso em caso de interacao entre as variaveis explicativas

# Exercicio opcional:
# Siga adiante, voltando ao M2, usando o comando drop1, para selecionar o modelo adequado
M2<-glm(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
          SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
          SQ.DWATCOUR+D.PARK,family=poisson,data=RK)
summary(M2)

drop1(M2, test="Chi") # vamos retirar SQ.DWATCOUR pois eh a que menos aumenta o Deviance

M3<-glm(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
          SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
          D.PARK,family=poisson,data=RK)
summary(M3)

drop1(M3, test="Chi") # vamos retirar OPEN.L pois eh a que menos aumenta o Deviance

M4 <-glm(TOT.N~MONT.S+SQ.POLIC+
           SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
           D.PARK,family=poisson,data=RK)
summary(M4)

drop1(M4, test="Chi") # vamos retirar SQ.WATRES pois eh a que menos aumenta o Deviance

M5 <- glm(TOT.N~MONT.S+SQ.POLIC+
            SQ.SHRUB+L.WAT.C+SQ.LPROAD+
            D.PARK,family=poisson,data=RK)
summary(M5)

drop1(M5, test="Chi") # vamos retirar SQ.LPROAD pois eh a que menos aumenta o Deviance

M6 <- glm(TOT.N~MONT.S+SQ.POLIC+
            SQ.SHRUB+L.WAT.C+
            D.PARK,family=poisson,data=RK)

drop1(M6, test="Chi")

# Enfim, voce pode continuar retirando variaveis seguindo essa logica
# No entanto, perceba que o certo seria parar pelo M3, pois la todas as variaveis sao significativas
# Quem sabe nao olhamos para o AIC...
# Para montar a famosa tabela de AIC, siga os comandos abaixo (vc tem varias opcoes)

if(!require(MuMIn)){install.packages("MuMIn");library(MuMIn)} # Instalando pacotes requeridos
model.sel(M1, M2, M3, M4, M5, M6) 

# Tente interpretar essa tabela. Veja que ela esta ordenando os modelos pelo menor AIC
# Assim, o M3 eh de fato o mais parcimonioso

?dredge # aos curiosos (e preguicosos) explore a funcao dredge... bem legal... mas cuidado com automatizacoes...

# Uma opcao para tabelar pelo AIC quando temos overdispersion (aceita ate 4 - VEJA ITEM SEGUINTE EM QUE LIDAMOS COM SOBREDISPERSAO)

if(!require(AICcmodavg)){install.packages("AICcmodavg");library(AICcmodavg)} # Instalando pacotes requeridos

ModCandidatos<-list()
ModCandidatos[[1]]<-M1
ModCandidatos[[2]]<-M2
ModCandidatos[[3]]<-M3
ModCandidatos[[4]]<-M4
ModCandidatos[[5]]<-M5
ModCandidatos[[6]]<-M6

Modnames <- paste("Mod", 1:length(ModCandidatos), sep = " ")
res.table <- aictab(cand.set = ModCandidatos, modnames = Modnames,
                    second.ord = FALSE, c.hat = 4) # o c-hat eh o parametro de sobredispercao

res.table # Legal... vc pode usar essa opcao para incluir dispercao no AIC (existem criticas ao QuasiAIC)

res.table <- aictab(cand.set = ModCandidatos, modnames = Modnames,
                    second.ord = FALSE, c.hat = 1) # se nao quiser incluir dispercao, c-hat = 1

res.table # Veja que o output apresenta um quasiAIC e n??o o AIC 

# Mais uma opcao... veja que temos muitas...

if(!require(bbmle)){install.packages("bbmle");library(bbmle)}

AIC <- ICtab (M1, M2, M3, M4, M5, M6, base = TRUE, type = c("AICc"), weights = TRUE,
              delta = TRUE, sort = TRUE, nobs = 52, k = TRUE)
AIC


#~~~~~~~~~~~~~~~~~~~~~~~~#
# 5. Sobredispersao ----
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Rever output do M2
summary(M2)
# Dividir o desvio residual pelos graus de liberdade estima a sobredispersao
270.23/42
# Muito, muitoooo alto!!!

# Correcao para sobredispersao: quasipoisson 

Mquasi1<- glm(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +
                SQ.SHRUB + SQ.WATRES + L.WAT.C + SQ.LPROAD+
                SQ.DWATCOUR + D.PARK, family = quasipoisson, data = RK)

summary(Mquasi1)

# Observar: O parametro de dispersao de 5.93 (agora considerado na variancia)
# Pergunta: O que esse parametros de dispersao esta fazendo? Ele vai inflar os erros dos coeficientes

# Selecao com quasipoisson
# Mesma funcao drop1, porem agora a diferenca dos desvios segue uma distribuicao F (cuidado para nao errar aqui)

drop1(Mquasi1,test="F") # SQ.DWATCOUR eh a que menos aumenta os residuos, bora tira-la

Mquasi2<- glm(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC+
                SQ.SHRUB + SQ.WATRES + L.WAT.C + SQ.LPROAD+
                D.PARK, family = quasipoisson, data = RK)

summary(Mquasi2)

drop1(Mquasi2, test = "F")

Mquasi3<- glm(TOT.N ~ MONT.S + SQ.POLIC+
                SQ.SHRUB + SQ.WATRES + L.WAT.C + SQ.LPROAD+
                D.PARK, family = quasipoisson, data = RK)
summary(Mquasi3)

drop1(Mquasi3, test="F")
Mquasi4<- glm(TOT.N ~ MONT.S + SQ.POLIC+
                SQ.SHRUB + L.WAT.C + SQ.LPROAD+
                D.PARK, family = quasipoisson, data = RK)
summary(Mquasi4)

drop1(Mquasi4, test = "F")

Mquasi5<- glm(TOT.N ~ MONT.S + SQ.POLIC+
                SQ.SHRUB + L.WAT.C +
                D.PARK, family = quasipoisson, data = RK)
summary(Mquasi5)

drop1(Mquasi5, test="F")

Mquasi6<- glm(TOT.N ~ MONT.S + SQ.POLIC + L.WAT.C +
               D.PARK, family = quasipoisson, data = RK)
summary(Mquasi6)

drop1(Mquasi6, test="F")

Mquasi7<- glm(TOT.N ~ MONT.S + L.WAT.C +
                D.PARK, family = quasipoisson, data = RK)
summary(Mquasi7)

drop1(Mquasi7, test='F')

Mquasi8 <-  glm(TOT.N ~ L.WAT.C +
                  D.PARK, family = quasipoisson, data = RK)
summary(Mquasi8)

drop1(Mquasi8, test = "F")

Mquasi9 <-  glm(TOT.N ~ 
                  D.PARK, family = quasipoisson, data = RK)
summary(Mquasi9)

# Exercicio opcional:
# Siga adiante, a partir do Mquasipoisson1, usando o comando drop1, para seleionar o modelo adequado
# Compare com o resultado quando sobredispersao nao foi considerada


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 6. Validacao do modelos ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# O comando plot sintetiza o processo de validacao com quatro graficos (ver aula 3)
# Por exemplo, considerando o modelo M1
M1<-glm(TOT.N~D.PARK,family=poisson,data=RK)
par(mfrow=c(2,2))
plot(M1)
# Interprete: padroes sao observados? Nao tenho ceu estrelado; o que posso usar aqui se estou com outra distribuicao?

# Validacao para quasipoisson 
M1quasi<- glm(TOT.N ~ D.PARK, family = quasipoisson, data = RK)
plot(M1quasi)
par(mfrow = c(1,1))
# Interprete: padroes sao observados? Continuo com o mesmo problema...

# Esta funcao plot apresenta apenas o residuo ordinario
# Tenho outros residuos para GLM e posso (e devo) considera-lo na minha validacao
# Apenas observe o codigo abaixo
M1<-glm(TOT.N~D.PARK,family=quasipoisson,data=RK)
EP=resid(M1,type="pearson") # residual de pearson do M1
ED=resid(M1,type="deviance") # residual de desvio do M1
mu=predict(M1,type="response") # valores preditos
E=RK$TOT.N-mu # residual resposta (ordinario) do M1 (y-u)
EP2=E/sqrt(7.630148*mu) # o residual de pearson precisa ser dividido pela raiz quadrada da dispersao
par(mfrow=c(2,2))
plot(x=mu,y=E,main="Response residuals") # residuos pelos valores ajustados
plot(x=mu,y=EP,main="Pearson residuals")
plot(x=mu,y=EP2,main="Pearson residuals scaled")
plot(x=mu,y=ED,main="Deviance residuals")
par(mfrow=c(1,1))

# DICA: Use ao menos o plot dos desvios residuais pelos valores preditos (principal ferramenta de validacao)
ED=resid(M1,type="deviance") # residual de desvio do M1
mu=predict(M1,type="response") # valores preditos
plot(x=mu,y=ED,main="Deviance residuals") # Temos estrelas no ceu?

# Lembra do qqPlot para outras distribuicoes?
if(!require(car)){install.packages('car')};library(car)

qqPlot(RK$TOT.N, distribution="pois", lambda=mean(RK$TOT.N))

# DICA:
# 1. Plote os residuos x vari??veis explanatorias n??o incluidas no modelo: se ocorrer um padrao, inclua essa variavel;
# 2. Plote os residuos x vari??veis explanatorias incluidas no modelo: se ocorrer um padrao, inclua um termo quadr??tico;
# 3. Plote os residuos x tempo ou coordenadas espaciais: se ocorrer um padrao, inclua um termo de autocorrelacao (veremos adiante);
# 4. Plote os residuos x valores preditos: se ocorrer um padrao, ou ocorreu sobredispersao ou a distribuicao esta errada.


# Aos curiosos, explore o pacote DHARMa, para diagnostico dos residuos (https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html)
# DHARMa: simula residuos standardizados que podem ser interpretados como em uma regressao normal
# 1. Simula novos dados observados a partir do modelo ajustado.
# 2. Calcula a fun????o de densidade acumulada dos dados simulados e usa como probabilidades dos dados observados
# 3. Utiliza essa fun??ao baseada nos dados simulados para calcular novos res??duos
if(!require(DHARMa)){install.packages('DHARMa')};library(DHARMa)

# Voltando ao modelo M6
M6 <- glm(TOT.N~MONT.S+SQ.POLIC+
            SQ.SHRUB+L.WAT.C+
            D.PARK,family=poisson,data=RK)

# Calcular os residuos
simulationOutput <- simulateResiduals(fittedModel = M6, n = 1000)
residuals(simulationOutput)

# Testando dispersao
testDispersion(simulationOutput, type = "PearsonChisq")

# Plot principal para usar
plot(simulationOutput)
# O primeiro plot ?? um qqplot para a distribuicao em uso (neste exemplo, a vermelhidao sugere problemas)
# Residuos vs preditos: pontos vermelhos sao observacoes nao estao nos residuos simulados.


# Exercicio: depois de selecionar o modelo mais parcimonioso usando Poisson e quasipoisson,
# Valide ambos e discuta... 
# ATENCAO: quasipoisson nao? uma nova distribuicao. Mudou apenas a variancia.
# Entao posso validar no poisson ou no quasi que o resultado seria o mesmo.
# O que muda eh o modelo selecionado 


# EXTRA: TENDO UM MODELO SELECIONADO, O QUE MOSTRAMOS em um artigo)?
# 1. A Tabela de sele????o de modelos (pode ser um suplementar)
# 2. Os coeficientes ao longo do texto; Por exemplo: (??=-0.11, Std. Error=0.02, z=-3.93, P<0.001)
# 3. Os efeitos graficamente;
# 4. Os gr??ficos diagn??sticos de sele????o de modelo (pode ser um suplementar);

# Para o item 3, gosto muito do pacote sjPlot
if(!require(sjPlot)){install.packages("sjPlot");library(sjPlot)}
if(!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
if(!require(ggpubr)){install.packages("ggpubr");library(ggpubr)}

# Com ele podemos visualizar os efeitos de cada vari??vel
M6 <- glm(TOT.N~MONT.S+SQ.POLIC+
            SQ.SHRUB+L.WAT.C+
            D.PARK,family=poisson,data=RK)

(effects.D.PARK <- plot_model(M6, 
                              type = "eff", 
                              show.legend = FALSE, 
                              title = "", 
                              axis.title = c("D.PARK", "TOT.N"), 
                              term = c("D.PARK")))

(effects.SQ.SHRUB <- plot_model(M6, 
                                type = "eff", 
                                show.legend = FALSE, 
                                title = "", 
                                axis.title = c("SQ.SHRUB", "TOT.N"), 
                                term = c("SQ.SHRUB")))

(effects.MONT.S <- plot_model(M6, 
                              type = "eff", 
                              show.legend = FALSE, 
                              title = "", 
                              axis.title = c("MONT.S", "TOT.N"), 
                              term = c("MONT.S")))

(Fig1.Predicts <- ggarrange(effects.D.PARK, effects.SQ.SHRUB, effects.MONT.S,
                            labels = c("(a)", "(b)", "(c)"),
                            ncol = 3, nrow = 1, font.label = list(size = 9, color ="black")))


M.exemplo <- glm(TOT.N~MONT.S*D.PARK,family=poisson,data=RK)

(effects.M.exemplo <- plot_model(M.exemplo, 
                                 type = "eff", 
                                 show.legend = TRUE, 
                                 title = "", 
                                 axis.title = c("D.PARK", "TOT.N"), 
                                 term = c("D.PARK", "MONT.S")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 7. GLM com Binomial negativa ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# A funcao glm.nb precisa do pacote MASS
if(!require(MASS)){install.packages("MASS");library(MASS)}

# Os modelos (para os mesmos dados)
Mbn1<-glm.nb(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
               SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
               SQ.DWATCOUR+D.PARK,link="log",data=RK) # link="log"

summary(Mbn1)

# Observar: O theta = o parametro de dispersao

# Exercicio opcional: 
# Selecione o modelo com drop1 utilizando BN
drop1(Mbn1, test="Chi")

Mbn2<-glm.nb(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
               SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+D.PARK,link="log",data=RK)

summary(Mbn2)

drop1(Mbn2, test="Chi")

Mbn3<-glm.nb(TOT.N~OPEN.L+MONT.S+
               SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+D.PARK,link="log",data=RK)
summary(Mbn3)

drop1(Mbn3, test="Chi")

Mbn4<-glm.nb(TOT.N~OPEN.L+MONT.S+
               SQ.SHRUB+L.WAT.C+SQ.LPROAD+D.PARK,link="log",data=RK)
summary(Mbn4)

drop1(Mbn4, test="Chi")

Mbn5<-glm.nb(TOT.N~OPEN.L+MONT.S+L.WAT.C+SQ.LPROAD+D.PARK,link="log",data=RK)
summary(Mbn5)

drop1(Mbn5, test="Chi")

Mbn6<-glm.nb(TOT.N~OPEN.L+L.WAT.C+SQ.LPROAD+D.PARK,link="log",data=RK)
summary(Mbn5)

drop1(Mbn6, test="Chi")

Mbn7<-glm.nb(TOT.N~OPEN.L+L.WAT.C+D.PARK,link="log",data=RK)
summary(Mbn7)

drop1(Mbn7, test="Chi")

Mbn8 <- glm.nb(TOT.N~OPEN.L+D.PARK,link="log",data=RK)
summary(Mbn8)
# Veja a tabela AIC (instrucoes acima) - use ICtab
model.sel(Mbn1,Mbn2,Mbn3,Mbn4,Mbn5,Mbn6,Mbn7,Mbn8)
## or in another way
AIC <- ICtab (Mbn1,Mbn2,Mbn3,Mbn4,Mbn5,Mbn6,Mbn7,Mbn8,base = TRUE, type = c("AICc"), weights = TRUE,
              delta = TRUE, sort = TRUE, nobs = 52, k = TRUE)

AIC

# Valide o modelo com a funcao plot(Modelo) ou pelos desvios residuais e discuta (ou ainda pelo pacote DHARMa)
par(mfrow = c(2,2))
plot(Mbn6)
plot(Mbn5)
plot(Mbn8)
par(mfrow = c(1,1))

# Calcular os residuos
simulationOutput6 <- simulateResiduals(fittedModel = Mbn6, n = 1000)
residuals(simulationOutput6)

simulationOutput7 <- simulateResiduals(fittedModel = Mbn7, n = 1000)
residuals(simulationOutput7)

simulationOutput5 <- simulateResiduals(fittedModel = Mbn5, n = 1000)
residuals(simulationOutput5)

simulationOutput8 <- simulateResiduals(fittedModel = Mbn8, n = 1000)
residuals(simulationOutput8)


# Testando dispersao
testDispersion(simulationOutput6, type = "PearsonChisq")

testDispersion(simulationOutput7, type = "PearsonChisq")

testDispersion(simulationOutput5, type = "PearsonChisq")

testDispersion(simulationOutput8, type = "PearsonChisq")
# Plot principal para usar
plot(simulationOutput6)

plot(simulationOutput7)

plot(simulationOutput5)

plot(simulationOutput8)
summary(Mbn8)

Mbn5<-glm.nb(TOT.N~OPEN.L+MONT.S+L.WAT.C+SQ.LPROAD+D.PARK,link="log",data=RK)

(effects.D.PARK <- plot_model(Mbn5, 
                              type = "eff", 
                              show.legend = FALSE, 
                              title = "", 
                              axis.title = c("D.PARK", "TOT.N"), 
                              term = c("D.PARK")))

(effects.MONT.S <- plot_model(Mbn5, 
                              type = "eff", 
                              show.legend = FALSE, 
                              title = "", 
                              axis.title = c("MONT.S", "TOT.N"), 
                              term = c("MONT.S")))

(effects.OPEN.L <- plot_model(Mbn5, 
                              type = "eff", 
                              show.legend = FALSE, 
                              title = "", 
                              axis.title = c("OPEN.L", "TOT.N"), 
                              term = c("OPEN.L")))

(effects.L.WAT.C <- plot_model(Mbn5, 
                              type = "eff", 
                              show.legend = FALSE, 
                              title = "", 
                              axis.title = c("L.WAT.C", "TOT.N"), 
                              term = c("L.WAT.C")))

(effects.SQ.LPROAD <- plot_model(Mbn5, 
                               type = "eff", 
                               show.legend = FALSE, 
                               title = "", 
                               axis.title = c("SQ.LPROAD", "TOT.N"), 
                               term = c("SQ.LPROAD")))
(Fig2.Predicts <- ggarrange(effects.D.PARK, effects.SQ.LPROAD, effects.MONT.S, 
                            effects.OPEN.L,
                            effects.L.WAT.C,
                            labels = c("(a)", "(b)", "(c)", "(d)","(e)"),
                            ncol = 3, nrow = 2, font.label = list(size = 9, color ="black")))

# Compare o resultado com Poisson e quasipoisson

#~~~~~~~~~~~~~~~~~~~~~~~~#
# 8. Offset (incluir) ----
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de piolho do mar em fazendas de piscicultura na Escocia
Lice <- read.table(file.choose(), header = TRUE, dec = ".")
head(Lice)
Lice <- Lice[-253,]

# Descricao: coleta de plancton em 2 profundidades, 5 estacoes por dois anos 
# Variavel resposta (Copepod)
# Variaveis explanatorias: Station (Estacao de coleta); Depth (Profundidade)
# Veja que a variavel Station eh categorica e eu preciso defini-la como um fator
# Offset: Volume coletado

# Volume varia entre amostras - incluir como offset (vantagens ja discutidas)
Moffset<- glm(Copepod~offset(Volume)+factor(Station), family = poisson, data = Lice)
summary(Moffset) 
# Olhe o output... temos um parametro para cada nivel da variavel Station

### Agora siga em frente com o exercicio abaixo. Voce tem todas as ferramentas


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 9. Exercicio obrigatorio - GLM Poisson (veja introducao ao estudo de caso) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Dados: RIKZRichness
# Descricao: Riqueza da fauna bentonica em uma praia na Holanda e sua relacao com variaveis abioticas;
# Desenho amostral: 9 praias amostradas (3 para cada nivel de exposicao) e 5 amostras em cada praia

# Variavel resposta a considerar: Richness
# Variaveis explanatorias a considerar: 
# NAP: altura de coleta da amostra em relacao ao nivel da mare
# Exposure: indice de exposicao a acao de ondas
# Humus: quantidade de materia organica
# Week: qual semana de junho de 2002 ocorreu amostragem (1,2,3,4)
# Angle1: Angulo de cada praia
# Angle2: Angulo da area amostrada
# Salinity: Ok
# Temperature: Ok
# Penetrabilidade: Ok
# Grain size: Tamanho do grao
# Beach: Ok

# Objetivo: Encontrar relacoes entre riqueza de macrofauna e variaveis explicativas coletadas

# Missao: Explorar os dados (avaliar colinearidade); Construir um conjunto de modelos (GLM) para dados de contagem; 
# Selecionar o modelo; Validar o modelos; Discutir resultados (Qual sua conclusao? Eh possivel melhorar a analise? Qual sua sugestao?)

# Atencao: Talvez GLM nao seja a opcao otima para analisar esses dados, mas por hora, fique por aqui.

# UFA!