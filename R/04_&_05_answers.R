########################################################################
## Introducao a Modelagem Estatistica no R                            ##
## Codigo Aula 5 - Pratica - GLM                                      ## 
## PPGECO - UFSC - 20222                                              ##
## Preparado por: Fabio G. Daura-Jorge                                ##
########################################################################

### Codigo do material exposto na aula 5 (ppt) ###
### Exercicios ###
### Adaptado de Zuur et al. 2009 ###

## Apresentando GLM (Poisson & Binomial Negativa)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. GLM Poisson - Preparacao  #
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

# Por simplificacao, considerar uma unica variavel explicativa por hora
plot(TOT.N~D.PARK,xlab="Distancia do parque",
     ylab="Mortes na estrada", data=RK) # o que esse plot sugere em termos de distribuicao?


#~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. O Modelo            #
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Funcao glm, familia poisson;
M1<-glm(TOT.N~D.PARK,family=poisson,data=RK)
summary(M1)
# Observar: Parametros; AIC; Desvios.
# Pergunta: Quanto a variavel explicativa explica da variacao da variavel resposta?


#~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Valores preditos    #
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
# 4. Selecao de modelos  #
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
# Pergunta 2: Preciso de todas elas? Ao menos duas n?o s?o significativas...

# Sera que nao tem colinearidade entre variaveis?
# Calcular VIF (Variance inflation factor)
install.packages("car", dependencies=TRUE)
library(car)
vif(M2) # Tudo ok!!! VIF precisa ser menor que 3

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

install.packages("MuMIn")
library(MuMIn)
model.sel(M1, M2, M3, M4, M5, M6) 
# Tente interpretar essa tabela. Veja que ela esta ordenando os modelos pelo menor AIC
# Assim, o M3 eh de fato o mais parcimonioso

?dredge # aos curiosos (e preguicosos) explore a funcao dredge... bem legal... mas cuidado com automatizacoes...

# Uma opcao para tabelar pelo AIC quando temos overdispersion (aceita ate 4)

install.packages("AICcmodavg")
library(AICcmodavg)

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

res.table # Veja que o output apresenta um AIC e nao o quasiAIC

# Mais uma opcao... veja que temos muitas...

install.packages("bbmle")
library(bbmle)
AIC <- ICtab (M1, M2, M3, M4, M5, M6, base = TRUE, type = c("AICc"), weights = TRUE,
              delta = TRUE, sort = TRUE, nobs = 52, k = TRUE)
AIC


#~~~~~~~~~~~~~~~~~~~~~~~~#
# 5. Sobredispersao      #
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

# Exercicio opcional: RESOLVIDO
# Siga adiante, a partir do Mquasipoisson1, usando o comando drop1, para seleionar o modelo adequado
# Compare com o resultado quando sobredispersao nao foi considerada

drop1(Mquasi2, test="F") # Tirar OPEN L

Mquasi3<- glm(TOT.N ~ MONT.S + SQ.POLIC+
                SQ.SHRUB + SQ.WATRES + L.WAT.C + SQ.LPROAD+
                D.PARK, family = quasipoisson, data = RK)

summary(Mquasi3)

drop1(Mquasi3, test="F") # Tirar SQ.WATRES

Mquasi4<- glm(TOT.N ~ MONT.S + SQ.POLIC+
                SQ.SHRUB + L.WAT.C + SQ.LPROAD+
                D.PARK, family = quasipoisson, data = RK)

summary(Mquasi4)

drop1(Mquasi4, test="F") # Tirar SQ.LPROAD

Mquasi5<- glm(TOT.N ~ MONT.S + SQ.POLIC+
                SQ.SHRUB + L.WAT.C +
                D.PARK, family = quasipoisson, data = RK)

summary(Mquasi5)

drop1(Mquasi5, test="F") # Tirar SQ.SHRUB

Mquasi6<- glm(TOT.N ~ MONT.S +
                SQ.POLIC + L.WAT.C +
                D.PARK, family = quasipoisson, data = RK)

summary(Mquasi6)

drop1(Mquasi6, test="F") # Tirar SQ.POLIC

Mquasi7<- glm(TOT.N ~ MONT.S +
                L.WAT.C +
                D.PARK, family = quasipoisson, data = RK)

summary(Mquasi7) # Opa, ao menos agora tudo esta significativo

drop1(Mquasi7, test="F") # Mas ainda da para tentar tirar MONT.S

Mquasi8<- glm(TOT.N ~ 
                L.WAT.C +
                D.PARK, family = quasipoisson, data = RK)

summary(Mquasi8) # L.WAT.C eh marginal... que duvida

drop1(Mquasi8, test="F") # Droga... tiro ou nao L.WAT.C? 

Mquasi9<- glm(TOT.N ~ 
                D.PARK, family = quasipoisson, data = RK)

summary(Mquasi9) 

# ATENCAO: Nao temos o AIC para quasipoisson. O que podemos fazer eh usar poisson e 
# incluir a dispercao no calculo do AIC para chegar no quasiAIC. Fizemos isso nas linhas 181 e 182
# O problema eh que o resultado seria bem diferente
# La ficariamos com o modelo 5: TOT.N ~ MONT.S + SQ.POLIC + D.PARK + SQ.SHRUB + L.WAT.C + SQ.LPROAD
# E agora chegamos no modelo: TOT.N ~ D.PARK ou TOT.N ~ L.WAT.C + D.PARK

# Vamos validar nossas duas opcoes de modelos 

plot(Mquasi8)
plot(Mquasi9) # Opa, parece que temos problemas (alem da sobredispercao)
# Mas esses graficos nao sao uma boa opcao aqui...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 6. Validacao do modelos   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# O comando plot sintetiza o processo de validacao com quatro graficos (ver aula 3)
# Por exemplo, considerando o modelo M1
M1<-glm(TOT.N~D.PARK,family=poisson,data=RK)
par(mfrow=c(2,2))
plot(M1)
# Interprete: padroes sao observados? Nao tenho ceu estrelado

# Validacao para quasipoisson 
M1quasi<- glm(TOT.N ~ D.PARK, family = quasipoisson, data = RK)
plot(M1quasi)
# Interprete: padroes sao observados? Continuo com o mesmo problema...

# Esta fun?ao plot apresenta apenas o residuo ordinario
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

# DICA: Use ao menos o plot dos desvios residuais pelos valores preditos (principal ferramenta de valida?ao)
ED=resid(M1,type="deviance") # residual de desvio do M1
mu=predict(M1,type="response") # valores preditos
plot(x=mu,y=ED,main="Deviance residuals") # Temos estrelas no ceu?

# Lembra do qqPlot para outras distribuicoes?
install.packages("car")
require(car)
qqPlot(RK$TOT.N, distribution="pois", lambda=mean(RK$TOT.N))

# Exercicio: depois de selecionar o modelo mais parcimonioso usando Poisson e quasipoisson,
# Valide ambos e discuta... 
# ATEN?AO: quasipoisson n?o ? uma nova distribuicao. Mudou apenas a variancia.
# Entao posso validar no poisson ou no quasi que o resultado seria o mesmo.
# O que muda eh o modelo selecionado

# Ok, validando o modelo M3 de poisson
ED3=resid(M3,type="deviance") # residual de desvio do M3
mu3=predict(M3,type="response") # valores preditos
# E o modelo Mquasi9 de quasipoisson
ED9=resid(Mquasi9,type="deviance") # residual de desvio do Mquasi9
mu9=predict(Mquasi9,type="response") # valores preditos

# Plotando lado a lado
par(mfrow=c(1,2))
p<-plot(x=mu3,y=ED3,main="Deviance residuals") # Temos estrelas no ceu? Mais ou menos... podia ser pior...
q<-plot(x=mu9,y=ED9,main="Deviance residuals") # Acho ate que piorou aqui... Nao ta bom nao...
par(mfrow=c(1,1)) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 7. GLM com Binomial negativa   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# A funcao glm.nb precisa do pacote MASS
library(MASS)

# Os modelos (para os mesmos dados)
Mbn1<-glm.nb(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
               SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
               SQ.DWATCOUR+D.PARK,link="log",data=RK) # link="log"

summary(Mbn1)

# Observar: O theta = o parametro de dispersao

# Exercicio opcional: 
# Selecione o modelo com drop1 utilizando BN
# Veja a tabela AIC (instrucoes abaixo)
# Valide o modelo com a funcao plot(Modelo) e discuta
# Compare o resultado com Poisson e quasipoisson

# Montar tabela AIC
# Vou repetir todos os modelos acima (embora por drop1, poderia parar antes de chegar no fim...)

drop1(Mbn1, test="Chi") # Tirar SQ.DWATCOUR

Mbn2<-glm.nb(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
               SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
               D.PARK,link="log",data=RK) # link="log"
summary(Mbn2)

drop1(Mbn2, test="Chi") # Tirar SQ.POLIC

Mbn3<-glm.nb(TOT.N~OPEN.L+MONT.S+
               SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
               D.PARK,link="log",data=RK) # link="log"
summary(Mbn3)

drop1(Mbn3, test="Chi") # Tirar SQ.WATRES

Mbn4<-glm.nb(TOT.N~OPEN.L+MONT.S+
               SQ.SHRUB+L.WAT.C+SQ.LPROAD+
               D.PARK,link="log",data=RK) # link="log"

summary(Mbn4) 

drop1(Mbn4, test="Chi") # Tirar SQ.SHRUB

Mbn5<-glm.nb(TOT.N~OPEN.L+MONT.S+
               L.WAT.C+SQ.LPROAD+
               D.PARK,link="log",data=RK) # link="log"
summary(Mbn5)

drop1(Mbn5, test="Chi") # Tirar MONT.S

Mbn6<-glm.nb(TOT.N~OPEN.L+
               L.WAT.C+SQ.LPROAD+
               D.PARK,link="log",data=RK) # link="log"
summary(Mbn6)

drop1(Mbn6, test="Chi") # Tirar SQ.LPROAD

Mbn7<-glm.nb(TOT.N~OPEN.L+
               L.WAT.C+
               D.PARK,link="log",data=RK) # link="log"
summary(Mbn7)

drop1(Mbn7, test="Chi") # Poderia parar por aqui... mas vou seguir um pouco mais... Tirar L.WAT.C

Mbn8<-glm.nb(TOT.N~OPEN.L+
               D.PARK,link="log",data=RK) # link="log"
summary(Mbn8)

drop1(Mbn8, test="Chi") # Opa... agora ta ok... vou tirar mais uma so para olhar o AIC 

Mbn9<-glm.nb(TOT.N~
               D.PARK,link="log",data=RK) # link="log"
summary(Mbn9)

## Eu ficaria com o modelo Mbn8
## Mas vamos olhar para o AIC

AIC <- ICtab (Mbn1, Mbn2, Mbn3, Mbn4, Mbn5, Mbn6, Mbn7,
              Mbn8, Mbn9, type = c("AIC"), base = TRUE, weights = TRUE,
              delta = TRUE, sort = TRUE, nobs = 52)?
  
  AIC # Veja so, pelo AIC, devemos ficar com o Mbn6, mas la SQ.LPROAD nao eh significativo
# Veja que eu tenho varios modelos que suportam os dados (Mbn6, Mbn5, Mbn7, Mbn4, Mbn3 e Mbn8)
# Se eu for pelo AIC, fico com Mbn6. Se eu for por Drop1 e pelos valores de p, Mbn7 ou Mbn8.
# Quem disse que modelar nao eh uma arte? O importante eh vc deixar claro suas decisoes
# Quando tenho incertezas como essa, eh um indicio de que algo nao vai tao bem...

# Vamos validar o Modelo Mbn6 com um grafico de residuos
ED6=resid(Mbn6,type="deviance") # residual de desvio do Mbn6
mu6=predict(Mbn6,type="response") # valores preditos
p<-plot(x=mu6,y=ED6,main="Deviance residuals") # Razoavel...

# E o modelo Mbn8

ED8=resid(Mbn8,type="deviance") # residual de desvio do Mbn8
mu8=predict(Mbn8,type="response") # valores preditos
p<-plot(x=mu8,y=ED8,main="Deviance residuals") # Razoavel...

# Concluindo: com binomial negativa resolvemos o problema de um padrao nos residuos
# e lido melhor com a sobredispercao

# Mas garanto... poderiamos melhorar essa analise...

#~~~~~~~~~~~~~~~~~~~~~~~~#
# 8. Offset (incluir)    #
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de piolho do mar em fazendas de piscicultura na Escocia
Lice <- read.table(file.choose(), header = TRUE, dec = ".")
head(Lice)

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
# 9. Exercicio obrigatorio - GLM Poisson (veja introducao ao estudo de caso) #
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