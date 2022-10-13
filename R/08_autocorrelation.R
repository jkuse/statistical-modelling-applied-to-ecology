##################################################################
## Introducao a Modelagem Estatistica no R                      ##
## Codigo Aula 8 - Pratica - Modelos com autocorrelacao         ##
## PPGECO - UFSC - 2022.2                                       ##
## Preparado por: Fabio G. Daura-Jorge                          ##
##################################################################

### Codigo do material exposto na aula  (ppt) ###

# Ative os pacotes necessarios e, se nao tiverem instalados na maquina, 
# instale-os antes de rodar os codigos abaixo
if(!require(forecast)){install.packages('forecast'); library(forecast)}
if(!require(nlme)){install.packages('nlme'); library(nlme)}
if(!require(spatstat)){install.packages('spatstat'); library(spatstat)}
if(!require(spdep)){install.packages('spdep'); library(spdep)}
if(!require(ncf)){install.packages('ncf'); library(ncf)}
if(!require(tseries)){install.packages('tseries'); library(tseries)}
if(!require(car)){install.packages('car'); library(car)}
if(!require(pgirmess)){install.packages('pgirmess'); library(pgirmess)}
if(!require(gstat)){install.packages('gstat'); library(gstat)}
if(!require(vegan)){install.packages('vegan'); library(vegan)}
if(!require(aTSA)){install.packages('aTSA'); library(aTSA)}
library(lme4)
library(forecast)
library(nlme)
install.packages('glmmTMB')
library(glmmTMB)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. TIME SERIES ANALYSIS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Dados de concentracao de dioxido de carbono medida no topo do vulcao Mauna Loa
data(co2)
class(co2)
head(co2)

# Vamos plotar
plot(co2)

# Funcoes de autocorrelacao
acf(co2)
pacf(co2)
?pacf

# Seasonal decomposition
fit <- stl(co2, s.window="period")
?stl
plot(fit)

# additional plots (pacote 'forecast')
monthplot(co2)
seasonplot(co2)
### Nao vimos no teórico, mas vamos demostrar o ARIMA para ajustar um modelo aos dados
### So ilustracao

### Testando a premissa de estacionaridade
adf.test(co2)


#~~~~~~~~~~~~~~~~~~~~#
# 2. GLS para Autocorrelacoes temporal----
#~~~~~~~~~~~~~~~~~~~~#

# Dados referentes a abundacia de aves em ilhas do Havaí:  1965-2003
Hawaii <- read.table(file.choose(), header = TRUE, dec = ".") # waterbird data
head(Hawaii)
str(Hawaii)

Hawaii$Birds<-sqrt(Hawaii$Moorhen_Kauai)

plot(Hawaii$Year,Hawaii$Moorhen_Kauai,xlab="Year",
     ylab="Moorhen abundance on Kauai")

plot(Hawaii$Year,Hawaii$Birds,xlab="Year",
     ylab="Moorhen abundance on Kauai")


# Ajustando um modelo GLS sem estrutura de autocorrelacao = lm
M0 <- gls(Birds ~ Rainfall + Year, na.action = na.omit, data = Hawaii) 
summary(M0)

# Diagnostico pela funcao ACF
E<-residuals(M0,type="normalized")
I<-!is.na(Hawaii$Birds)
Efull<-vector(length=length(Hawaii$Birds))
Efull<-NA
Efull[I]<-E
acf(Efull,na.action = na.pass,
    main="Auto-correlation plot for residuals")


# Ajustando modelo alternativo com estrutura de autocorrelacao corCompSymm: constante correlacao no tempo
M1<-gls(Birds ~ Rainfall + Year, na.action = na.omit,
        correlation = corCompSymm(form =~ Year),
        data=Hawaii)
summary(M1)

# Ajustando modelo alternativo com estrutura de autocorrelacao Auto.regressiva: com parametro p referente a correlacao de ordem 1
M2<-gls(Birds ~ Rainfall + Year, na.action = na.omit,
        correlation = corAR1(form =~ Year), data = Hawaii)
summary(M2)


# Ajustando modelo alternativo com estrutura de autocorrelacao Auto.regressiva e Media-movel: 
# com parâmetro p referente a correlacao de ordem 1 e q referente ao numero de parametros da media movel
?corARMA
cs1 <- corARMA(p = 1, q = 0)
cs2 <- corARMA(p = 2, q = 0) # veja que q = 0, ARMA = AR; p = 2 = AR.2
M3arma1<-gls(Birds~Rainfall+Year,na.action=na.omit,
             correlation=cs1,data = Hawaii)
M3arma2<-gls(Birds~Rainfall+Year,na.action=na.omit,
             correlation=cs2,data = Hawaii)

AIC(M3arma1,M3arma2)

# Voce pode explorar outros modelos variando o valor de p e q entre 0 e 3


#~~~~~~~~~~~~~~~~~~~~#
# 3. GLS para Autocorrelacoes Espacial----
#~~~~~~~~~~~~~~~~~~~~#

# Dados referentes a Indice florestal(floresta boreal Russa) em funcao de variaveis de habitat amostradas via SIG
Boreality <- read.table(file.choose(), header = TRUE, dec = ".") # data Boreality
head(Boreality)
str(Boreality)


# Reescalonando os dados
Boreality$Bor<-sqrt(1000*(Boreality$nBor+1)/(Boreality$nTot))


# Ajustando um modelo linear para comecar
B.lm<-lm(Bor~Wet,data=Boreality)
summary(B.lm)

library(sp)
# Avaliando correlacao espacial via bubble
E<-rstandard(B.lm)
mydata<-data.frame(E,Boreality$x,Boreality$y)
coordinates(mydata)<-c("Boreality.x","Boreality.y")
bubble(mydata,"E",col=c("black","grey"),
       main="Residuals",xlab="X-coordinates",
       ylab="Y-coordinates")
library(gstat)

# Avaliando correlacao espacial via Variograma
Vario1 = variogram(E ~ 1, mydata)
plot(Vario1)


# Ajustando o modelo sem correlacao
B1.gls<-gls(Bor ~ Wet, data = Boreality) 


# Variograma para dignostico da autocorrelacao
var1<-Variogram(B1.gls,form=~x+y,robust=TRUE,maxDist=2000,
                resType="pearson")
plot(var1,smooth=T)


# Incluindo diferentes opcoes de estrutura de correlacao ao modelo 
B1A<-gls(Bor ~ Wet, correlation=corSpher(form=~x+y,nugget=T),data=Boreality)
B1B<-gls(Bor ~ Wet, correlation=corLin(form=~x+y,nugget=T),data=Boreality)
B1C<-gls(Bor ~ Wet, correlation=corRatio(form=~x+y,nugget=T),data=Boreality)
B1D<-gls(Bor ~ Wet, correlation=corGaus(form=~x+y,nugget=T),data=Boreality)
B1E<-gls(Bor ~ Wet, correlation=corExp(form=~x+y,nugget=T),data=Boreality)

# Seleccao por AIC
AIC(B1A,B1C,B1D,B1E)


# Diagnostico por Variograma
Vario2A <- Variogram(B1A, form =~ x + y,
                     robust =  TRUE, maxDist = 2000,
                     resType = "normalized")
plot(Vario2A, smooth = FALSE)

Vario2E <- Variogram(B1E, form =~ x + y,
                     robust =  TRUE, maxDist = 2000,
                     resType = "normalized")
plot(Vario2E, smooth = FALSE)

#~~~~~~~~~~~~~~~~~~~~#
# 4. EXERCICIOS ----
#~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Ex. 4.1 - Nicholson's Blowflies ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Importe a tabela de dados para o R, conferindo se tudo esta OK

flies<-read.table(file.choose(),header=T)
str(flies)

flies$time3 <- seq(from=1, to=361, by=1)

# Transforme o vetor em um vetor da classe 'time series'
flies.ts <- ts(flies)

# Plote a serie de dados em um grafico
plot(flies)

# Plote os graficos de autocorrelacao e autocorrelacao parcial
# Descreve os padroes de autocorrelacao serial
# Qual(is) lag(s) parece(m) ser relevante(s)?
par(mfrow = c(2,1))
acf(flies)
pacf(flies)
par(mfrow = c(1,1))
# Ajuste modelos ARMA aos dados e ache o modelo mais parcimonioso
# Qual a estrutura de correlacao serial mais adequada para os dados?

cs1 <- corARMA(p = 1, q = 0)
cs2 <- corARMA(p = 2, q = 0) # veja que q = 0, ARMA = AR; p = 2 = AR.2
Marma1<-gls(flies ~ time3 , na.action=na.omit,
             correlation=cs1,data = flies)
Marma2<-gls(Birds~Rainfall+Year,na.action=na.omit,
             correlation=cs2,data = Hawaii)

AIC(Marma1,Marma2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Ex. 4.2 - Acaros orobatideos ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Vamos chamar os dados
data(mite) # Abundancia de 35 especies de Acaros oribatideos
data(mite.xy) # coordenadas espaciais
data(mite.env) # variaveis ambientais

# Vamos trabalhar com os dados da especie 'Brachy', primeira coluna de 'mite'

# Faca uma analise exploratoria destes dados
hist(mite$Brachy)
# Plote os dados em um mapa

# Ajuste modelos para explicar a abundancia desta especie
# Considere modelos 'gls' com e sem autocorrelacao espacial
# Use diferentes estruturas de autocorrelacao espacial
# Ache o melhor modelo, de um conjunto considerando diferentes combinacoes de variaveis explanatorias
# Valide o melhor modelo