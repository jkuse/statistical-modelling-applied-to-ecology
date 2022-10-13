########################################################################
## Introducao a Modelagem Estatistica no R                            ##
## Codigo Aula 7 - Pratica - GAM                                      ##
## PPGECO - UFSC - 20222                                              ##
## Preparado por: Fabio G. Daura-Jorge                                ##
########################################################################

### Codigo do material exposto na aula 7 (ppt) ###
### Exercicios ###
### Adaptado de Zuur et al. 2009 ###

## Apresentando Modelos Aditivos e GAM


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. GAM em gam com LOESS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de bioluminescencia pelagica em um gradiente de profundidade 
ISIT <- read.table(file.choose(), header = TRUE, dec = ".")
head(ISIT)
str(ISIT)

# Variavel resposta: Sources (Medida de bioluminescencia)
# Variaveis explanatorias: SampleDepth (Profundidade) e Station (Estacao)
# Por ora trabalhar apenas com dados da Estacao 16
Sources16<-ISIT$Sources[ISIT$Station==16]
Depth16<-ISIT$SampleDepth[ISIT$Station==16]
# Ver relacao grafica
plot(Depth16, Sources16,type="p") # O que voces acham?


#~~~~~~~~~~~~~~~~~~~~~~#
# 2. O Modelo com gam ----
#~~~~~~~~~~~~~~~~~~~~~~#

install.packages("gam") 
library(gam) # pacote gam com a funcao lo (Loess)
M1<-gam(Sources16~lo(Depth16,span=0.5)) # destaque para lo (funcao Loess) e span (largura da banda)
summary(M1)
plot(M1, se=T)
# Observar: Parametros; interpretar... 
# Preste atencao nos eixos dos graficos... o que esta acontecendo?


#~~~~~~~~~~~~~~~~~~~~~~~#
# 3. O Modelo com mgcv ----
#~~~~~~~~~~~~~~~~~~~~~~~#

detach("package:gam") # para nao confundir com gam de gam
install.packages("mgcv") 
library(mgcv) # pacote com spline

# Vamos fazer um primeiro modelo bem audacioso... assumindo que a relacao entre variaveis eh a mesma para todas as estacoes
fStation <- factor(ISIT$Station)
M1sb <- gam(Sources ~ s(SampleDepth) + fStation, data = ISIT) # veja o codigo
summary(M1sb)
plot(M1sb) # Vamos interpretar olhando os parametros

# Agora vamos considerar apenas duas estacoes, 8 e 13 (para repetir o que ocorreu na teorica)
# Algumas preparacoes... apenas "run the code" - organiza os dados, biolumi e prof, por estacoes
S8 <- ISIT$Sources[ISIT$Station == 8]
D8 <- ISIT$SampleDepth[ISIT$Station == 8]
S13 <- ISIT$Sources[ISIT$Station == 13]
D13 <- ISIT$SampleDepth[ISIT$Station == 13]
So <- c(S8, S13); 
De <- c(D8, D13)
ID <- rep(c(8, 13), c(length(S8), length(S13)))
mi <- max(min(D8), min(D13))
ma <- min(max(D8), max(D13))
data.df <- data.frame(So = So,
                      De = De,
                      ID = ID)
if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} # adoro esse pacote
library(dplyr)
data.df <- data.df %>%
  filter(De >= mi) %>%
  filter(De <= ma)
sort(data.df$De)
# Esse codigo acima retira os dados das estacoes 8 e 13, inclui em um dataframe, e considerando apenas as profundidades comuns a ambas

# Grafico Biolumi por profundidade em duas estacoes
op <- par(mfrow = c(1, 2))
plot(data.df$De[data.df$ID==8], data.df$So[data.df$ID==8], pch = 16, xlab = "Profundidade",
     ylab = "Fonte", col = 1, main = "Estacao 8",
     xlim = c(500, 2700), ylim = c(0, 40))
plot(data.df$De[data.df$ID==13], data.df$So[data.df$ID==13], pch = 16, xlab = "Profundidade",
     ylab = "Fonte", col = 1, main = "Estacao 13",
     xlim = c(500, 2700), ylim = c(0, 40))
par(op) # observe os padroes graficos

# Agora (depois da confusao acima) as variaveis sao:
# Resposta: (So - bioluminescencia)
# Explanatorias: De (profundidade) e ID (Estacao - 8 ou 13)
data.df$fID <- factor(data.df$ID)  # Estacao eh um fator

# O modelo considerando uma unica suavizacao para as duas estacoes:
M1s <- gam(So ~ s(De) + fID, data = data.df) # "s" eh a funcao de suavizacao
# subset garante o uso de observacoes de uma mesma profundidade nos diferentes fatores
summary(M1s)

# Observar: Parametros; edf, Desvio explicado.
# Monte o modelo numerico para as duas estacoes com base nos parametros estimados
# Interprete o grafico:
par(mfrow = c(1, 1))
plot(M1s)

# Interprete o grafico:
par(mar=c(2,2,2,2))
vis.gam(M1s,theta=120,color="heat") # massa..:)

# Validar o modelo
par(mfrow = c(2, 2))
gam.check(M1s)
par(mfrow = c(1, 1))
# Observa padroes? Mais ou menos neh...

# O modelo considerando interacao entre profundidade e estacao pode ser montado por:
M2s<-gam(So ~ s(De) + s(De, by = as.numeric(ID == 13)) + factor(ID),
         data = data.df) # by=as.numeric discrimina a suavizacao de 13
summary(M2s)
par(mfrow = c(1, 2))
plot(M2s) # o que sao esses graficos? (rever teorica)
par(mfrow = c(2, 2))
gam.check(M2s)
par(mfrow = c(1, 1))
# Interprete: o que estamos fazendo?

# O modelo considerando uma suavizacao para cada estacao:
M3s <- gam(So~s(De,by = as.numeric(ID == 8))+
             s(De,by = as.numeric(ID == 13)),data = data.df)
summary(M3s)
par(mfrow = c(1, 2))
plot(M3s)
par(mfrow = c(2, 2))
gam.check(M3s)
par(mfrow = c(1, 1))
# Interprete: o que estamos fazendo?

# Qual modelo usar? Pela valida??o j? posso ter uma ideia... 
# E sao comparaveis por AIC
AIC(M1s); AIC(M2s); AIC(M3s)


#~~~~~~~~~~~~~~~~~~~~~~~#
# 4. GAM com Poisson  ----
#~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de piolho do mar em fazendas de peixes na Escocia
Lice <- read.table(file.choose(), header = TRUE, dec = ".")
head(Lice)

# Preparacoes recomendadas das variaveis a considerar: profundidade, estacao (local) e producao semanal
Lice$LVol<-log(Lice$Volume) # offset
Lice$fStation<-factor(Lice$Station) # Variavel explanatorias (5 estacoes como fator)
Lice$fDepth<-factor(Lice$Depth) # Variavel explanatorias (duas como fator)
Lice$PW<-Lice$Production_week # Variavel explanatorias (produtividade semanal)
# Variavel resposta: Copepod (Abundancia de Copepodes)
# A AED nao mostra clara relacao entre variaveis e GAM eh uma opcao

# O modelo mais complicado seria (uma suavizacao para cada estacao): 
library(mgcv)
L1<-gam(Copepod~offset(LVol)+
          s(PW,by=as.numeric(Depth=="0m" & Station=="A"))+
          s(PW,by=as.numeric(Depth=="0m" & Station=="C"))+
          s(PW,by=as.numeric(Depth=="0m" & Station=="E"))+
          s(PW,by=as.numeric(Depth=="0m" & Station=="F"))+
          s(PW,by=as.numeric(Depth=="0m" & Station=="G"))+
          s(PW,by=as.numeric(Depth=="5m" & Station=="A"))+
          s(PW,by=as.numeric(Depth=="5m" & Station=="C"))+
          s(PW,by=as.numeric(Depth=="5m" & Station=="E"))+
          s(PW,by=as.numeric(Depth=="5m" & Station=="F"))+
          s(PW,by=as.numeric(Depth=="5m" & Station=="G"))+
          fDepth + fStation,
        family=nb, data=Lice)
summary(L1)
# Observar: foi usado a distribuicao binomial negativa (nb); Poisson teria problemas de convergencia
par(mfrow = c(2, 5))
plot(L1)
par(mfrow = c(1, 1))
# Interprete o modelo: o que esta fazendo?

# E este modelo, o que esta fazendo?
L3 <- gam(Copepod ~ offset(LVol) +
            s(PW, by = as.numeric(Depth=="0m")) +
            s(PW, by = as.numeric(Depth=="5m")) +
            fDepth + fStation, family = nb, data = Lice)
summary(L3)
par(mfrow = c(1, 2))
plot(L3)
par(mfrow = c(1, 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 5. GAM com presenca-ausencia ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Entrar dados de parasitas em bacalhau
ParasiteCod <- read.table(file.choose(), header = TRUE, dec = ".")
head(ParasiteCod)

# Preparacoes recomendadas
ParasiteCod$fArea <- factor(ParasiteCod$Area) # Variavel explanatoria 
ParasiteCod$fYear <- factor(ParasiteCod$Year) # Variavel explanatoria
ParasiteCod$Length
ParasiteCod$Prevalence # Variavel resposta

# O modelo
P2 = gam(Prevalence~factor(Area)*factor(Year)+
           s(Length),family=binomial, data = ParasiteCod)
summary(P2)
anova(P2)
# Interprete o modelo: o que esta fazendo?
plot(P2)

# tente criar novos modelos e encontrar o mais parcimonioso...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 6. Exercicio obrigatorio - GAM ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Dados: Bahama
# Descricao: Densidade de peixe papagaio em diferentes condicoes de habitat e metodo de coleta;
# Desenho amostral: Densidade de peixe papagaio registrada em diferentes condicoes de cobertura de coral e metodos de coleta (ponto de contagem ou transectos).

# Variavel resposta a considerar: Parrot (densidade)
# Variaveis explanatorias a considerar: 
# CoralRichness: Riqueza do coral
# CoralTotal: Cobertura do coral
# Month: Mes de coleta 
# Station: Estacao de coleta
# Method: Metodo de coleta

# Objetivo: Avaliar a relacao entre a densidade de peixes papagaio e a riqueza de especies no coral, levando em consideracao o metodo de coleta (para esse exercicio as demais variaveis devem ser ignoradas)

# Missao: Explorar os dados (avaliar a relacao entre densidade de papagaios e riqueza de coral); Construir um conjunto de modelos (GAM) para a relacao densidade e riqueza, incluindo ou nao a interferencia do metodo de amostragem; Selecionar e validar o modelo; Discutir resultados (Qual sua conclusao? Eh possivel melhorar a analise? Qual sua sugestao?)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 7. Exercicio obrigatorio - GAM ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Dados: RIKZRichness (novamente)
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
# Grain size: Tamanho do grao
# Beach: Ok

# Objetivo: Avaliar a relacao entre riqueza de especies e a variavel NAP, considerando exposicao (exposure) como uma variavel nominal (3 niveis).

# Missao: Explorar os dados (avaliar a relacao entre variaveis); 
# Construir um conjunto de modelos (GAM) para a relacao de riqueza com temperatura e tamanho do grao, incluindo ou nao a exposicao como variavel nominal; Selecionar e validar o modelo; Discutir resultados (Qual sua conclusao? Eh possivel melhorar a analise? Qual sua sugestao?)
