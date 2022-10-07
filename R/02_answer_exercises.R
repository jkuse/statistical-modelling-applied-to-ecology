#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####     EXERCICIOS    ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 1. Quarteto de Anscombe  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Utilize o banco de dados 'anscombe'
data(anscombe)

# Calculando as medias e desvios padrao
apply(anscombe, 2, mean)
apply(anscombe, 2, sd)

# Indices de correlacao
with(anscombe,cor(x1,y1))
with(anscombe,cor(x2,y2))
with(anscombe,cor(x3,y3))
with(anscombe,cor(x4,y4))

# Ajustando regressao linear
with(anscombe,lm(y1 ~ x1)) -> lm1
with(anscombe,lm(y2 ~ x2)) -> lm2
with(anscombe,lm(y3 ~ x3)) -> lm3
with(anscombe,lm(y4 ~ x4)) -> lm4

# Comparando coeficientes
coef(lm1)
coef(lm2)
coef(lm3)
coef(lm4)

# Plotando os graficos
par(mfrow=c(2,2)) # 4 graficos em uma janela
plot(y1~x1, data=anscombe)
abline(lm1)
plot(y2~x2, data=anscombe)
abline(lm2)
plot(y3~x3, data=anscombe)
abline(lm3)
plot(y4~x4, data=anscombe)
abline(lm4)
par(mfrow=c(1,1))



#~~~~~~~~~~~~~~~~#
#### 2. Iris  ####
#~~~~~~~~~~~~~~~~#

# Vamos usar um banco de dados usado por Fisher (1936)
# Sao medidas florais de tres especies
data(iris)
str(iris)


# Usando as tecnicas mostradas em aula de Analise Exploratoria de Dados, 
# Responda as seguintes perguntas:

# Como eh a distribuicao das medidas de cada variavel?
summary(iris)

par(mfrow=c(2,2))
hist(iris[,1])
hist(iris[,2])
hist(iris[,3])
hist(iris[,4])

# Tem outliers?
dotchart(iris[,1])
dotchart(iris[,2])
dotchart(iris[,3])
dotchart(iris[,4])

# Tem distribuicao normal?
qqnorm(iris[,1])
qqline(iris[,1])
qqnorm(iris[,2])
qqline(iris[,2])
qqnorm(iris[,3])
qqline(iris[,3])
qqnorm(iris[,4])
qqline(iris[,4])
par(mfrow=c(1,1))

# As variaveis estao correlacionadas?
cor(iris[,1:4])
pairs(iris[,1:4], panel=panel.smooth)

# Existe diferenca das medidas entre as tres especies? Compare as medias,
tapply(iris[,1], iris[,5], mean)
tapply(iris[,2], iris[,5], mean)
tapply(iris[,3], iris[,5], mean)
tapply(iris[,4], iris[,5], mean)

# plote graficos
par(mfrow=c(2,2))
boxplot(iris[,1]~iris[,5])
boxplot(iris[,2]~iris[,5])
boxplot(iris[,3]~iris[,5])
boxplot(iris[,4]~iris[,5])
par(mfrow=c(1,1))

# Analise de variancia
aov(iris[,1]~iris[,5]) -> anova.iris
summary(anova.iris)

lm(iris[,1]~iris[,5]) -> lm.iris
summary(lm.iris)
