### EDA exercise with dataset iris

# Vamos usar um banco de dados usado por Fisher (1936)
# Sao medidas florais de tres especies
data(iris)
help(iris)
new_iris <- iris[,-5]

# Usando as tecnicas mostradas em aula de Analise Exploratoria de Dados, 
# Responda as seguintes perguntas:

# Como eh a distribuicao das medidas de cada variavel?
head(iris)
par(mfrow = c(2,2))
hist(iris$Sepal.Length, prob=T)
curve(dnorm(x, mean = mean(iris$Sepal.Length), sd = sd(iris$Sepal.Length)),
      add=T, col="red")
hist(iris$Sepal.Width, prob=T)
curve(dnorm(x, mean = mean(iris$Sepal.Width), sd = sd(iris$Sepal.Width)),
      add=T, col="red")
hist(iris$Petal.Length, prob=T)
curve(dnorm(x, mean = mean(iris$Petal.Length), sd = sd(iris$Petal.Length)),
      add=T, col="red")
hist(iris$Petal.Width, prob=T)
curve(dnorm(x, mean = mean(iris$Petal.Width), sd = sd(iris$Petal.Width)),
      add=T, col="red")
par(mfrow = c(1,1))

### QQ-plot per variable
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

### QQ-plot per specie for sepal length variable
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"],
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"],
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "virginica"],
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])
par(mfrow = c(1,1))
dev.off()
### try to do a for loop for this

# Tem outliers?
par(mfrow = c(2,2))
beeswarm(iris$Sepal.Length)
beeswarm(iris$Sepal.Width)
beeswarm(iris$Petal.Length)
beeswarm(iris$Petal.Width)
par(mfrow = c(1,1))


plot(iris$Sepal.Length)
plot(iris$Sepal.Width)
plot(iris$Petal.Length)
plot(iris$Petal.Width)

dotchart(iris$Sepal.Length)
dotchart(iris$Sepal.Width)
dotchart(iris$Petal.Length)
dotchart(iris$Petal.Width)

stripchart(iris$Sepal.Length,method="stack")
stripchart(iris$Sepal.Width,method="stack") ## seems like it has outliers
stripchart(iris$Petal.Length,method="stack")
stripchart(iris$Petal.Width,method="stack")

# Tem distribuicao normal?
## sepal's measurements are closer to be normally distributed than petal's

# As variaveis estao correlacionadas?

cor(new_iris)

pairs(new_iris)

pairs(new_iris, diag.panel = panel.hist)

# Agora plotamos com indices de correlacao e histogramas
pairs(new_iris, diag.panel=panel.hist, lower.panel=panel.cor)

# Incluimos tambem a linha de suavizacao
pairs(new_iris, panel=panel.smooth, diag.panel=panel.hist, lower.panel=panel.cor)

### sepal length seems to be strongly correlated with petal length and petal width
### petal length and petal width seems strongly correlated 

# Existe diferenca das medidas entre as tres especies? Compare as medias,
## mean of all measurements of variables
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)
mean(iris$Petal.Length)
mean(iris$Petal.Width)

### with apply function!! much easier
apply(new_iris, 2, mean)

### variance
apply(new_iris, 2, var) ### petal length has a big variance

### standand deviation
sd01 <- apply(new_iris, 2, sd)
sd01

### calculating sepal length mean per specie
aggregate(Sepal.Length ~ Species, data = iris, mean)

### calculating sepal width mean per specie
aggregate(Sepal.Width ~ Species, data = iris, mean)

### calculating petal length mean per specie
aggregate(Petal.Length ~ Species, data = iris, mean)

### calculating petal width mean per specie
aggregate(Petal.Width ~ Species, data = iris, mean)

### count NAs
apply(iris, 2, function(x) sum(is.na(x))) ### no NA

# plote graficos
par(mfrow = c(2,2))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width) ## seems like it has outliers
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)
par(mfrow = c(1,1))

### with for loop
all_bp <- for (i in new_iris) {
  boxplot(i ~ iris$Species, ylab = colnames(i))
}

### identifying outliers in general, without considering it for each species in 
### a certain variable
my_boxplot <- boxplot(iris$Sepal.Width, plot = FALSE)
my_boxplot
# the object is a list and the outliers are stored in the $out element of the list
outliers <- my_boxplot$out
# what is the position of the outliers
which(iris$Sepal.Width %in% outliers)
# let's use the position to index the object
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

### identifying outliers per species per variable
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data = iris, plot = FALSE)
my_boxplot2
# the object is a list and the outliers are stored in the $out element of the list
outliers2 <- my_boxplot2$out
# in this case, we only want the outliers of the setosa species
# let's use the position to index the object
iris[iris$Sepal.Width %in% outliers2 &
       iris$Species == "setosa",
     c("Sepal.Width", "Species")]
