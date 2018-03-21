##########################################
##########################################
####                                  ####
#### Curso de Big Data e Data Science ####
####  Aula 1 - Módulo de Estatística  ####
####             Turma 10             ####
####                                  ####
##########################################
##########################################


## Dados do R
ChickWeight     # Sou um produtor e desejo escolher a melhor forma de alimentação
esoph           # Campanha para redução do abuso de alcool e tabaco, causa cancer de esofago
ToothGrowth     # A vitamina C tem incluência no crescimento de ossos de mamíferos?
InsectSprays    # Gerenciadora de condomínios deseja acabar com pragas. Qual melhor inseticida usar?

## Dados externos
pnad
alturas

##### Aula 1 - Análise descritiva de dados #####

# O objeto principal da estatística são os dados
# Todos os problemas estatísticos estão relacionados com a COLETA, DESCRIÇÃO e ANÁLISE DE DADOS.
# Nesse primeiro tópico falaremos da DESCRIÇÃO.

#### Parte 1 ####
setwd('material aula')
df<-read.csv2('./material aula/aula 1/altura_e_peso.csv')

# Visualizando o data.frame
head(df,3)
tail(df,5)
View(df)

# acessando uma coluna
altura<-df$altura

## Mínimo e máximo
min(altura)
max(altura)
range(altura)

## tabela de frequência
intervalo<-cut(altura,breaks = seq(from = 140,to = 200,by = 5))
table(intervalo)
tab<-data.frame(table(intervalo))
names(tab)<-c('intervalo','frequência')
tab

## Histogramas
# Definidas as frequências 
bplot<-barplot(height = tab$frequência,space = F,main='Histograma das alturas')
axis(side = 1,at = bplot
     ,labels = tab$intervalo
     ,cex.axis=0.8)
# Alternativamente
hist(altura)
hist(altura,breaks = seq(from=140,to=200,by=5))
hist(altura,breaks = seq(from=140,to=200,by=2))
hist(altura,breaks = seq(from=140,to=200,by=1))

# O histograma pode ser apresentado com a frequência relativa, ao invés da frequência absoluta.
# Nesse caso a área de cada de cada barra representa a frequência relativa daquele intervalo.
# Isto é feito para que a área total do histograma seja igual a 1.
hist(altura,prob=T)

## Estatísticas de síntese (centralidade e dispersão)
# média
mean(altura)
# mediana
median(altura)
# quantis
quantile(altura)                           # quartis
quantile(altura,probs = seq(0,1,by=0.1))   # decis
quantile(altura,probs = seq(0,1,by=0.01))  # percentis
# IQR (interquartile range)
IQR(altura)
as.numeric(quantile(altura)[4]-quantile(altura)[2])
# Boxplot
boxplot(altura,main = 'Boxplot das alturas')
points(mean(altura),col = 2,pch = 20)          # introduzindo a média
# desvio-padrão (Standard Deviation)
sd(altura)

##### Parte 2 
# Vamos agora estudar o comportamento conjunto das duas variáveis
plot(df$peso,df$altura,xlab='peso',ylab='altura')

# Que bagunça de pontos! Como fazer para melhor visualizar e descrever essa informação?

# Histogramas marginais

scatterhist = function(x, y, xlab="", ylab=""){
  def.par <- par(no.readonly = TRUE) # save initial parameters for margin
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  par(oma=c(3,3,0,0))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x,y)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
  par(def.par) # reset the original parameters for margin
}
# autoria: https://www.r-bloggers.com/example-8-41-scatterplot-with-marginal-histograms/
scatterhist(df$peso,df$altura,xlab='peso',ylab='altura')


# install.packages('plot3D')
library(plot3D)
##  Crio os cortes:
x_c <- cut(df$peso, seq(50,105,by=5))
y_c <- cut(df$altura, seq(140,200,by=5))
##  Tabela de frequências:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
hist3D(z=z, border="black",xlab='peso',ylab='altura',zlab='freq')


# Box-plot
range(df$peso)
df$intervalo_peso<-cut(df$peso,seq(50,105,by=5))
box<-boxplot(altura ~ intervalo_peso,data = df,
             ylab='altura',
             xlab='peso',
             main='Boxplot por classe')

# Note que existe uma relação crescente entre altura e peso

# média de altura em cada classe
media_classe<-tapply(X = df$altura,
                     INDEX = df$intervalo_peso,
                     FUN = mean)
# Colocando cada média no gráfico
points(media_classe,col=2,pch=20)

## Medidas de variação conjunta
# Covariância
cov(df$altura,df$peso)
# Correlação
with(df,cor(altura,peso))

# Medida invariante a mudança na escala
with(df,cov(altura/100,peso))
with(df,cor(altura,peso))
with(df,cor(altura/100,peso))

# Matriz de variância e covariância e de correlação
cov(df[,1:2])
cor(df[,1:2])

# corrplot
# install.packages('corrplot')
library(corrplot)
corrplot(cor(df[,1:2]))
# Exemplo com outra base de dados
corrplot(cor(mtcars))

# Exercício ChickWeight
# Apresentação da base de dados.
head(ChickWeight)
tail(ChickWeight)
View(ChickWeight)
# Esse plot mostra a evolução do peso de cada frango no tempo
coplot(weight ~ Time | Chick, data = ChickWeight,
       type = "b", show.given = FALSE)

