#### Exercícios - ChickWeight
# Com o que aprendemos até aqui vamos verificar o que podemos fazer.

# Suponha que você vou contratado para prestar consultoria a um produtor de frangos que deseja conhecer
# a melhor dieta para engordar os seus animais.
# Para ter uma ideia atualmente com melhora genética o abate costuma ocorrer entre 28 e 42 dias,
# quando o peso vivo do animal for de aproximadamente 1 kg. Utilizaremos uma base disponível no R
# que contém tais dados.


### 1 - Quantos frangos são monitorados?

# O primeiro passo é verificar os rótulos diferentes dados a cada animal;
unique(ChickWeight$Chick)
# uma maneira alternativa de fazer isso é utilizar a função with para não precisar
# utilizar o simbolo $ para acessar a variável
with(ChickWeight,unique(Chick))
# Em seguida contar esses rótulos
length(unique(ChickWeight$Chick))



### 2 - Quantos são monitorados em cada grupo de ração?

# Uma maneira intuitiva é construir um data.frame para cada tipo de dieta
# e contar quantos frangos tem em cada.
df1<-ChickWeight[ChickWeight$Diet==1,]
df2<-ChickWeight[ChickWeight$Diet==2,]
df3<-ChickWeight[ChickWeight$Diet==3,]
df4<-ChickWeight[ChickWeight$Diet==4,]
length(unique(df1$Chick))
length(unique(df2$Chick))
length(unique(df3$Chick))
length(unique(df4$Chick))

# Uma outra maneira, é utilizar a função tapply, apresentada no módulo de introdução ao R.
with(ChickWeight,tapply(Chick,Diet,function(x) length(unique(x))))



### 3 - Qual a média de peso em cada dia?
# Aqui utilizo a função tapply a
tapply(ChickWeight$weight,ChickWeight$Time,mean)
# Novamente posso utilizar a função with, para evitar escrever sempre o nome da base
with(ChickWeight,tapply(weight,Time,mean))
# Abaixo faço um gráfico para plotar essa informação.
df<-data.frame(media_dia = with(ChickWeight,tapply(weight,Time,mean)))   # primeiro transformo em data.frame para manipular melhor
plot(x = as.numeric(row.names(df)),           # variável no eixo x
     y = df$media_dia, type='l',              # variável no eixo y
     main = 'Média de peso em cada dia',      # Título do gráfico
     ylab='peso médio',                       # nome do eixo y
     xlab='dias')                             # nome do eixo x


### 4 - Qual a média de peso em cada dia, dado o tipo de ração

# Você pode utilizar os data.frames criados no exercício 2:
with(df1,tapply(weight,Time,mean))
with(df2,tapply(weight,Time,mean))
with(df3,tapply(weight,Time,mean))
with(df4,tapply(weight,Time,mean))

# Uma outra maneira é utilizar o sapply juntamente com o tapply
# vamos por partes:
# a - Com a função tapply você consegue calcular o vetor com a média em cada dia,
# porém deve especificar o tipo de dieta. Abaixo se Diet = 1
with(ChickWeight[ChickWeight$Diet==1,],tapply(weight,Time,mean))

# b - O próximo passo é definir o passo a como função que depende do tipo da dieta e,
# assim utilizar o sapply.
sapply(1:4,
       function(x) with(ChickWeight[ChickWeight$Diet==x,],tapply(weight,Time,mean)))

# c - Para generalizar eu utilizo uma função que encontra os tipos diferentes de dietas 
sapply(as.numeric(unique(ChickWeight$Diet)),
       function(x) with(ChickWeight[ChickWeight$Diet==x,],tapply(weight,Time,mean)))


### 5 -  Gráfico

# Utilizando os data.frame criados
media_dia1<-data.frame(with(df1,tapply(weight,Time,mean)))
media_dia2<-data.frame(with(df2,tapply(weight,Time,mean)))
media_dia3<-data.frame(with(df3,tapply(weight,Time,mean)))
media_dia4<-data.frame(with(df4,tapply(weight,Time,mean)))

plot(row.names(media_dia1),media_dia1[,1],type = 'l',
     ylim=range(ChickWeight$weight),
     main='Comparação de Dieta no peso médio',
     ylab='peso médio',
     xlab='dias')
lines(row.names(media_dia1),media_dia2[,1],col=2)
lines(row.names(media_dia1),media_dia3[,1],col=3)
lines(row.names(media_dia1),media_dia4[,1],col=4)
legend(0,350,
       legend = c('dieta 1','dieta 2','dieta 3','dieta 4'), # o que está escrito
       col=1:4, # cores da legenda
       lty=1)   # símbolo apropriado, no caso lty=1 representa linha


# Utilizando a função sapply e tapply
media_dia_diet<-sapply(as.numeric(unique(ChickWeight$Diet)),function(x) with(ChickWeight[ChickWeight$Diet==x,],tapply(weight,Time,mean)))
plot(media_dia_diet[,1],type='l',
     ylim=range(ChickWeight$weight),
     main='Comparação de Dieta no peso médio',
     ylab='peso médio',
     xlab='dias')
lines(media_dia_diet[,2],col=2)
lines(media_dia_diet[,3],col=3)
lines(media_dia_diet[,4],col=4)
legend(0,350,
       legend = c('dieta 1','dieta 2','dieta 3','dieta 4'), # o que está escrito
       col=1:4, # cores da legenda
       lty=1)   # símbolo apropriado, no caso lty=1 representa linha

### 6 - Qual a correlação existente entre os dias e a média de peso para cada dieta?
cor(media_dia1[,1],as.numeric(rownames(media_dia1)))
cor(media_dia2[,1],as.numeric(rownames(media_dia2)))
cor(media_dia3[,1],as.numeric(rownames(media_dia3)))
cor(media_dia4[,1],as.numeric(rownames(media_dia4)))

# ou utilizando sapply
sapply(1:4,function(x) cor(as.numeric(rownames(media_dia_diet)),media_dia_diet[,x]))

### 7 - Compare as medidas de centralidade e dispersão usando um boxplot para cada tipo de dieta no último dia.
df<-ChickWeight[ChickWeight$Time==21,]
box<-boxplot(weight ~ Diet,df)
# introduzindo as médias
points(at=box,
       as.numeric(cbind(media_dia1,media_dia2,media_dia3,media_dia4)[12,]),
       col=2,
       pch=20)


### 8 - Compare as medidas de centralidade e dispersão usando um boxplot para cada tipo de dieta no último dia.

# Essa análise descritiva traz evidências de que a dieta 3 é mais eficiente do que as demais.
# No entanto isso é verdade para o caso específico dos frangos utilizados na pesquisa
# Como podemos extrapolar a informação obtida no experimento em questão para toda a população de frangos?
# Isso é um trabalho para a inferência estatísica, mas antes precisamos introduzir alguns conceitos de probabilidade.



