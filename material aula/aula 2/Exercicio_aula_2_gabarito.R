#######################################################
########                                     ##########
########  Curso de Big Data e Data Science   ##########
########        módulo de Estatística        ##########
########     Gabarito exercícios aula 2      ##########
########                                     ##########
#######################################################

## Exercício 1 - Utilizando a base de dados da PNAD

# item 1
pnad2015<-read.csv2('./material aula/aula 2/pnad2015_sub.csv')
with(pnad2015,tapply(renda_total,sexo,mean,na.rm=T))

# item 2
x1h<-pnad2015$renda_total[pnad2015$sexo=='homem']
t.test(x1h,mu = 1500,conf.level = 0.95,alternative = 'less')

# item 3
x1m<-pnad2015$renda_total[pnad2015$sexo!='homem']
t.test(x1h,x1m)


## Exercício 2 - Utilizando a base Chickweight

# primeiramente recorto os vetores que serão utilizados
z1<-ChickWeight[ChickWeight$Diet==1 & ChickWeight$Time==21,]$weight
z2<-ChickWeight[ChickWeight$Diet==2 & ChickWeight$Time==21,]$weight
z3<-ChickWeight[ChickWeight$Diet==3 & ChickWeight$Time==21,]$weight
z4<-ChickWeight[ChickWeight$Diet==4 & ChickWeight$Time==21,]$weight

# item 1
t.test(z3,z4)

# item 2
t.test(z1,z2)

# item 3
t.test(z3,z2)

# item 4
# não é possível concluir isso, observe que se testarmos 3 contra 1 teremos que elas são
# em média diferentes com nível de confiância superior a 99%.
t.test(z3,z1)

