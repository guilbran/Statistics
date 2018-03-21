##########################################
##########################################
####                                  ####
#### Curso de Big Data e Data Science ####
####  Aula 2 - Módulo de Estatística  ####
####             Turma 10             ####
####                                  ####
##########################################
##########################################

### Distribuição de probabilidade

# Distribuição de probabilidade do lançamento de duas moedas
b<-barplot(c(0.25,0.5,0.25),space=F,main = "Distribuição do lançamento de duas moedas")
axis(side=1,at=b,labels = c(0,1,2))

# Representando a distribuição de uma binomial
n<-10       # quantas moedas está lançando?
p<-0.5     # qual a probabilidade de dar cara (1)
x<-0:n
dbinom(x,n,p)
b<-barplot(dbinom(x,n,p),space=F,main=paste("Dist. Binomial: n =",n,"p =",p))
axis(side=1,at=b,labels = x)

# Aproximação para a Normal(np,np(1-p))
lines(x+0.5,dnorm(x,mean = n*p,sd = sqrt(n*p*(1-p))),col=2)

# Quanto mais o n cresce mais fina fica a distribuição, cada vez mais difícil de observar valores extremos.
# A intuição é que quando mais vezes for lançada a moeda mais raro é o evento de todas as observações serem Cara.

### Inferência (estimação pontual)

# Simule 1 lançamento de 10 moedas
set.seed(123)
n<-10
p<-0.5
rbinom(1,n,p)


# Simule 100, 1000 e 10000 lançamentos de 10 moedas
set.seed(123)
n<-10
p<-0.5
x<-0:n
result<-rbinom(100,n,p)
result

his<-hist(result+1,breaks=x+1,col='red',xaxt='n',xlab='',main='Histograma e Distribuição')
# lembre-se de alterar para a probabilidade

# plotando juntamente a distribuição
dbinom(x,n,p)
b<-barplot(dbinom(x,n,p),add=T,col=rgb(0,0,1,0.2),space = F,names.arg = x)



# Teste de hipótese

# Joaquim lança 100 vezes a moeda e não tem moeda viciadas
set.seed(321)
n<-100
p<-0.5
s<-rbinom(1,size = n,prob = p)
s
# Como 59 é maior que 50 ele é tido como culpado, mesmo sendo inocente
# Erro do tipo 1

# Joaquim lança 100 vezes a moeda e não tem moeda viciadas
set.seed(321)
n<-100
p<-0.55
s<-rbinom(1,size = n,prob = p)
s
# Como 46 é menor que 50 ele é tido como inocente, mesmo sendo culpado
# Erro do tipo 2

# Simule 100000 lançamentos de 100 moedas
set.seed(123)
n<-100
p<-0.5
x<-0:n
result<-rbinom(100000,n,p)
his<-hist(result+1,breaks=x+1,col='red',xaxt='n',xlab='',main='Histograma e Distribuição',prob=T,xlim=c(20,80))
# plotando juntamente a distribuição
dbinom(x,n,p)
b<-barplot(dbinom(x,n,p),add=T,col=rgb(0,0,1,0.2),space = F,names.arg = x,xlim=c(20,80))

# O histograma quando o número de repetições vai pro infinito se aproxima da distribuição binomial,
# que por sua vez se aproxima da Normal, pois temos muitos lançamentos (n grande)
lines(x+0.5,dnorm(x,mean = n*p,sd = sqrt(n*p*(1-p))),col='green')

# Assim podemos utilizar a Normal para calcular a probabilidade do erro do tipo 1
1-pnorm(50,n*p,sqrt(n*p*(1-p)))

# Se quisermos definir a probabilidade de cometer o erro do tipo 1 como sendo 0.05, qual deve ser o critério?
qnorm(0.95,n*p,sqrt(n*p*(1-p)))


### Teste z
# Novo lançamento de moeda
set.seed(1)
n<-100
p<-0.5
s<-rbinom(1,size = n,prob = p)
s

# valor p
1-pnorm(s,n*p,sqrt(n*p*(1-p)))
# Como o valor p é maior que o nível de significância, não rejeito H0


### Teste t
set.seed(1)
n<-100
x<-rnorm(n,5,2)
t.test(x,mu = 5)

# Exercício PNAD e ChickenWeight

