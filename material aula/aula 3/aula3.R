##########################################
##########################################
####                                  ####
#### Curso de Big Data e Data Science ####
####  Aula 3 - Módulo de Estatística  ####
####             Turma 10             ####
####                                  ####
##########################################
##########################################


# Exercícios de recapitulação
# Utilizando a base de dados survey
# item 1 - Carregando a base de dados do pacote MASS
survey <- readRDS('./material aula/aula 3/survey.rds')

# item 2 - Apresente a difereça existente entre a média de altura de homens e mulheres 
boxplot(Height ~ Sex,data = survey)
media<-tapply(survey$Height, survey$Sex, mean, na.rm=T)
points(media,pch=20,col=2)

# item 3 - Faça um teste de hipótese para verificar se a média é diferente entre os sexos
t.test(survey$Height[survey$Sex == 'Female'],survey$Height[survey$Sex == 'Male'])

# Note que algumas observações para altura são NA (not available)
# Qual seria a melhor previsão que podemos fazer para essas observações?

# Temos algumas alternativas:
# 1 - utilizar alguma medida de centralidade geral: média ou mediana
# 2 - utilizar alguma medida de centralidade entre grupos (média de homens para homens e média de mulher para mulheres)

# A primeira não parece ser muito interessante já que verificamos que as médias são diferentes entre esses grupos.

# Uma outra maneira de descrever essa relação é por meio de uma função afim.
plot(Height ~ Wr.Hnd,data=survey)
reg<-lm(Height ~ Wr.Hnd,data=survey)
abline(reg,col=2)

# Mas como chego nesses valores? Como interpreto esses resultados?
# Por meio do MQO (ver apostila e slides)


### Qualidade do ajuste via ANOVA
SQR<-sum(reg$residuals^2)
SQT<-(sum(!is.na(survey$Height))-1)*var(survey$Height,na.rm = T)
1-SQR/SQT
# via correlação
with(survey,cor(Height,Wr.Hnd,use = 'complete.obs'))^2


### Multiplos regressores
# Wr.Hnd and Sex
reg<-lm(Height ~ Wr.Hnd + Sex,data=survey)
SQR<-sum(reg$residuals^2)
SQT<-sum((survey$Height[!is.na(survey$Height)]
          -mean(survey$Height,na.rm = T))^2)
1-SQR/SQT
# plot
with(survey,plot(Wr.Hnd[survey$Sex=='Male'],
                 Height[survey$Sex=='Male'],
                 xlab = 'Wr.Hnd',
                 ylab = "Height",
                 xlim = range(Wr.Hnd,na.rm = T),
                 ylim = range(Height,na.rm=T),col=4))
with(survey,points(Wr.Hnd[survey$Sex=='Female'],
                   Height[survey$Sex=='Female'],
                   xlab = 'Wr.Hnd',
                   ylab = "Height",
                   xlim = range(Wr.Hnd,na.rm = T),
                   ylim = range(Height,na.rm=T),col=2))
abline(c(reg$coefficients[1]+reg$coefficients[3],reg$coefficients[2]),col=4)
abline(c(reg$coefficients[1],reg$coefficients[2]),col=2)
legend(x = 13,y = 200,
       legend = c('Female','Male'),
       pch = c(1,1),
       col = c(2,4))

### Teste t
summary(reg)

# Previsão:
# Utilizando como regressores Wr.Hnd e Sex,
# qual a melhor previsão para os dados que estão missings (NA).
predict(reg,newdata = survey[is.na(survey$Height),])

### Logit
# exemplo do tastesgreat - 1 regressor
# Carregando a base de dados tastesgreat do pacote UsingR
tastesgreat <- readRDS('./material aula/aula 3/tastesgreat.rds')
plot(enjoyed ~ age,data=tastesgreat)
reg<-glm(enjoyed ~ age, data=tastesgreat,family = binomial)
curve(exp(reg$coefficients[1]+reg$coefficients[2]*x)/
        (1+exp(reg$coefficients[1]+reg$coefficients[2]*x)),30,60,add = T,col=2)

# A probabilidade de gostar do produto dado a idade
predict(reg,type = 'response')

# Como interpretar os coeficiente?
summary(reg)
exp(reg$coefficients[2])

# exemplo do tastesgreat - 2 regressor (idade e gênero)
reg<-glm(enjoyed ~ age + gender, data=tastesgreat,family = binomial)
predict(reg,type = 'response')

exp(reg$coefficients[2])
exp(reg$coefficients[3])


# Segundo esse modelo responda:
# Qual a probabilidade de uma mulher de 34 anos e um homem de 25,2 anos gostarem do produto?
newdata <- data.frame(gender = c('Female','Male'),age = c(34,25.2))
predict(reg,newdata,type = 'response')


# Exercício Titanic
df<-read.csv2('./material aula/aula 3/train.csv',sep = ',')
df$idade<-as.numeric(df$Age)
df$idade[df$Age=='']<-NA
df$taxa<-as.numeric(df$Fare)
df$taxa[df$Fare=='']<-NA
df$Sex<-(df$Sex=='male')*1
df$Sex[df$Sex=='']<-NA
reg<-glm(Survived ~ Sex + taxa + idade,data=df,family = binomial)
summary(reg)


