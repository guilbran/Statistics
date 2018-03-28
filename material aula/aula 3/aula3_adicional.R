##########################################
##########################################
####                                  ####
#### Curso de Big Data e Data Science ####
#### Aula 3.2 - Módulo de Estatística ####
####             Turma 10             ####
####                                  ####
##########################################
##########################################

## Visualização gráfica

library(rgl)
survey <- readRDS('./material aula/aula 3/survey.rds')

reg <- lm(Height ~ Wr.Hnd + NW.Hnd,data = survey)

Wr.Hnd <- seq(range(survey$Wr.Hnd,na.rm = T)[1],range(survey$Wr.Hnd,na.rm = T)[2],10)
NW.Hnd <- seq(range(survey$NW.Hnd,na.rm = T)[1],range(survey$NW.Hnd,na.rm = T)[2],10)
Height <- outer(Wr.Hnd,NW.Hnd,function(Wr.Hnd,NW.Hnd) predict(reg,newdata = data.frame(Wr.Hnd,NW.Hnd)))

with(survey,plot3d(Wr.Hnd,NW.Hnd,Height, col="blue", size=1, type="s", main="3D Linear Model Fit"))
surface3d(Wr.Hnd,NW.Hnd,Height,alpha=0.2, back="line",color = "blue")


# Regressão categórica Sexo

reg <- lm(Height ~ Wr.Hnd + Sex,data = survey)

Sex <- unique(survey$Sex)
Wr.Hnd <- seq(range(survey$Wr.Hnd,na.rm = T)[1],range(survey$Wr.Hnd,na.rm = T)[2],10)
NW.Hnd <- seq(range(survey$NW.Hnd,na.rm = T)[1],range(survey$NW.Hnd,na.rm = T)[2],10)
Height <- outer(Wr.Hnd,Sex,function(Wr.Hnd,Sex) predict(reg,newdata = data.frame(Wr.Hnd,Sex)))

with(survey,plot3d(Wr.Hnd,Sex,Height, col="blue", size=1, type="s", main="3D Linear Model Fit"))




## Previsão dentro da amostra
# Função predict

reg <- lm(Height ~ Wr.Hnd + Sex,data=survey)

# função predict retorna a previsão pelo modelo
predict(reg)

# O erro de previsão é dado pela diferença com os valores observados (é chamado de resíduo)
erro <- survey$Height - predict(reg)
erro <- reg$model[,1] - predict(reg)

plot(erro)

# Erro quadrático médio (MSE)
mean(erro^2)
mean(reg$residuals^2)

# Raíz do erro quadrático médio (RMSE)
rmse <- sqrt(mean(reg$residuals^2))




## Previsão fora da amostra (Cross - Validation)

survey[is.na(survey$Height),]   # dados sem informação sobre altura

newdata <- survey[is.na(survey$Height),]   # dados sem informação sobre altura

predict(reg,newdata)

# O quão bom é esse modelo?

survey_complete <- reg$model # seleciono apenas os dados utilizados pelo modelo

dim(survey_complete)[1]

# Avaliação fora da amostra: seleciono uma parte da base para teste e outra para treino
# Por fatorização 207 é divisível por 9, então seleciono uma base de teste aleatória com 9 obs para teste
set.seed(1)
(indices_test <- sample(1:207,9))

survey_test <- survey_complete[indices_test,]
survey_train <- survey_complete[-indices_test,]

reg1 <- lm(Height ~ Wr.Hnd + Sex,data=survey_train)

# previsão fora da amostra
predict(reg1,survey_test)
# erro fora da amostra
erro <- survey_test$Height - predict(reg1,survey_test)

# Erro quadrático médio de previsão (MSFE)
mean(erro^2)

# Raíz do erro quadrático médio (RMSFE)
rmsfe <- sqrt(mean(erro^2))

# comparação
rmsfe
rmse


# Porém essa foi uma medida baseada em um possível sorteio
# k-fold faz o mesmo para todas as demais amostras
set.seed(222)
(indices_test <- sample(1:207,207))

(indices_test_matrix <- t(matrix(indices_test,nrow = 9)))

# Realizo o procedimento em um loop para todos os 23 grupos possíveis de teste
# Salvo o resultado em um vetor 

rmsfe_k <- {}
for(i in 1:dim(indices_test_matrix)[1]){
  indices_test_1 <- indices_test_matrix[i,]
  survey_test <- survey_complete[indices_test_1,]
  survey_train <- survey_complete[-indices_test_1,]
  reg1 <- lm(Height ~ Wr.Hnd + Sex,data=survey_train)
  predict(reg1,survey_test)
  erro <- survey_test$Height - predict(reg1,survey_test)
  rmsfe_k[i] <- sqrt(mean(erro^2))
}
rmsfe_k

# Calculo a média dessa medida de ajuste
rmsfe_k_fold <- mean(rmsfe_k)

# Comparação com o modelo
rmsfe_k_fold
rmse

