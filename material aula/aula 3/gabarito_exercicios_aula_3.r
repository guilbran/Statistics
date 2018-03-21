#######################################################
########                                     ##########
########  Curso de Big Data e Data Science   ##########
########        módulo de Estatística        ##########
########     Gabarito exercícios aula 3      ##########
########                                     ##########
#######################################################

###### Exercício 1 -------

reg <- lm(len ~ dose,ToothGrowth)

###### Exercício 2 -------

summary(reg)
# Como o p-valor do teste t é muito próximo de 0, o coeficiente é significativo

###### Exercício 3 -------

with(ToothGrowth,plot(dose,len))
abline(reg, col = 2)

###### Exercício 4 -------

reg2 <- lm(len ~ dose + supp,ToothGrowth)

###### Exercício 5 -------

summary(reg2)
