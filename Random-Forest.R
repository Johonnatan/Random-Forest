rm(list=ls(all=TRUE)) #Remove objetos da memória do R

#Instala bibliotecas necessarias
install.packages('mlbench') #biblioteca mlbench disponibiliza varios conjunto de dados, incluindo o Pima Indians Diabetes
install.packages('caret',dependencies = TRUE) #biblioteca que tem enorme quantidade de ferramentas e algoritmos para trabalhar com machine learning
install.packages('rpart') #rpart traz o algoritmo de arvore de decisao
install.packages('rpart.plot') #o rpart.plot serve para visualizar a arvore de decisao gerado pelo rpart
install.packages('randomForest')

#Carrega as bibliotecas
library(mlbench)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

#Carrega o conjunto de dados na memoria do R
data(PimaIndiansDiabetes)

#Armazena o conjunto de dados PimaIndiansDiabetes em um data frame com o nome dados
dados <- PimaIndiansDiabetes

#visualiza a media (mean) e outras estatisticas descritivas das variaveis
summary(dados)

#Separa conjunto de dados para treino e teste para hold-out
index <- createDataPartition(dados$diabetes, #Variavel resposta
                             p = 0.8, #Definir percentual para treino
                             list = F #Manter list = F
)
treino <- dados[index,]
teste <- dados[-index,]

####--- Treina random forest utilizando 100 arvores de decisoes
set.seed(1)

random_forest <- train(diabetes ~.,
                       data = treino,
                       ntrees = 100,
                       method = 'rf')

#Visualiza as variais mais importantes para predicao, ou seja, aquelas que apresentaram maior ganha de informacao
varImpPlot(random_forest$finalModel)

#A partir do algoritmo treinado, faz predicao nos dados separados para teste
predicoes_random_forest <- predict(random_forest, newdata = teste)

confusionMatrix(predicoes_random_forest,
                teste$diabetes,
                positive = 'pos')
