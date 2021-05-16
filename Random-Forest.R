#Remove objetos da memória do R
rm(list=ls(all=TRUE))

#Instala bibliotecas
install.packages('mlbench') #biblioteca com conjunto de dados Pima Indians Diabetes
install.packages('caret',dependencies = TRUE) #biblioteca para trabalhar com machine learning
install.packages('randomForest')

#Carrega as bibliotecas
library(mlbench)
library(caret)
library(randomForest)

#Carrega o conjunto de dados PimaIndiansDiabetes na memoria do R
data(PimaIndiansDiabetes)

#Armazena o conjunto de dados PimaIndiansDiabetes em um data frame com o nome dados
dataframe <- PimaIndiansDiabetes

#visualiza as estatisticas descritivas das variaveis do conjunto de dados
summary(dataframe)

#Separa conjunto de dados para treino e teste no método de hold-out
conjunto <- createDataPartition(dataframe$diabetes, #Variavel resposta do conjunto de dados
                                p = 0.8, #Definir percentual para treino em 80%
                                list = F #Manter lista
)

#
base_treino <- dataframe[conjunto,]
base_teste <- dataframe[-conjunto,]

#Planta a seamente
set.seed(1)

#Treina o modelo utilizando o algoritmo do Random Forest
random_forest <- train(diabetes ~.,
                       data = base_treino,
                       ntrees = 100,
                       method = 'rf')

#Visualiza as variais da predicao
varImpPlot(random_forest$finalModel)

#Realiza predicao nos dados separados para teste
predicoe <- predict(random_forest, newdata = base_teste)

#Imprime a matrix confusão do modelo
confusionMatrix(predicoes,
                base_teste$diabetes,
                positive = 'pos')
