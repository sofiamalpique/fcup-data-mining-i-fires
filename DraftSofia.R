library(tidyverse)
library(caret)
library(naivebayes)

set.seed(123)
treino <- createDataPartition(y=final$intentional_cause, p=0.7, list=FALSE)

fire_treino <- final %>% slice(treino)
fire_teste <- final %>% slice(-treino)

# Vamos guardar o intentional cause do nosso teste:
fire_teste_IC <- fire_teste$intentional_cause

# E agora removê-lo do teste para depois compararmos
fire_teste <- fire_teste %>% select(-intentional_cause)



# K Nearest Neighbor 

# KNN com k = 3:

# Criar o modelo
knn3.model <- knn3(intentional_cause ~ ., data = fire_treino, k=3)
# Fazer previsão
knn3.preds <- predict(knn3.model,fire_teste,type = "class")

#Checar a matriz para saber accuracy
knn3.confM <- confusionMatrix(fire_teste_IC,knn3.preds)
knn3.confM # (0.6692)


# KNN com k = 5:

# Criar o modelo
knn5.model <- knn3(intentional_cause ~ ., data = fire_treino, k=5)
# Fazer previsão
knn5.preds <- predict(knn5.model,fire_teste,type = "class")

#Checar a matriz para saber accuracy
knn5.confM <- confusionMatrix(fire_teste_IC,knn5.preds)
knn5.confM # (0.6841)


# KNN com k = 7:

# Criar o modelo
knn7.model <- knn3(intentional_cause ~ ., data = fire_treino, k=7)
# Fazer previsão
knn7.preds <- predict(knn7.model,fire_teste,type = "class")

#Checar a matriz para saber accuracy
knn7.confM <- confusionMatrix(fire_teste_IC,knn7.preds)
knn7.confM # (0.7019)



# Naive Bayes

nbFire.model <- naive_bayes(intentional_cause ~., data = fire_treino)

nbFire.preds <- predict(nbFire.model, fire_teste, type = "class")






