
### Dataset Iris - Machine Learning

iris <- data.frame(iris)
head(iris)
tail(iris)
summary(iris)

### Amostragem

set.seed(2)
amostra <- sample(1:nrow(iris), nrow(iris)*0.8, replace = FALSE)
head(iris[amostra,])

treino<-iris[amostra,] #conjunto treinamento
treino
summary(treino)

teste <- iris[- amostra,] #conjunto teste
teste
summary(teste)

###### Outra possibilidade (treinamento e teste)

library(caTools)
set.seed(1)
divisao <- sample.split(iris$Species, SplitRatio = 0.8)
divisao
treinamento <- subset(iris, divisao == TRUE)
treinamento
teste <- subset(iris, divisao == FALSE)
teste

### Árvores de Decisão

library(rpart)
classificador1 <- rpart(formula =  Species ~ ., data = treino)
print(classificador1)
plot(classificador1)
text(classificador1)

### Outra possibilidade

library(partykit)
plot(as.party(classificador1))

## Melhorando o gráfico

library(rpart.plot)
rpart.plot(classificador1)

#### Matriz de confusão

previsoes = predict(classificador1, newdata = teste[-5], type = 'class')
matriz_confusao = table(teste[, 5], previsoes)
print(matriz_confusao)

library(caret)
confusionMatrix(matriz_confusao)

### Random Forest

library(randomForest)
classificador2 <- randomForest(Species ~ .,treino,ntree=500)
summary(classificador2)
predicted = predict(classificador2,teste)
predicted
plot(classificador2)

### Cross validation (k-fold)

library(ipred)
set.seed(12)
errorCtree <- numeric(10) # k = 10
for (i in 1:10) errorCtree[i] <- errorest(Species ~ ., data = iris, model = ctree)$error
errorCtree
summary(errorCtree)



