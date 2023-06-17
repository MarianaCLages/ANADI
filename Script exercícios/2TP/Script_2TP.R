#Trabalho Prático 2

set.seed(42)

# Regressão

# Packages necessários para a realização deste trabalho
library(caret)
library(ggplot2)
library(corrplot)
library(dplyr)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(class)

# 1.
# Definição do caminho em que se encontra o script
#setwd("C:/Users/franc/Documents/Repositórios/anadi23/Script exercícios/2TP")
#setwd("/Users/fredol/Documents/isep/anadi23/Script exercícios/2TP")
#setwd("C:/Users/maria/Desktop/ISEP/3ºano/2ºsemestre/ANADI/anadi23/Script exercícios/2TP")
setwd("C:/Users/MiguelJordão(1201487/Desktop/ANADI/anadi23/Script exercícios/2TP")

# Importação dos dados
dataset <- read.csv("ciclismo.csv", header = TRUE, stringsAsFactors = FALSE)
dataset <- dataset[, !colnames(dataset) %in% "ID"]

# Verificação da dimensão do datasets
dimensao_dataset <- dim(dataset)

# Sumário estatístico dos dados
summary(dataset)


# 2.
# Conversão da data de nascimento para um tipo de Data no R
dataset$dob <- as.Date(dataset$dob)

# Cálculo da idade. Uso de 365.25 devido aos anos bissextos (a cada 4 anos)
dataset$Age <- floor(as.integer(Sys.Date() - dataset$dob) / 365.25)
age <- dataset$Age


# 3. 
# Seleção de atributos numéricos
numerical_vars <- sapply(dataset, is.numeric)
  
# Boxplot para altitude, vo2, hr e age
boxplot(dataset[, numerical_vars], col = "lightblue", main = "O2 Related Attributes")

# Scatter Plots para os diferentes atributos e como se relacionam
ggplot(dataset, aes(x = altitude_results, y = vo2_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Altitude", y = "VO2") +
  ggtitle("Altitude vs. VO2 Plot")

ggplot(dataset, aes(x = altitude_results, y = hr_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Altitude", y = "Heartrate") +
  ggtitle("Altitude vs. Heartrate")

ggplot(dataset, aes(x = vo2_results, y = hr_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "VO2", y = "Heartrate") +
  ggtitle("VO2 vs. Heartrate ")


# 4.
# A) Não aplicável, não há valores NA em qualquer coluna. clean_dataset e dataset são iguais

# Identificação dos valores em falta
missing_values <- is.na(dataset)

# Contar o número de valores em falta para cada coluna
missing_count <- colSums(missing_values)

# Imprimir a contagem de valores em falta
print(missing_count)

# Remoção de linhas com valores em falta
clean_dataset <- na.omit(dataset)

# Substituição do dataset atual pelo limpo, algo que não é necessário
#dataset <- clean_dataset

# Remoção do clean_dataset
rm("clean_dataset")


# B) Analyzing the boxplot, it is possible to verify outliers on the altitude, vo2, hr


# C)
# Manter as colunas desejadas
dataset <- dataset[, c("gender", "Team", "Background", "Pro.level", "Winter.Training.Camp", "altitude_results", "vo2_results", "hr_results", "Continent")]

# Adicionar a idade previamente calculada
dataset <- cbind(dataset, age)
rm("age")


# D)
########################## ONE-LABEL ENCODING ##################################

dataset$gender <- as.numeric(as.factor(dataset$gender)) - 1
dataset$Pro.level <- as.numeric(as.factor(dataset$Pro.level)) - 1
dataset$Winter.Training.Camp <- as.numeric(as.factor(dataset$Winter.Training.Camp)) - 1

labeled_data <- data.frame(
  gender = dataset$gender,
  Pro.level = dataset$Pro.level,
  Winter.Training.Camp = dataset$Winter.Training.Camp
)


################################################################################


########################## ONE-HOT ENCODING ####################################

# Identificação de variáveis não numéricas
categorical_vars <- sapply(dataset, is.character)

# Especificação das variáveis para fazer encoding
variables <- colnames(dataset)[categorical_vars]

# Aplicar o one-hot encoding
encoded_data <- predict(dummyVars("~.", data = dataset[, variables]), newdata = dataset)

rm("categorical_vars", "variables")

################################################################################


########################## MIN-MAX Normalization ####################################

# Remover as colunas de Gender, Pro.level e Winter.Training.Camp, vão ser adicionadas de seguida
dataset <- dataset[, c("altitude_results", "vo2_results", "hr_results", "age")]

#numeric_cols <- sapply(dataset, is.numeric)
numeric_data <- dataset

# Min-Max Scaling
scaled_data <- as.data.frame(lapply(numeric_data, function(y) {
  (y - min(y)) / (max(y) - min(y))
}))

# Atribuição dos valores para o dataset original
dataset <- cbind(labeled_data,encoded_data, scaled_data)

# Limpeza dos dados com normalizacao aplicada
rm("numeric_data", "labeled_data","encoded_data", "scaled_data")


# Corrigir colunas com um espaço a mais
colnames(dataset)[4:8] <- c("TeamGroupA","TeamGroupB","TeamGroupC","TeamGroupD","TeamGroupE")
colnames(dataset)[14] <- "BackgroundTimeTrial"
colnames(dataset)[19:20] <- c("ContinentNorthAmerica","ContinentSouthAmerica")

################################################################################

# 5.
# Cores para a matriz de correlação
col_palette <- colorRampPalette(c("#FF8080", "#FFB3B3", "#FAFAFA", "#BCCCFF", "#663366"))

# Matriz de correlação em si
correlation_matrix <- cor(dataset)

#Plot com a representação da matriz
corrplot(correlation_matrix, method = "color", 
         col = col_palette(200),
         order = "hclust",
         addCoef.col = "black",
         tl.cex = 0.3, tl.col = "black",
         diag = TRUE)


# 6.
# Criar a amostra dos dados em treino e teste
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.70, 0.30))

dataset.train <- dataset[sample,]
dataset.test <- dataset[!sample,]

# Resumo estatístico dos valores da variável altitude_results nos conjuntos de treino e treino
summary(dataset.train$altitude_results)
summary(dataset.test$altitude_results)

# a)
# Criar um modelo de regressão linear simples
# A variável dependente é altitude_results e a variável independente é hr_results
slr.model <- lm(altitude_results ~ hr_results, data = dataset.train)
slr.model # Equação: altitude_results = 0.02919 + 0.86157*hr_results

# b)
# Criar o gráfico de dispersão e a reta de regressão
ggplot(slr.model, aes(hr_results, altitude_results)) +
  geom_point() + stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = hr_results, yend= .fitted), color = "red")

# c)
# Realizar previsão dos valores utilizando o modelo de regressão linear simples
slr.pred = predict(slr.model, dataset.test) ; slr.pred

# Calcular a diferença entre os valores reais e previstos
dif = dataset.test$altitude_results - slr.pred ; dif

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
slr.mae = mean(abs(dif))
cat("mae : ", slr.mae)
# mae : 0.09047102

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
slr.rmse = sqrt(mean(dif^2))
cat("rmse : ", slr.rmse)
# rmse : 0.1088708

# d)
# Criar um modelo de regressão linear mais complexo (regressão linear múltipla)
complex.model <- lm(altitude_results ~ hr_results + vo2_results, data = dataset.train)
complex.model # Equação: altitude_results = 0.01565 + 0.28762*hr_results + 0.59886*vo2_results

# Fazer a previsão dos valores utilizando o modelo
complex.model.pred = predict(complex.model, dataset.test) ; complex.model.pred

# Calcular a diferença entre os valores reais e previstos
dif = dataset.test$altitude_results - complex.model.pred ; dif

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
complex.model.mae = mean(abs(dif))
cat("mae : ", complex.model.mae)
# mae : 0.08699231

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
complex.model.rmse = sqrt(mean(dif^2))
cat("rmse : ", complex.model.rmse)
# rmse : 0.1074573


# 7.
# a)
# Criar um modelo de regressão linear múltipla
# A variável dependente é vo2_results e as variáveis independentes são altitude_results e hr_results
mlr.model = lm(vo2_results ~ altitude_results + hr_results, data = dataset.train)
mlr.model # Equação: vo2_results = 0.01845 + 0.14254*altitude_results + 0.83559*hr_results

# Resumo estatístico do modelo de regressão linear múltipla
summary(mlr.model)

# Fazer a previsão dos valores utilizando o modelo de regressão linear múltipla
mlr.pred = predict(mlr.model, dataset.test) ; mlr.pred

# Calcular a diferença entre os valores reais e previstos
dif = dataset.test$vo2_results - mlr.pred ; dif

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
mlr.mae = mean(abs(dif))
cat("mae : ", mlr.mae)
# mae : 0.04313445

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
mlr.rmse = sqrt(mean(dif^2))
cat("rmse : ", mlr.rmse)
# rmse : 0.0536209

# b)
# Criar um modelo de árvore de decisão
# A variável dependente é vo2_results e as variáveis independentes são altitude_results e hr_results
tree.model = rpart(vo2_results ~ altitude_results + hr_results, method = "anova", data = dataset.train) ; tree.model

# Gerar um gráfico visual da árvore de decisão criada
rpart.plot(tree.model)

# Outra alternativa de visualização da árvore (mais detalhada)
rpart.plot(tree.model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# A variável que mais influencia os resultados da árvore é hr_results.

# c)
# Modelos de redes neuronais com vários parâmetros

# Criar um modelo de rede neural com um único nó interno.
# A variável dependente é vo2_results e as variáveis independentes são altitude_results e hr_results
numnodes <- 1

nn.model <- 
  neuralnet(
    vo2_results ~ altitude_results + hr_results,
    data = dataset.train,
    hidden = numnodes
  )
# Gerar o gráfico da rede neuronal criada
plot(nn.model)

# Apresentar os resultados da rede neuronal
nn.model$result.matrix

# Criar um modelo de rede neuronal com três nós internos
numnodes <- 3

nn.model.i <- 
  neuralnet(
    vo2_results ~ altitude_results + hr_results,
    data = dataset.train,
    hidden = numnodes
  )
# Gerar o gráfico da rede neuronal criada
plot(nn.model.i)

# Apresentar os resultados da rede neuronal
nn.model.i$result.matrix


# Criar um modelo de rede neuronal com 2 níveis internos:
# o primeiro com 6 nós internos e o segundo com 2 nós internos
numnodes <- c(6,2)

nn.model.ii <- 
  neuralnet(
    vo2_results ~ altitude_results + hr_results,
    data = dataset.train,
    hidden = numnodes
  )
# Gerar o gráfico da rede neuronal criada
plot(nn.model.ii)

# Apresentar os resultados da rede neuronal
nn.model.ii$result.matrix


# 8.
# Fazer a previsão dos valores utilizando o modelo da regressão linear múltipla (RLM)
mlr.pred = predict(mlr.model, dataset.test) ; mlr.pred

# Calcular a diferença entre os valores reais e previstos
dif = dataset.test$vo2_results - mlr.pred ; dif

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
mlr.mae = mean(abs(dif))
cat("mae : ", mlr.mae)
# mae : 0.04313445

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
mlr.rmse = sqrt(mean(dif^2))
cat("rmse : ", mlr.rmse)
# rmse : 0.0536209


# Fazer a previsão dos valores utilizando o modelo da árvore de decisão
tree.pred = predict(tree.model, dataset.test)

# Calcular a diferença entre os valores reais e previstos
dif = dataset.test$vo2_results - tree.pred

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
tree.mae = mean(abs(dif))
cat("mae : ", tree.mae)
# mae : 0.0505167

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
tree.rmse = sqrt(mean(dif^2))
cat("rmse : ", tree.rmse)
# rmse : 0.06429228


# Fazer a previsão dos valores utilizando o modelo da rede neuronal (1 nó interno)
nn.pred = predict(nn.model, dataset.test)

# Calcular a diferença entre os valores reais e previstos
dif = dataset.test$vo2_results - nn.pred

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
nn.mae = mean(abs(dif))
cat("mae : ", nn.mae)
# mae : 0.04382219

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
nn.rmse = sqrt(mean(dif^2))
cat("rmse : ", nn.rmse)
# rmse : 0.05435656


# Modelo                       | MAE (Mean Absolute Error) | RMSE (Root Mean Squared Error) |
# Regressão linear múltipla    | 0.04313445                | 0.0536209                      |
# Árvore de regressão          | 0.0505167                 | 0.06429228                     |
# Rede neuronal (1 nó interno) | 0.04382219                | 0.05435656                     |

# Menor MAE (Mean Absolute Error) e RMSE (Root Mean Squared Error) -> RLM
# Maior MAE (Mean Absolute Error) e RMSE (Root Mean Squared Error) -> Árvore de regressão


# 9.
# 2 melhores modelos são RLM e Rede neuronal (1 nó interno)

# Criar amostra para cada um dos modelos
mlr.sample <- c(mlr.mae, mlr.rmse)
nn.sample <- c(nn.mae, nn.rmse)

# H0: Os resultados obtidos para os dois modelos são estatisticamente significativos
# H1: Os resultados obtidos para os dois modelos não são estatisticamente significativos

# Teste t de Student com significância de 5% (default)
t.test(mlr.sample, nn.sample)

# Como p = 0.9324 > alfa = 0.05, não existe evidência estatística suficiente para se rejeitar H0.
# Logo, conclui-se que os resultados obtidos para os dois modelos são estatisticamente significativos.
# O mais eficiente é aquele que apresenta um MAE e um RMSE menor, ou seja, o modelo da regressão linear múltipla.


#-------------------------------------------------------------------------------

# Classificação

# 1.

# Criar um dataset limpo para incluir apenas os campos com correlação elevada com o Pro.level
clean_dataset <- dataset[c("gender","Pro.level", "altitude_results", "vo2_results", "hr_results")]
index <- sample(1:nrow(dataset), 0.7 * nrow(dataset))

# Criação do dataset de treino e teste
clean_dataset.train <- clean_dataset[index,]
clean_dataset.test <- clean_dataset[-index,]


####################################### Modelo de Árvore de Decisão ####################################

# Elaboração do modelo da Decision Tree
model_dt <- rpart(
  Pro.level~ gender + altitude_results + vo2_results + hr_results,
  method="anova", data = clean_dataset.train)

# Representação do modelo sob a forma de plot
rpart.plot(model_dt)
predictions_dt <- predict(model_dt,  clean_dataset.test)
predictions_dt <- ifelse(predictions_dt > 0.5, "1", "0")

# Cálculo da precisão do modelo
accuracy_dt <- sum(predictions_dt == clean_dataset.test$Pro.level) / length(clean_dataset.test$Pro.level) * 100

########################################################################################################

####################################### Modelo de Rede Neuronal ########################################

# 2 Layers, com 6 e 2 nós, respetivamente
numnodes <- 1#c(6,2)

# Modelo de rede neural
model_nn <- neuralnet(
  Pro.level~ gender + altitude_results + vo2_results + hr_results,
  hidden = numnodes, data = clean_dataset.train)

# Representação do modelo sob a forma de plot
plot(model_nn)
predictions_nn <- predict(model_nn, clean_dataset.test)
predictions_nn <- ifelse(predictions_nn > 0.5, "1", "0")

# Cálculo da precisão do modelo
accuracy_nn <- sum(predictions_nn == clean_dataset.test$Pro.level) / length(clean_dataset.test$Pro.level) * 100

########################################################################################################

####################################### Modelo de k-vizinhos-mais-próximos #######################################

# Averiguação do melhor k
k <- c()
accuracy <- c()
for (i in seq(1, 50, 2)){
  
  knn.pred <- knn(train = clean_dataset.train[, -which(names(clean_dataset) == "Pro.level")],
                  test = clean_dataset.test[, -which(names(clean_dataset) == "Pro.level")],
                  cl = clean_dataset.train$Pro.level, k=i) 
  
  predictions_knn <- as.factor(knn.pred)
  
  cfmatrix <- table(clean_dataset.test$Pro.level, knn.pred)
  
  accuracy <- c(accuracy, sum(diag(cfmatrix))/sum(cfmatrix))
  
  k <- c(k,i)
}

# Verificação da precisão máxima
resNeigh<-data.frame(k,accuracy)
resNeigh[resNeigh$accuracy==max(resNeigh$accuracy), ]
plot(resNeigh$k,resNeigh$accuracy)

# Precisão máxima para k = 41

# Elaboração do modelo
model_knn <- knn(train = clean_dataset.train[, -which(names(clean_dataset) == "Pro.level")],
                 test = clean_dataset.test[, -which(names(clean_dataset) == "Pro.level")],
                 cl = clean_dataset.train$Pro.level,
                 k = 41) #resNeigh[resNeigh$accuracy==max(resNeigh$accuracy), ][1] - Uso Dinamico provocava erro

# Confusion Matrix
cfmatrix <- table(clean_dataset.test$Pro.level, model_knn)

# Cálculo da prediction
predictions_knn <- as.factor(model_knn)

# Cálculo da precisão
accuracy_knn <- sum(diag(cfmatrix))/sum(cfmatrix) * 100

##################################################################################################################

# 1.A)

# Definido o número de folds (k)
cvf <- 10
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)
numnodes <- 1#c(6,2)

# Realize k-fold cross validation

accuracy <- matrix(nrow = cvf, ncol = 2)

for (i in 1:cvf) {
  
  # Separe os dados em treinamento e teste usando os folds
  dataset.train <- dataset[folds != i, ]
  dataset.test <- dataset[folds == i, ]
  
  # Trino do Modelo de rede neural
  model_nn <- neuralnet(
    Pro.level~ gender + altitude_results + vo2_results + hr_results,
    hidden = numnodes, data = dataset.train)
  
  # Fazer as previsões através do modelo de rede neuronal
  predictions_nn <- predict(model_nn, dataset.test)
  predictions_nn <- ifelse(predictions_nn > 0.5, "1", "0")
  
  # Cálculo d<a taxa de precisão da rede neuronal
  accuracy_nn <- sum(predictions_nn == dataset.test$Pro.level) / length(dataset.test$Pro.level) * 100
  
  
  
  # Treine o modelo KDD
  model_knn <- knn(train = dataset.train[, -which(names(dataset.train) == "Pro.level")],
                   test = dataset.test[, -which(names(dataset.test) == "Pro.level")],
                   cl = dataset.train$Pro.level,
                   k = 41)
  
  # Cálculo da Confusion Matrix
  cfmatrix <- table(dataset.test$Pro.level, model_knn)
  
  # Cálculo da prediction
  predictions_knn <- as.factor(model_knn)
  
  # Cálculo da precisão
  accuracy_knn <- sum(diag(cfmatrix))/sum(cfmatrix) * 100
  
  # Disposição em Array para efetuar os cálculos seguintes
  accuracy[i, ] <- c( accuracy_nn,
                      accuracy_knn) 
}

accuracy

apply(accuracy, 2, mean)
apply(accuracy, 2, sd)

# 1.B)
# Este modelo não realiza um processo explícito de treinamento, ou seja, ele armazena os dados de treino e
# efetua cálculos sob demanda para cada previsão. 
# Isso pode resultar num tempo de treino mais rápido, mas um tempo de previsão mais longo, 
# pois é necessário calcular a distância entre os pontos de treino e o novo ponto para cada previsão.


# 1.C)

# Efetuar o t-test
t_test <- t.test(accuracy[, 1], accuracy[, 2], paired = TRUE)

# Extrair o p-value após a realização do teste
p_value <- t_test$p.value

# Verificação do nível de significância
alpha <- 0.05

# Comparação do p-value com a significância escolhida
if (p_value < alpha) {
  if (mean(accuracy[, 1]) > mean(accuracy[, 2])) {
    best_model <- "Neural Network"
  } else {
    best_model <- "K-Nearest Neighbors"
  }
  result <- paste("There is a significant difference between the models.",
                  "The", best_model, "performs better.")
} else {
  result <- "There is no significant difference between the models."
}

result


# 2.
# Criar a amostra dos dados em treino e teste
sample.wtc <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.70, 0.30))

dataset.train <- dataset[sample.wtc,]
dataset.test <- dataset[!sample.wtc,]

# Resumo estatístico dos valores da variável Winter.Training.Camp nos conjuntos de treino e treino
summary(dataset.train$Winter.Training.Camp)
summary(dataset.test$Winter.Training.Camp)


# Criar um modelo de árvore de decisão
# A variável dependente é Winter.Training.Camp e todas as restantes são variáveis independentes
tree.model = rpart(Winter.Training.Camp ~ ., method = "anova", data = dataset.train) ; tree.model
rpart.plot(tree.model)

# Outra alternativa de visualização da árvore (mais detalhada)
rpart.plot(tree.model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# Fazer a previsão dos valores utilizando o modelo da árvore de decisão
tree.pred = predict(tree.model, dataset.test)

# Calcular a diferença entre os valores reais e previstos
dif = dataset.test$Winter.Training.Camp - tree.pred

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
tree.mae = mean(abs(dif))
cat("mae : ", tree.mae)
# mae : 0.423244

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
tree.rmse = sqrt(mean(dif^2))
cat("rmse : ", tree.rmse)
# rmse : 0.4950887


# Criar um modelo de rede neural com 1 único nó interno
# A variável dependente é Winter.Training.Camp e todas as restantes são variáveis independentes
numnodes.1 <- 1

nn.model.1 <- 
  neuralnet(
    Winter.Training.Camp ~ .,
    data = dataset.train,
    hidden = numnodes.1
  )
# Gerar o gráfico da rede neuronal criada
plot(nn.model.1)

# Apresentar os resultados da rede neuronal
nn.model.1$result.matrix

# Fazer a previsão dos valores utilizando o modelo da rede neuronal (1 nó interno)
nn.pred.1 = predict(nn.model.1, dataset.test)

# Calcular a diferença entre os valores reais e previstos
dif.1 = dataset.test$Winter.Training.Camp - nn.pred.1

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
nn.mae.1 = mean(abs(dif.1))
cat("mae : ", nn.mae.1)
# mae : 0.3664197

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
nn.rmse.1 = sqrt(mean(dif.1^2))
cat("rmse : ", nn.rmse.1)
# rmse : 0.4455933


# Criar um modelo de rede neuronal com 2 nós internos
numnodes.2 <- 2

nn.model.2 <- 
  neuralnet(
    Winter.Training.Camp ~ .,
    data = dataset.train,
    hidden = numnodes.2
  )
# Gerar o gráfico da rede neuronal criada
plot(nn.model.2)

# Apresentar os resultados da rede neuronal
nn.model.2$result.matrix

# Fazer a previsão dos valores utilizando o modelo da rede neuronal (2 nós internos)
nn.pred.2 = predict(nn.model.2, dataset.test)

# Calcular a diferença entre os valores reais e previstos
dif.2 = dataset.test$Winter.Training.Camp - nn.pred.2

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
nn.mae.2 = mean(abs(dif.2))
cat("mae : ", nn.mae.2)
# mae : 0.388896

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
nn.rmse.2 = sqrt(mean(dif.2^2))
cat("rmse : ", nn.rmse.2)
# rmse : 0.470537


# Criar um modelo de rede neuronal com 3 nós internos
numnodes.3 <- 3

nn.model.3 <- 
  neuralnet(
    Winter.Training.Camp ~ .,
    data = dataset.train,
    hidden = numnodes.3
  )
# Gerar o gráfico da rede neuronal criada
plot(nn.model.3)

# Apresentar os resultados da rede neuronal
nn.model.3$result.matrix

# Fazer a previsão dos valores utilizando o modelo da rede neuronal (3 nós internos)
nn.pred.3 = predict(nn.model.3, dataset.test)

# Calcular a diferença entre os valores reais e previstos
dif.3 = dataset.test$Winter.Training.Camp - nn.pred.3

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean Absolute Error)
nn.mae.3 = mean(abs(dif.3))
cat("mae : ", nn.mae.3)
# mae : 0.4011656

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
nn.rmse.3 = sqrt(mean(dif.3^2))
cat("rmse : ", nn.rmse.3)
# rmse : 0.5008155


# Criar um modelo de rede neuronal com 4 nós internos
numnodes.4 <- 4

nn.model.4 <- 
  neuralnet(
    Winter.Training.Camp ~ .,
    data = dataset.train,
    hidden = numnodes.4
  )
# Gerar o gráfico da rede neuronal criada
plot(nn.model.4)

# Apresentar os resultados da rede neuronal
nn.model.4$result.matrix

# Fazer a previsão dos valores utilizando o modelo da rede neuronal (4 nós internos)
nn.pred.4 = predict(nn.model.4, dataset.test)

# Calcular a diferença entre os valores reais e previstos
dif.4 = dataset.test$Winter.Training.Camp - nn.pred.4

# Calcular o erro absoluto médio entre os valores reais e previstos
# MAE (Mean absolute error)
nn.mae.4 = mean(abs(dif.4))
cat("mae : ", nn.mae.4)
# mae : 0.4214287

# Calcular a raiz quadrada do erro médio entre os valores reais e previstos
# RMSE (Root Mean Squared Error)
nn.rmse.4 = sqrt(mean(dif.4^2))
cat("rmse : ", nn.rmse.4)
# rmse : 0.5097741


# Modelo                         | MAE (Mean absolute error) | RMSE (Root Mean Squared Error) |
# Árvore de regressão            | 0.423244                  | 0.4950887                      |
# Rede neuronal (1 nó interno)   | 0.3664197                 | 0.4455933                      |
# Rede neuronal (2 nós internos) | 0.388896                  | 0.470537                       |
# Rede neuronal (3 nós internos) | 0.4011656                 | 0.5008155                      |
# Rede neuronal (4 nós internos) | 0.4214287                 | 0.5097741                      |


# a)
# 2 melhores modelos: Árvore de regressão e Rede neuronal (1 nó interno)

# Definir o número de folds
cvf <- 11

# Gerar amostras aleatórias de números de 1 a 11 para atribuir um fold a cada observação do dataset
# A substituição é permitida para que um mesmo fold possa ser selecionado mais de uma vez
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

# Apresentar uma tabela com a contagem de observações em cada fold
table(folds)

# Variáveis auxiliares
accuracy <- matrix(nrow = cvf, ncol = 2)
numnodes <- 1

for (i in 1:cvf) {
  # Dividir o dataset num conjunto de treino e teste
  train.cv <- dataset[folds != i, ]
  test.cv <- dataset[folds == i, ]
  
  # Obter a variável dependente (Winter.Training.Camp) para o treino e teste
  train_labels <- dataset[folds != i, "Winter.Training.Camp"]
  tst_labels <- dataset[folds == i, "Winter.Training.Camp"]
  
  # Criar o modelo de árvore de decisão
  # A variável dependente é Winter.Training.Camp e todas as restantes são variáveis independentes
  rpart.model <- rpart(Winter.Training.Camp ~ ., method="class", data = train.cv)
  
  # Fazer a previsão dos valores utilizando o modelo da árvore de decisão
  rpart.pred <- predict(rpart.model, test.cv, type = "class")
  
  # Criar matriz de confusão comparando as classes reais com as classes previstas
  cfmatrpart <- table(tst_labels, rpart.pred)
  
  
  # Criar o modelo da rede neuronal (1 nó interno)
  # A variável dependente é Winter.Training.Camp e todas as restantes são variáveis independentes
  nn.model <- 
    neuralnet(
      Winter.Training.Camp ~ .,
      data = train.cv,
      hidden = numnodes
    )
  
  # Fazer a previsão dos valores utilizando o modelo da rede neuronal (1 nó interno)
  nn.pred <- compute(nn.model, test.cv[, -sample])$net.result
  
  # Converter as probabilidades de previsão em classes binárias utilizando um limiar de corte de 0.5
  # Valores acima de 0.5 são atribuídos à classe 1, enquanto valores abaixo de 0.5 são atribuídos à classe 0
  nn.pred <- ifelse(nn.pred > 0.5, 1, 0)
  
  # Criar matriz de confusão comparando as classes reais com as classes previstas
  cfmatneuralnet <- table(tst_labels, nn.pred)
  
  # Calcular a precisão para cada modelo e armazenar os resultados na matriz accuracy
  accuracy[i, ] <- c(
    sum(diag(cfmatrpart))/sum(cfmatrpart),
    sum(diag(cfmatneuralnet))/sum(cfmatneuralnet)
  )
}
# Apresentar a matriz accuracy
accuracy

# Calcular a média e desvio padrão da precisão de cada modelo ao longo dos folds
apply(accuracy, 2, mean)
apply(accuracy, 2, sd)

# Para k = 10
# Modelo                       | Accuracy Mean | Accuracy SD |
# Árvore de decisão            | 0.6905398     | 0.03779142  |
# Rede neuronal (1 nó interno) | 0.6692691     | 0.04917135  |

# Para k = 11
# Modelo                       | Accuracy Mean | Accuracy SD |
# Árvore de decisão            | 0.7046665     | 0.05105016  |
# Rede neuronal (1 nó interno) | 0.6710215     | 0.05080322  |

# Para k = 12
# Modelo                       | Accuracy Mean | Accuracy SD |
# Árvore de decisão            | 0.7002333     | 0.04956924  |
# Rede neuronal (1 nó interno) | 0.6690206     | 0.04606955  |


# b)
# 2 melhores modelos: Árvore de decisão e Rede neuronal (1 nó interno)

# Definir valores de precisão de cada modelo
tree.accuracy <- accuracy[, 1]
nn.accuracy <- accuracy[, 2]

# H0: Existe diferença significativa no desempenho dos dois modelos
# H1: Não existe diferença significativa no desempenho dos dois modelos

# Realizar teste t com significância de 5%
t.test(tree.accuracy, nn.accuracy)

# Como p = 0.137 > alfa = 0.05, não existe evidência estatística suficiente para se rejeitar H0.
# Logo, conclui-se que existe diferença significativa no desempenho dos dois modelos.


# c)
# Definir o número de folds
cvf <- 11

# Gerar amostras aleatórias de números de 1 a 11 para atribuir um fold a cada observação do dataset
# A substituição é permitida para que um mesmo fold possa ser selecionado mais de uma vez
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

# Apresentar uma tabela com a contagem de observações em cada fold
table(folds)

# Variáveis auxiliares
accuracy <- matrix(nrow = cvf, ncol = 2)
sensitivity <- matrix(nrow = cvf, ncol = 2)
specificity <- matrix(nrow = cvf, ncol = 2)
f1 <- matrix(nrow = cvf, ncol = 2)
numnodes <- 1

for (i in 1:cvf) {
  # Dividir o dataset num conjunto de treino e teste
  train.cv <- dataset[folds != i, ]
  test.cv <- dataset[folds == i, ]
  
  # Obter a variável dependente (Winter.Training.Camp) para o treino e teste
  train_labels <- dataset[folds != i, "Winter.Training.Camp"]
  tst_labels <- dataset[folds == i, "Winter.Training.Camp"]
  
  # Criar o modelo de árvore de decisão
  # A variável dependente é Winter.Training.Camp e todas as restantes são variáveis independentes
  rpart.model <- rpart(Winter.Training.Camp ~ ., method="class", data = train.cv)
  
  # Fazer a previsão dos valores utilizando o modelo da árvore de decisão
  rpart.pred <- predict(rpart.model, test.cv, type = "class")
  
  # Criar matriz de confusão comparando as classes reais com as classes previstas
  cfmatrpart <- table(tst_labels, rpart.pred)
  
  
  # Criar o modelo da rede neuronal (1 nó interno)
  # A variável dependente é Winter.Training.Camp e todas as restantes são variáveis independentes
  nn.model <- 
    neuralnet(
      Winter.Training.Camp ~ .,
      data = train.cv,
      hidden = numnodes
    )
  
  # Fazer a previsão dos valores utilizando o modelo da rede neuronal (1 nó interno)
  nn.pred <- compute(nn.model, test.cv[, -sample])$net.result
  
  # Converter as probabilidades de previsão em classes binárias utilizando um limiar de corte de 0.5
  # Valores acima de 0.5 são atribuídos à classe 1, enquanto valores abaixo de 0.5 são atribuídos à classe 0
  nn.pred <- ifelse(nn.pred > 0.5, 1, 0)
  
  # Criar matriz de confusão comparando as classes reais com as classes previstas
  cfmatneuralnet <- table(tst_labels, nn.pred)
  
  # Calcular a precisão para cada modelo e armazenar os resultados na matriz accuracy
  accuracy[i, ] <- c(
    sum(diag(cfmatrpart))/sum(cfmatrpart),
    sum(diag(cfmatneuralnet))/sum(cfmatneuralnet)
  )
  
  # Calcular a sensibilidade (taxa de verdadeiros positivos) para cada modelo
  sensitivity_rpart <- sensitivity(cfmatrpart)[2]
  sensitivity_neuralnet <- sensitivity(cfmatneuralnet)[2]
  
  # Verificar se a sensibilidade é um valor válido
  # Caso contrário, atribuir 0
  if (is.na(sensitivity_rpart)) sensitivity_rpart <- 0
  if (is.na(sensitivity_neuralnet)) sensitivity_neuralnet <- 0
  
  # Armazenar as sensibilidades dos 2 modelos na matriz sensitivity
  sensitivity[i, ] <- c(
    sensitivity_rpart,
    sensitivity_neuralnet
  )
  
  # Calcular a especificidade (taxa de verdadeiros negativos) para cada modelo
  # e armazenar os resultados na matriz specificity
  specificity[i, ] <- c(
    specificity(cfmatrpart)[1],
    specificity(cfmatneuralnet)[1]
  )
  
  # Calcular a medida F1 para cada modelo
  # e armazenar os resultados na matriz f1
  f1[i, ] <- c(
    (2 * cfmatrpart[2, 2]) / (sum(cfmatrpart[, 2]) + sum(cfmatrpart[2, ])),
    (2 * cfmatneuralnet[2, 2]) / (sum(cfmatneuralnet[, 2]) + sum(cfmatneuralnet[2, ]))
  )
}

# Calcular a média das métricas de desempenho para cada modelo ao longo dos folds
mean_accuracy <- apply(accuracy, 2, mean)
mean_sensitivity <- apply(sensitivity, 2, mean)
mean_specificity <- apply(specificity, 2, mean)
mean_f1 <- apply(f1, 2, mean)

# Apresentar as médias das métricas de desempenho de cada modelo
cat("Mean Accuracy:", mean_accuracy, "\n")
cat("Mean Sensitivity:", mean_sensitivity, "\n")
cat("Mean Specificity:", mean_specificity, "\n")
cat("Mean F1:", mean_f1, "\n")

# Modelo                       | Mean Accuracy | Mean Sensitivity | Mean Specificity | Mean F1
# Árvore de decisão            | 0.6893017     | 0                | 0.7128468        | 0.7850189 
# Rede neuronal (1 nó interno) | 0.688401      | 0                | 0.7699543        | 0.7562122 

########################################################################################################

# Exercício 3.

sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.70, 0.30))


########################################################################################################

####################################### Modelo de Rede Neuronal ########################################

# Internal nodes
numnodes <- c(16,10,6,2)

data.train <- dataset[sample,]
data.tst <- dataset[-sample, ]

#Modelo NN
nn.model.ex.3 <- 
  neuralnet(
    gender ~ .,
    data = data.train,
    hidden = numnodes
  )
plot(nn.model.ex.3)

#Pred NN
nn.pred.ex.3 = predict(nn.model.ex.3, data.tst)
dif = data.tst$gender - nn.pred.ex.3

# MAE
nn.mae.ex.3 = mean(abs(dif))
cat("mae : ", nn.mae.ex.3)

# RMSE
nn.rmse.ex.3 = sqrt(mean(dif^2))
cat("rmse : ", nn.rmse.ex.3)

# Modelo                           | MAE        | RMSE       |
# Rede neuronal (6,2 nós)          | 0.1109829  | 0.2822349  |
# Rede neuronal (9,3 nós)          | 0.1109829  | 0.2822349  |
# Rede neuronal (3,1 nós)          | 0.1016662  | 0.2591252  |
# Rede neuronal (10,6,2 nós)       | 0.09024785 | 0.2538431  |
# Rede neuronal (16,10,6,2) nós)   | 0.0727091  | 0.2588458  |
# Rede neuronal (20,16,10,6,2) nós)| 0.06134385 | 0.2342042  |
# Rede neuronal (1 nó)             | 0.1583033  | 0.2870171  |

# Uma vez que usando os nós (20,16,10,6,2) demora imenso tempo só a fazer uma vez o modelo vamos adaptar o modelo com os nodes (16,10,6,2) pois os valores não diferem
# tanto e é possível usar este modelo em K-Cross models uma vez que vai realizar o mesmo modelo 1-12 vezes sobre partes diferentes do dataset.train e dataset.test, logo vai demorar imenso tempo

##################################################################################################################

####################################### Modelo de k-vizinhos-mais-próximos #######################################

data.train <- dataset[sample, -which(names(dataset) == "gender")]
data.tst <- dataset[-sample, -which(names(dataset) == "gender")]

train_labels <- dataset[sample, "gender"]
tst_labels <- dataset[-sample, "gender"]

k <- c()
accuracy <- c()
for (i in seq(1, 50, 2)){   
  
  knn.pred <- knn(train=data.train, test=data.tst, cl=train_labels, k=i) 
  
  cfmatrix <- table(tst_labels,knn.pred)
  
  accuracy <- c(accuracy, sum(diag(cfmatrix))/sum(cfmatrix))
  
  k <- c(k,i)
}
resNeigh<-data.frame(k,accuracy)

#Max accuracy
resNeigh[resNeigh$accuracy==max(resNeigh$accuracy), ]   
plot(resNeigh$k,resNeigh$accuracy)

#   k  accuracy
#1 1 0.8508509
# Melhor K -> k=1

#Plot Max accuracy
resNeigh[resNeigh$accuracy == max(resNeigh$accuracy), ]
k[which.max(accuracy)]
plot(
  resNeigh$k,
  resNeigh$accuracy,
  col = ifelse(
    resNeigh$accuracy == max(resNeigh$accuracy),
    'orangered1',
    'steelblue4'
  )
)

# K = 1 é o melhor K dado o plot apresentado, uma vez que é o que apresenta um melhor accuracy

##################################################################################################################

# Alínea a)

# Melhores modelos:
# NN model -> c(16,10,6,2)
# KNN model -> K=1

# Gerar amostras aleatórias de números de 1 a 11 para atribuir um fold a cada observação do dataset
# A substituição é permitida para que um mesmo fold possa ser selecionado mais de uma vez
cvf <- 11
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

# Fold size
table(folds)

# Aux variáveis
accuracy <- matrix(nrow = cvf, ncol = 2)
k <- 1
numnodes <- c(16,10.6,2)

for (i in 1:cvf) {
  # Dividir o dataset num conjunto de treino e teste
  train.cv <- dataset[folds != i, ]
  test.cv <- dataset[folds == i, ]
  
  # Obter a variável dependente (gender) para o treino e teste
  train_labels <- dataset[folds != i, "gender"]
  tst_labels <- dataset[folds == i, "gender"]
  
  # Criar o modelo do knn
  # A variável dependente é gender e todas as restantes são variáveis independentes
  # Fazer a previsão dos valores utilizando o modelo de knn
  knn.pred <- knn(train = train.cv[, -sample], test = test.cv[, -sample], cl = train_labels, k)
  
  # Criar matriz de confusão comparando as classes reais com as classes previstas
  cfmatknn <- table(tst_labels, knn.pred)
  
  # Criar o modelo da rede neuronal c(16,10,6,2) nós
  # A variável dependente é gender e todas as restantes são variáveis independentes
  neuralnet.model <- neuralnet(gender ~ ., data = train.cv, hidden = numnodes)
  
  # Fazer a previsão dos valores utilizando o modelo da rede neuronal c(16,10,6,2) nós
  neuralnet.pred <- compute(neuralnet.model, test.cv[, -sample])$net.result
  
  # Converter as probabilidades de previsão em classes binárias utilizando um limiar de corte de 0.5
  # Valores acima de 0.5 são atribuídos à classe 1, enquanto valores abaixo de 0.5 são atribuídos à classe 0
  neuralnet.pred <- ifelse(neuralnet.pred > 0.5, 1, 0)  # Convert probabilities to binary predictions
  
  # Criar matriz de confusão comparando as classes reais com as classes previstas
  cfmatneuralnet <- table(tst_labels, neuralnet.pred)
  
  # Calcular a precisão para cada modelo e armazenar os resultados na matriz accuracy
  accuracy[i, ] <- c(
    sum(diag(cfmatknn))/sum(cfmatknn),
    sum(diag(cfmatneuralnet))/sum(cfmatneuralnet)
  )
}

# Calcular a média das métricas de desempenho para cada modelo ao longo dos folds
# MEAN
apply(accuracy, 2, mean)

# SD
apply(accuracy, 2, sd)

# KNN/NN
# Após experimentar com vários folds, o melhor fold é o K=10 pois obtemos uma
# accuracy com uma média melhor

# K = X           |  NN Mean   | NN SD      | KNN Mean |  KNN SD  |
# K = 10          | 0.7948120  | 0.03732952 |0.5390539 |0.05025901|
# K = 11          | 0.7916849  | 0.03603084 |0.5289479 |0.02882902|

# O mais eficiente é aquele que apresenta uma Accuracy MEAN maior, ou seja, o modelo da rede neuronal.
# O valor do Accuracy SD significa a variância dos dados (a consistência em si) em que o KNN para o valor de K=12 apresenta uma variancia mais pequena
# Independente do número de folds é possível afirmar que o modelo NN apresenta uma melhor accuracy

##################################################################################################################

# Alínea b)

knn.mean <- apply(accuracy, 2, mean)[1];
knn.sd <- apply(accuracy, 2, sd)[1];

nn.mean <- apply(accuracy, 2, mean)[2];
nn.sd <- apply(accuracy, 2, sd)[2];

# 2 melhores modelos: KNN (k=1) e NN (c(16,10.6,2))

# H0: Os resultados obtidos para os dois modelos são estatisticamente significativos
# H1: Os resultados obtidos para os dois modelos não são estatisticamente significativos
# Criar amostra para cada modelo
knn.sample <- c(knn.mean, knn.sd)
nn.sample <- c(nn.mean, nn.sd)

# Teste t com significância de 5%
t.test(knn.sample, nn.sample)

# Como p = 0.8166 > alfa = 0.05, não existe evidência estatística suficiente para se rejeitar H0.
# Logo, conclui-se que os resultados obtidos para os dois modelos são estatisticamente significativos.

##################################################################################################################

# c)

# Melhores modelos:
# NN model -> c(16,10,6,2)
# KNN model -> K=1

# Gerar amostras aleatórias de números de 1 a 12 para atribuir um fold a cada observação do dataset
# A substituição é permitida para que um mesmo fold possa ser selecionado mais de uma vez
cvf <- 11
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

# Fold size
table(folds)

# Aux variáveis
accuracy <- matrix(nrow = cvf, ncol = 2)
precision <- matrix(nrow = cvf, ncol = 2)
sensitivity <- matrix(nrow = cvf, ncol = 2)
specificity <- matrix(nrow = cvf, ncol = 2)
f1 <- matrix(nrow = cvf, ncol = 2)
k <- 1
numnodes <- c(16,10,6,2)

for (i in 1:cvf) {
  # Dividir o dataset num conjunto de treino e teste
  train.cv <- dataset[folds != i, ]
  test.cv <- dataset[folds == i, ]
  
  # Obter a variável dependente (gender) para o treino e teste
  train_labels <- dataset[folds != i, "gender"]
  tst_labels <- dataset[folds == i, "gender"]
  
  # Criar o modelo do knn
  # A variável dependente é gender e todas as restantes são variáveis independentes
  knn.pred <- knn(train = train.cv[, -sample], test = test.cv[, -sample], cl = train_labels, k)
  
  # Criar matriz de confusão comparando as classes reais com as classes previstas
  cfmatknn <- table(tst_labels, knn.pred)
  
  # Criar o modelo da rede neuronal c(16,10,6,2) nós
  # A variável dependente é gender e todas as restantes são variáveis independentes
  neuralnet.model <- neuralnet(gender ~ ., data = train.cv, hidden = numnodes)
  
  # Fazer a previsão dos valores utilizando o modelo da rede neuronal c(16,10,6,2) nós
  neuralnet.pred <- compute(neuralnet.model, test.cv[, -sample])$net.result
  
  # Converter as probabilidades de previsão em classes binárias utilizando um limiar de corte de 0.5
  # Valores acima de 0.5 são atribuídos à classe 1, enquanto valores abaixo de 0.5 são atribuídos à classe 0
  neuralnet.pred <- ifelse(neuralnet.pred > 0.5, 1, 0)  # Convert probabilities to binary predictions
  
  # Criar matriz de confusão comparando as classes reais com as classes previstas
  cfmatneuralnet <- table(tst_labels, neuralnet.pred)
  
  # Calcular a precisão para cada modelo e armazenar os resultados na matriz accuracy
  accuracy[i, ] <- c(
    sum(diag(cfmatknn))/sum(cfmatknn),
    sum(diag(cfmatneuralnet))/sum(cfmatneuralnet)
  )
  
  # Calcular a precisão para cada modelo
  precision[i, ] <- c(
    cfmatknn[1,1]/sum(cfmatknn[,1]),
    cfmatneuralnet[1,1]/sum(cfmatneuralnet[,1])
  )
  
  # Calcular a sensibilidade (taxa de verdadeiros positivos) para cada modelo
  sensitivity[i, ] <- c(
    cfmatknn[1,1]/sum(cfmatknn[1,]),
    cfmatneuralnet[1,1]/sum(cfmatneuralnet[1,])
  )
  
  # Calcular a especificidade (taxa de verdadeiros negativos) para cada modelo
  # e armazenar os resultados na matriz specificity
  specificity[i, ] <- c(
    cfmatknn[2,2]/sum(cfmatknn[2,]),
    cfmatneuralnet[2,2]/sum(cfmatneuralnet[2,])
  )
  
  # Calcular a medida F1 para cada modelo
  # e armazenar os resultados na matriz f1
  f1[i, ] <- c(
    (2 * precision[i, ][1] * sensitivity[i, ][1] ) / ((precision[i, ][1] + sensitivity[i, ][1])),
    (2 * precision[i, ][2] * sensitivity[i, ][2] ) / ((precision[i, ][2] + sensitivity[i, ][2]))
  )

}

# Calcular a média das métricas de desempenho para cada modelo ao longo dos folds
mean_accuracy <- apply(accuracy, 2, mean)
mean_precision <- apply(precision, 2, mean)
mean_sensitivity <- apply(sensitivity, 2, mean)
mean_specificity <- apply(specificity, 2, mean)
mean_f1 <- apply(f1, 2, mean)

# Apresentar as médias das métricas de desempenho de cada modelo
cat("Mean Accuracy:", mean_accuracy, "\n")
cat("Mean Precision:", mean_precision, "\n")
cat("Mean Sensitivity:", mean_sensitivity, "\n")
cat("Mean Specificity:", mean_specificity, "\n")
cat("Mean F1:", mean_f1, "\n")

####################################################### Valores (KNN/NN) #######################################################

# Modelo                       | Mean Accuracy | Mean Precision     | Mean Sensitivity | Mean Specificity | Mean F1
# KNN                          | 0.5276064     | 0.5439251          | 0.5644452        | 0.4871879        | 0.5521911 
# NN (16,10,6,2) nós           | 0.7908638     | 0.7929163          | 0.8149046        | 0.7613693        | 0.8014577 

################################################################################################################################