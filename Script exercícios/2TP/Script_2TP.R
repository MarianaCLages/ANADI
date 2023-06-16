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
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.70, 0.30))

dataset.train <- dataset[sample,]
dataset.test <- dataset[!sample,]

summary(dataset.train$altitude_results)
summary(dataset.test$altitude_results)

# a)
# Criar um modelo de regressão linear simples
slr.model <- lm(altitude_results ~ hr_results, data = dataset.train)
slr.model # Equação: altitude_results = 0.02919 + 0.86157*hr_results

# b)
# Criar o gráfico de dispersão e a reta de regressão
ggplot(slr.model, aes(hr_results, altitude_results)) +
  geom_point() + stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = hr_results, yend= .fitted), color = "red")

# c)
slr.pred = predict(slr.model, dataset.test) ; slr.pred

dif = dataset.test$altitude_results - slr.pred ; dif

# MAE
slr.mae = mean(abs(dif))
cat("mae : ", slr.mae)
# mae : 0.09047102

# RMSE
slr.rmse = sqrt(mean(dif^2))
cat("rmse : ", slr.rmse)
# rmse : 0.1088708

# d)
complex.model <- lm(altitude_results ~ hr_results + vo2_results, data = dataset.train)
complex.model # Equação: altitude_results = 0.01565 + 0.28762*hr_results + 0.59886*vo2_results

complex.model.pred = predict(complex.model, dataset.test) ; complex.model.pred

dif = dataset.test$altitude_results - complex.model.pred ; dif

# MAE
complex.model.mae = mean(abs(dif))
cat("mae : ", complex.model.mae)
# mae : 0.08699231

# RMSE
complex.model.rmse = sqrt(mean(dif^2))
cat("rmse : ", complex.model.rmse)
# rmse : 0.1074573


# 7.
# a)
mlr.model = lm(vo2_results ~ altitude_results + hr_results, data = dataset.train)
mlr.model # Equação: vo2_results = 0.01845 + 0.14254*altitude_results + 0.83559*hr_results

summary(mlr.model)

mlr.pred = predict(mlr.model, dataset.test) ; mlr.pred

dif = dataset.test$vo2_results - mlr.pred ; dif

# MAE
mlr.mae = mean(abs(dif))
cat("mae : ", mlr.mae)
# mae : 0.04313445

# RMSE
mlr.rmse = sqrt(mean(dif^2))
cat("rmse : ", mlr.rmse)
# rmse : 0.0536209

# b)
tree.model = rpart(vo2_results ~ altitude_results + hr_results, method = "anova", data = dataset.train) ; tree.model
rpart.plot(tree.model)

# outra alternativa de visualização da árvore
rpart.plot(tree.model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# Variável que mais influencia os resultados da árvore: hr_results

# c)
#---------------------
# 1 internal node
numnodes <- 1

nn.model <- 
  neuralnet(
    vo2_results ~ altitude_results + hr_results,
    data = dataset.train,
    hidden = numnodes
  )
plot(nn.model)

nn.model$result.matrix


# 3 internal nodes
numnodes <- 3

nn.model.i <- 
  neuralnet(
    vo2_results ~ altitude_results + hr_results,
    data = dataset.train,
    hidden = numnodes
  )
plot(nn.model.i)

nn.model.i$result.matrix


# 6 first and 2 second internal nodes
numnodes <- c(6,2)

nn.model.ii <- 
  neuralnet(
    vo2_results ~ altitude_results + hr_results,
    data = dataset.train,
    hidden = numnodes
  )
plot(nn.model.ii)

nn.model.ii$result.matrix


# 8.
# Modelo da regressão linear múltipla (RLM)
mlr.pred = predict(mlr.model, dataset.test) ; mlr.pred

dif = dataset.test$vo2_results - mlr.pred ; dif

# MAE
mlr.mae = mean(abs(dif))
cat("mae : ", mlr.mae)
# mae : 0.04313445

# RMSE
mlr.rmse = sqrt(mean(dif^2))
cat("rmse : ", mlr.rmse)
# rmse : 0.0536209


# Modelo da árvore de regressão
tree.pred = predict(tree.model, dataset.test)

dif = dataset.test$vo2_results - tree.pred

# MAE
tree.mae = mean(abs(dif))
cat("mae : ", tree.mae)
# mae : 0.0505167

# RMSE
tree.rmse = sqrt(mean(dif^2))
cat("rmse : ", tree.rmse)
# rmse : 0.06429228


# Modelo da rede neuronal (1 nó interno)
nn.pred = predict(nn.model, dataset.test)

dif = dataset.test$vo2_results - nn.pred

# MAE
nn.mae = mean(abs(dif))
cat("mae : ", nn.mae)
# mae : 0.04382219

# RMSE
nn.rmse = sqrt(mean(dif^2))
cat("rmse : ", nn.rmse)
# rmse : 0.05435656


# Modelo                       | MAE        | RMSE       |
# Regressão linear múltipla    | 0.04313445 | 0.0536209  |
# Árvore de regressão          | 0.0505167  | 0.06429228 |
# Rede neuronal (1 nó interno) | 0.04382219 | 0.05435656 |

# Menor MAE e RMSE -> RLM
# Maior MAE e RMSE -> Árvore de regressão


# 9.
# 2 melhores modelos: RLM e Rede neuronal (1 nó interno)

# Criar amostra para cada modelo
mlr.sample <- c(mlr.mae, mlr.rmse)
nn.sample <- c(nn.mae, nn.rmse)

# H0: Os resultados obtidos para os dois modelos são estatisticamente significativos
# H1: Os resultados obtidos para os dois modelos não são estatisticamente significativos

# Teste t com significância de 5%
t.test(mlr.sample, nn.sample)

# Como p = 0.9324 > alfa = 0.05, não existe evidência estatística suficiente para se rejeitar H0.
# Logo, conclui-se que os resultados obtidos para os dois modelos são estatisticamente significativos.
# O mais eficiente é aquele que apresenta um MAE e um RMSE menor, ou seja, o modelo da regressão linear múltipla.

###################################################### Classificação ########################################################

########## 3

sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.70, 0.30))


########### NN ###########

# Internal nodes
numnodes <- c(16,10,2)

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
# Rede neuronal (16,10,6,2) nós)   | 0.08026149 | 0.2442239  |
# Rede neuronal (20,16,10,6,2) nós)| 0.06134385 | 0.2342042  |
# Rede neuronal (1 nó)             | 0.1583033  | 0.2870171  |

# Uma vez que usando os nós (20,16,10,6,2) demora imenso tempo só a fazer uma vez o modelo vamos adaptar o modelo com os nodes (16,10,6,2) pois os valores não diferem
# tanto e é possível usar este modelo em K-Cross models uma vez que vai realizar o mesmo modelo 10 vezes sobre partes diferentes do dataset.train e dataset.test

########## KNN ###########

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
#1 1 0.8628629
# K=1

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

# K = 1 é o melhor K dado o plot

# a)

# NN model -> c(16,10.6,2)
# KNN model -> K=1

# K-Cross info
cvf <- 12
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

# Fold size
table(folds)

# Aux variáveis
accuracy <- matrix(nrow = cvf, ncol = 2)
k <- 1
numnodes <- c(16,10.6,2)

for (i in 1:cvf) {
  train.cv <- dataset[folds != i, ]
  test.cv <- dataset[folds == i, ]
  
  train_labels <- dataset[folds != i, "gender"]
  tst_labels <- dataset[folds == i, "gender"]
  
  knn.pred <- knn(train = train.cv[, -sample], test = test.cv[, -sample], cl = train_labels, k)
  cfmatknn <- table(tst_labels, knn.pred)
  
  neuralnet.model <- neuralnet(gender ~ ., data = train.cv, hidden = numnodes)
  neuralnet.pred <- compute(neuralnet.model, test.cv[, -sample])$net.result
  neuralnet.pred <- ifelse(neuralnet.pred > 0.5, 1, 0)  # Convert probabilities to binary predictions
  cfmatneuralnet <- table(tst_labels, neuralnet.pred)
  
  accuracy[i, ] <- c(
    sum(diag(cfmatknn))/sum(cfmatknn),
    sum(diag(cfmatneuralnet))/sum(cfmatneuralnet)
  )
}

accuracy

# MEAN
apply(accuracy, 2, mean)

# SD
apply(accuracy, 2, sd)

# Experimentar com vários folds, o melhor fold é o K=12 pois obtemos uma
# accuracy com uma média melhor e um desvio padrão também

# K = X           |  NN Mean   | NN SD      | KNN Mean |  KNN SD  |
# K = 10          | 0.7901825  | 0.06211227 |0.5165388 |0.05987278|
# K = 12          | 0.7820203  | 0.02880992 |0.5210406 |0.04131759|
# K = 13          | 0.7617719  | 0.06211227 |0.5151280 |0.04372504|

# b)

knn.mean <- apply(accuracy, 2, mean)[1];
knn.sd <- apply(accuracy, 2, sd)[1];

nn.mean <- apply(accuracy, 2, mean)[2];
nn.sd <- apply(accuracy, 2, sd)[2];

# 2 melhores modelos: KNN (k=1) e NN (c(16,10.6,2))

# Criar amostra para cada modelo
knn.sample <- c(knn.mean, knn.sd)
nn.sample <- c(nn.mean, nn.sd)

# H0: Os resultados obtidos para os dois modelos são estatisticamente significativos
# H1: Os resultados obtidos para os dois modelos não são estatisticamente significativos

# Teste t com significância de 5%
t.test(knn.sample, nn.sample)

# Como p = 0.8064 > alfa = 0.05, não existe evidência estatística suficiente para se rejeitar H0.
# Logo, conclui-se que os resultados obtidos para os dois modelos são estatisticamente significativos.

# O mais eficiente é aquele que apresenta um MEAN e um SD menor, ou seja, o modelo da rede neuronal.

# c)

cvf <- 12
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

# Fold size
table(folds)

accuracy <- matrix(nrow = cvf, ncol = 2)
sensitivity <- matrix(nrow = cvf, ncol = 2)
specificity <- matrix(nrow = cvf, ncol = 2)
f1 <- matrix(nrow = cvf, ncol = 2)
k <- 1
numnodes <- c(16,10,6,2)

for (i in 1:cvf) {
  train.cv <- dataset[folds != i, ]
  test.cv <- dataset[folds == i, ]
  
  train_labels <- dataset[folds != i, "gender"]
  tst_labels <- dataset[folds == i, "gender"]
  
  knn.pred <- knn(train = train.cv[, -sample], test = test.cv[, -sample], cl = train_labels, k)
  cfmatknn <- table(tst_labels, knn.pred)
  
  neuralnet.model <- neuralnet(gender ~ ., data = train.cv, hidden = numnodes)
  neuralnet.pred <- compute(neuralnet.model, test.cv[, -sample])$net.result
  neuralnet.pred <- ifelse(neuralnet.pred > 0.5, 1, 0)  # Convert probabilities to binary predictions
  cfmatneuralnet <- table(tst_labels, neuralnet.pred)
  
  accuracy[i, ] <- c(
    sum(diag(cfmatknn))/sum(cfmatknn),
    sum(diag(cfmatneuralnet))/sum(cfmatneuralnet)
  )
  
  sensitivity_knn <- ifelse(sum(cfmatknn[2, ]) == 0, 0, cfmatknn[2, 2] / sum(cfmatknn[2, ]))
  sensitivity_neuralnet <- ifelse(sum(cfmatneuralnet[2, ]) == 0, 0, cfmatneuralnet[2, 2] / sum(cfmatneuralnet[2, ]))
  
  sensitivity[i, ] <- c(
    sensitivity_knn,
    sensitivity_neuralnet
  )
  
  specificity[i, ] <- c(
    specificity(cfmatknn)[1],
    specificity(cfmatneuralnet)[1]
  )
  
  f1[i, ] <- c(
    (2 * cfmatknn[2, 2]) / (sum(cfmatknn[, 2]) + sum(cfmatknn[2, ])),
    (2 * cfmatneuralnet[2, 2]) / (sum(cfmatneuralnet[, 2]) + sum(cfmatneuralnet[2, ]))
  )

}

# Calculate the average performance metrics across folds
mean_accuracy <- apply(accuracy, 2, mean)
mean_sensitivity <- apply(sensitivity, 2, mean)
mean_specificity <- apply(specificity, 2, mean)
mean_f1 <- apply(f1, 2, mean)

# Print the average performance metrics
cat("Mean Accuracy:", mean_accuracy, "\n")
cat("Mean Sensitivity:", mean_sensitivity, "\n")
cat("Mean Specificity:", mean_specificity, "\n")
cat("Mean F1:", mean_f1, "\n")

##### Values #####

# Mean Accuracy: KNN/NN
# 0.5436018 0.7999609

# Mean Sensitivity: KNN/NN
# 0.5069086 0.7955286

# Mean Specificity: KNN/NN
# 0.5288314 0.8002718

# Mean F1: KNN/NN
# 0.516018 0.7922352

#K-fold cross-validation provides a more reliable estimate of a model's performance compared to a single train-test split. It helps to 
#mitigate the impact of random variations in the train-test split and provides a more robust evaluation of the model's ability to generalize to unseen data.

### Data analysis ###

#Based on the provided mean values for accuracy, sensitivity, specificity, and F1 score of the KNN (K-Nearest Neighbors) and NN (Neural Network) models
#, the following conclusions can be made:
  
#Accuracy: The mean accuracy of the NN model (0.7999609) is higher than that of the KNN model (0.5436018). 
#This indicates that, on average, the NN model performs better in predicting the "Gender" attribute compared to the KNN model.

#Sensitivity: The mean sensitivity of the NN model (0.7955286) is higher than that of the KNN model (0.5069086). 
#Sensitivity, also known as recall or true positive rate, represents the proportion of actual positive instances correctly predicted as positive. 
#Therefore, the NN model shows better performance in correctly identifying positive instances (in this case, the gender) compared to the KNN model.

#Specificity: The mean specificity of both models is relatively close. The NN model has a slightly higher mean specificity (0.8002718) 
#compared to the KNN model (0.5288314). Specificity represents the proportion of actual negative instances correctly predicted as negative. 
#Both models demonstrate reasonably good performance in identifying negative instances accurately.

#F1 Score: The mean F1 score of the NN model (0.7922352) is higher than that of the KNN model (0.516018). 
#The F1 score is a measure of the balance between precision and recall (sensitivity). It combines both metrics and provides an overall measure of model 
#performance. The higher F1 score for the NN model suggests better overall performance in terms of precision and recall compared to the KNN model.

#In summary, based on the given mean values, the NN model generally outperforms the KNN model in terms of accuracy, sensitivity, specificity, 
#and F1 score when predicting the "Gender" attribute. It is important to consider additional factors such as model complexity, computational requirements, 
#and the specific characteristics of the dataset when making a final decision about model selection.