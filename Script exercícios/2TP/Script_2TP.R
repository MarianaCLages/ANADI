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

# 1.
# Definição do caminho em que se encontra o script
#setwd("C:/Users/franc/Documents/Repositórios/anadi23/Script exercícios/2TP")
setwd("C:/Users/maria/Desktop/ISEP/3ºano/2ºsemestre/ANADI/anadi23/Script exercícios/2TP")

#"/Users/fredol/Documents/isep/anadi23/Script exercícios/2TP"
#"C:/Users/franc/Documents/Repositórios/anadi23/Script exercícios/2TP"

# Importação dos dados
dataset <- read.csv("ciclismo.csv", header = TRUE, stringsAsFactors = FALSE)
dataset <- dataset[, !colnames(dataset) %in% "ID"]

# Verificação da dimensão do datasets
dimensao <- dim(dataset)

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
dataset <- dataset[, c("gender", "Pro.level", "Winter.Training.Camp", "altitude_results", "vo2_results", "hr_results")]

# Adicionar a idade previamente calculada
dataset <- cbind(dataset, age)


# D)

########################## One Label Encoding ####################################

# Identificação de variáveis não numéricas
categorical_vars <- sapply(dataset, is.character)

# Especificação de variáveis eligíveis para one-label encoding
variables <- colnames(dataset)[categorical_vars]

encoded_data <- dataset[, categorical_vars]

# Aplicação do encoding
for (var in variables) {
  encoded_data[[var]] <- as.integer(as.factor(encoded_data[[var]]))
  encoded_data[[var]] <- encoded_data[[var]] - 1 
}

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
dataset <- cbind(encoded_data, scaled_data)

# Limpeza dos dados com normalizacao aplicada
rm("numeric_data", "encoded_data", "scaled_data")

################################################################################


# 5.

numeric_cols <- sapply(dataset, is.numeric)
numeric_data <- dataset[, numeric_cols]

cor_matrix <- cor(dataset)

corrplot(cor_matrix, method = "number",
         type = "upper", order = "hclust",
         tl.cex = 0.8, tl.col = "black",
         diag = FALSE)

rm("numeric_data")


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

