#Trabalho Prático 2

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
setwd("C:/Users/franc/Documents/Repositórios/anadi23/Script exercícios/2TP")

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

# Cálculo da idade. Uso de 365.25 pelos anos bissextos a cada 4
dataset$Age <- floor(as.integer(Sys.Date() - dataset$dob) / 365.25)
age <- dataset$Age


# 3. 

# Seleção de atributos numericos
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


# 4.A) Não aplicável, não há valores NA em qualquer coluna. clean_dataset e dataset são iguais

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


# 4.B) Analyzing the boxplot, it is possible to verify outliers on the altitude, vo2, hr


# 4.C) 

# Manter as colunas desejadas
dataset <- dataset[, c("gender", "Winter.Training.Camp", "altitude_results", "vo2_results", "hr_results")]

# Adicionar a idade previamente calculada
dataset <- cbind(dataset, age)


# 4.D)

########################## ONE-HOT ENCODING ####################################

# Identificação de variáveis não numéricas
categorical_vars <- sapply(dataset, is.character)

# Especificação das variáveis para fazer encoding
variables <- colnames(dataset)[categorical_vars]

# Aplicar o one-hot encoding
encoded_data <- predict(dummyVars("~.", data = dataset[, variables]), newdata = dataset)

################################################################################



########################## MIN-MAX Normalization ####################################

# Remover as colunas de gender e Training Camp, visto que vão ser adicionadas de seguida
dataset <- dataset[, c("altitude_results", "vo2_results", "hr_results", "age")]

numeric_cols <- sapply(dataset, is.numeric)
numeric_data <- dataset[, numeric_cols]

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

cor_matrix <- cor(numeric_data)


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
slr.model # Equação: 0.03951 + 0.84440*hr_results

#b)
# Criar o gráfico de dispersão e a reta de regressão
ggplot(slr.model, aes(hr_results, altitude_results)) +
  geom_point() + stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = hr_results, yend= .fitted), color = "red")

#c)
slr.pred = predict(slr.model, dataset.test) ; slr.pred

dif = dataset.test$altitude_results - slr.pred ; dif

#MAE
slr.mae = mean(abs(dif))
cat("mae : ", slr.mae)
# mae : 0.08948653

# RMSE
slr.rmse = sqrt(mean(dif^2))
cat("rmse : ", slr.rmse)
# rmse : 0.1101757

#d) ?? - Sem certeza
complex.model <- lm(altitude_results ~ ., data = dataset.train) ; complex.model


#7.
#a)
mlr.model = lm(vo2_results ~ altitude_results + hr_results, data = dataset.train) ; mlr.model

# eq. vo2_results = 0.0220 + 0.1185*altitude_results + 0.8490*hr_results
summary(mlr.model)

# Criar o gráfico de dispersão e a reta de regressão
#ggplot(slr.model, aes(altitude_results, hr_results, vo2_results)) +
#  geom_point() + stat_smooth(method = lm, se = FALSE) +
#  geom_segment(aes(xend = hr_results, yend= .fitted), color = "red")

mlr.pred = predict(mlr.model, dataset.test) ; mlr.pred

dif = dataset.test$vo2_results - mlr.pred ; dif

#MAE
mlr.mae = mean(abs(dif))
cat("mae : ", mlr.mae)
# mae : 0.04399539

# RMSE
mlr.rmse = sqrt(mean(dif^2))
cat("rmse : ", mlr.rmse)
# rmse : 0.05515508

#b)
tree.model = rpart(vo2_results ~ ., method = "anova", data = dataset.train) ; tree.model
rpart.plot(tree.model)

#c)
#---------------------
# 1 internal node
numnodes <- 1

#the formula must be specific (A DAR ERRO)
nn.model <- 
  neuralnet(
    vo2_results ~ gender + Winter.Training.Camp + altitude_results + hr_results + age,
    data = dataset.train,
    hidden = numnodes
  )
plot(nn.model)

nn.model$result.matrix
