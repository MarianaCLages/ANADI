#install.packages("caret")
library(caret)
library(ggplot2)
library(corrplot)

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

# Print para a consola das idades
print(age)


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


# 4.A) Não aplicável, creio

# Identificação dos valores em falta
missing_values <- is.na(dataset)

# Contar o número de valores em falta para cada coluna
missing_count <- colSums(missing_values)

# Imprimir a contagem de valores em falta
print(missing_count)

# Remoção de linhas com valores em falta
clean_dataset <- na.omit(dataset)

# Dimensão do "cleared" dataset
print(dim(clean_dataset))


# 4.B) Analyzing the boxplot, it is possible to verify outliers on the altitude, vo2, hr


# 4.C) 

# Manter as colunas desejadas
dataset <- dataset[, c("gender", "Winter.Training.Camp", "altitude_results", "vo2_results", "hr_results")]

# Adicionar a idade previamente calculada
dataset <- cbind(dataset, age)


# 4.D)

numeric_cols <- sapply(dataset, is.numeric)
numeric_data <- dataset[, numeric_cols]

# Min-Max Scaling entre [0,1]
scaled_data <- as.data.frame(lapply(numeric_data, function(x) {
  (x - min(x)) / (max(x) - min(x))
}))

# Atribuição dos valores para o dataset original
dataset[, numeric_cols] <- scaled_data


# 5.

########################## ONE-HOT ENCODING ####################################

# Identificação de variáveis não numéricas
categorical_vars <- sapply(dataset, is.character)

# Especificação das variáveis para fazer encoding
variables <- colnames(dataset)[categorical_vars]

# Aplicar o one-hot encoding
encoded_data <- predict(dummyVars("~.", data = dataset[, variables]), newdata = dataset)

################################################################################


numeric_cols <- sapply(dataset, is.numeric)
numeric_data <- dataset[, numeric_cols]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data)

# Create a correlation plot with numeric values
corrplot(cor_matrix, method = "number",
         type = "upper", order = "hclust",
         tl.cex = 0.8, tl.col = "black",
         diag = FALSE)


