#install.packages("caret")
library(caret)

# 1. 

# Definição do caminho em que se encontra o script
setwd("C:/Users/franc/Documents/Repositórios/anadi23/Script exercícios/2TP")

#"/Users/fredol/Documents/isep/anadi23/Script exercícios/2TP"
#"C:/Users/franc/Documents/Repositórios/anadi23/Script exercícios/2TP"

# Importação dos dados
dataset <- read.csv("ciclismo.csv", header = TRUE, stringsAsFactors = FALSE)

# Verificação da dimensão do datasets
dimensao <- dim(dataset)

# Sumário estatístico dos dados
summary(dataset)


# 2.

# Conversão da data de nascimento para um tipo de Data no R
dataset$dob <- as.Date(dataset$dob)

# Cálculo da idade. Uso de 365.25 pelos anos bissextos
dataset$Age <- floor(as.integer(Sys.Date() - dataset$dob) / 365.25)
age <- dataset$Age

# Print para a consola das idades
print(age)


# 3. 

# Identificação de variáveis não numéricas
categorical_vars <- sapply(dataset, is.character)

# Especificação das variáveis para fazer encoding
variables <- colnames(dataset)[categorical_vars]

# Aplicar o one-hot encoding
encoded_data <- predict(dummyVars("~.", data = dataset[, variables]), newdata = dataset)


boxplot()
cor_matrix <- cor(dataset)






# 4.A)

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






# 4.B)

iqr4cyl <- IQR(dados4cyl)
median4cyl <- median(dados4cyl)
liminf4 <- quantile(dados4cyl, 0.25) - 1.5 * iqr4cyl
limsup4 <- quantile(dados4cyl, 0.75) + 1.5 * iqr4cyl

