# 1. 

# Definição do caminho em que se encontra o script
setwd("C:/Users/franc/Documents/Repositórios/anadi23/Script exercícios/2TP/")

# Importação dos dados
dataset <- read.csv("ciclismo.csv", header = TRUE)

# Verificação da dimensão do dataset
dimensao <- dim(dataset)

# Sumário estatístico dos dados
summary(dataset)


# 2.

# Conversão da data de nascimento para um tipo de Data no R
dataset$dob <- as.Date(dataset$dob)

# Cálculo da idade. Uso de 365.25 pelos anos bissextos
dataset$Age <- as.integer(Sys.Date() - dataset$dob) / 365.25
idades <- dataset$Age

# Print para a consola das idades
print(idades)