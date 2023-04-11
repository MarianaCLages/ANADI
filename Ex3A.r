############################## 3 ################################

# Primeiro efetua-se a definicao do caminho em que se encontra o nosso data set, 
# que contém os dados de 99 viaturas escolhidas aleatoriamente
setwd("C:/Users/franc/Desktop/Pratico1/")

# Importação do ficheiro que contém os dados
dados <- read.csv("DADOS3.csv",)

# Conversão de  todos os campos para valores numéricos, de forma a evitar conflitos
dados$Acceleration <- as.numeric(dados$Acceleration)
dados$Cylinders <- as.numeric(dados$Cylinders)
dados$Weight <- as.numeric(dados$Weight)
dados$Horsepower <- as.numeric(dados$Horsepower)

# Separação dos dados para Data separados
dados4cyl <- dados[dados$Cylinders == 4, ]$Acceleration 
dados6cyl <- dados[dados$Cylinders == 6, ]$Acceleration
dados8cyl <- dados[dados$Cylinders == 8, ]$Acceleration
