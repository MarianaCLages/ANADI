############################## 3 ################################

# Primeiro efetua-se a definicao do caminho em que se encontra o nosso data set, 
# que contém os dados de 99 viaturas escolhidas aleatoriamente.
setwd("C:/Users/franc/OneDrive - Instituto Superior de Engenharia do Porto/Desktop/3 ANO/ANADI/Pratico1/")

# Importação do ficheiro que contém os dados
dados <- read.csv("DADOS3.csv",)

# Conversão de  todos os campos para valores numéricos, de forma a evitar conflitos.
dados$Acceleration <- as.numeric(dados$Acceleration)
dados$Cylinders <- as.numeric(dados$Cylinders)
dados$Weight <- as.numeric(dados$Weight)
dados$Horsepower <- as.numeric(dados$Horsepower)

# Separação dos dados para Data separados.
dados4cyl <- dados[dados$Cylinders == 4, ]$Acceleration 
dados6cyl <- dados[dados$Cylinders == 6, ]$Acceleration
dados8cyl <- dados[dados$Cylinders == 8, ]$Acceleration


############################## 3A ################################

# Para podermos efetuar afirmações sobre uma determinada população de dados, recorremos
# a testes paramétricos. Para tal, temos de efetuar os seguintes passos:

# Primeiro, formulamos a nossa hipótese de testes:
# H0: Não existem diferenças significativas na aceleração entre os diferentes grupos.
# H1: Existem diferenças significativas na aceleração entre os diferentes grupos.

# Em segundo, a variável dependente (aceleração) é contínua?
# Sim.

# Em terceiro, a variável independente (nr de cilindros) tem dois ou mais grupos de cilindros?
# Sim, 4, 6 e 8 Cilindros.

# Em quarto, as observações são independentes dado que, de acordo com os dados fornecidos, apenas
# o número de cilindros afeta os valores da aceleração.

# Em quinto, as observações tem outliers significativos?
# Para tal criamos um boxplot, no qual verificamos se existem outliers fora dos bigodes.

numCilindros=c("4 Cilindros","6 Cilindros","8 Cilindros")

dadosAgrupados <- list(dados4cyl, dados6cyl, dados8cyl)

boxplot(dadosAgrupados,
        names= numCilindros,main = 'Aceleração por cada grupo de cilindros', xlab = 'Cilindros', ylab = 'Aceleração')

# Após analisar o boxplot, foi possível confirmar que, de facto, existem outliers significativos,
# dado que a aceleração é uma variável contínua proveniente de uma medição.
#
# Contudo, será feita também a verificação através do cálculo dos valores dos limites inferiores e superiores
# Para tal, será necessário averiguar se existem valores fora da amplitude dos bigodes.
# Consequentemente, estes valores terão de ser calculados:
#
iqr4cyl <- IQR(dados4cyl)
iqr6cyl <- IQR(dados6cyl)
iqr8cyl <- IQR(dados8cyl)
liminf4 <- quantile(dados4cyl, 0.25) - 1.5 * iqr4cyl
limsup4 <- quantile(dados4cyl, 0.75) + 1.5 * iqr4cyl
liminf6 <- quantile(dados6cyl, 0.25) - 1.5 * iqr6cyl
limsup6 <- quantile(dados6cyl, 0.75) + 1.5 * iqr6cyl
liminf8 <- quantile(dados8cyl, 0.25) - 1.5 * iqr8cyl
limsup8 <- quantile(dados8cyl, 0.75) + 1.5 * iqr8cyl
outliers4cyl <- dados4cyl[dados4cyl > limsup4 | dados4cyl < liminf4]
outliers6cyl <- dados6cyl[dados6cyl > limsup6 | dados6cyl < liminf6]
outliers8cyl <- dados8cyl[dados8cyl > limsup8 | dados8cyl < liminf8]
#
# Em sexto, após averiguarmos que, de facto, existem outliers, não será possível usar testes ONE-WAY ANOVA, 
# vamos também averiguar que nao existe homogenidade de variâncias.

# Para realizarmos essa afirmação, usamos testes de Shapiro.
#
shapiro.test(dados4cyl)
shapiro.test(dados6cyl)
shapiro.test(dados8cyl)
#
# Para os motores de 4 e 8 cilindros, a homogenidade é verificada, visto que
# o nível de significância é superior a 0,05.
#
# Contudo, para os motores de 6 cilindros, o mesmo não se veifica, visto que é menor que 0,05.
# Podemos então constantar que não existe homogenidade dos dados.
#
# Assim, podemos concluir que, como uma ou mais hipóteses falham, a variável dependente não é normalmente distribuída.
# Recorremos então a testes não paramétricos, visto que o uso de ANOVA é impossibilitado.


# Para conseguirmos então verificar qual das hipóteses se encontra correta, será necessário
# usar um teste de Kruskal-Wallis.
#
#

dados4cylComp <- dados[dados$Cylinders == 4, ] 
dados6cylComp <- dados[dados$Cylinders == 6, ]
dados8cylComp <- dados[dados$Cylinders == 8, ]


dados_numericos <- c(dados4cylComp,dados6cylComp,dados8cylComp)
grupos <- factor (c(rep("4 Cilindros",length(dados4cylComp$Cylinders)), 
                    rep("6 Cilindros",length(dados6cylComp$Cylinders)),
                    rep("8 Cilindros",length(dados8cylComp$Cylinders))))

kruskal.test(dados$Acceleration, grupos)
#
# Observando que  p-value = 0.0006377, inferior a 0.05, rejeitamos a hipótese h0 e 
# concluímos que existem diferenças significativas entre os grupos de cilindros.

# Com os valores obtidos, podemos atingir a conclusão de que, comparando os 3 grupos, as viaturas
# com 4 clindros têm a maior aceleração, com as de 6 cilindros no ponto intermédio e, por último, as
# de 8 cilindros com o menor valor de aceleração.


############################## 3Bi ################################

# Através do uso da regressão linear, averiguaremos se é possível relacionar influências nos valores da aceleração com os restantes dados.

# Fazemos a definição das variáveis que vamos usar.
weight <- dados$Weight
horsepower <- dados$Horsepower
acceleration <- dados$Acceleration
cylinders <- factor(dados$Cylinders)


# Ajuste do modelo de Regressão linear.
modelo <- lm(Acceleration ~ weight + horsepower + cylinders, data = dados)
summary(modelo)

# Visualização dos resultados.
plot(modelo)



############################## 3Bii ################################

# Criar um data frame com as variáveis independentes para um carro com 4 cilindros, peso 2950 e potência 100.
novos_dados <- data.frame(
  cylinders = factor(4),
  weight = 2950,
  horsepower = 100
)

# Estimar a aceleração usando o modelo de regressão linear, usando os dados fornecidos.
predict(modelo, newdata = novos_dados)

