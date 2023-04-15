#Trabalho Prático 1

#1.
#a)
# Importar dados do ficheiro DADOS1.csv
getwd()
setwd("C:/Users/maria/Desktop/ISEP/3ºano/2ºsemestre/ANADI/Trabalho Prático 1")
dados1 <- read.csv("DADOS1.csv", header = TRUE, skip = 2)

# Converter segundos em POSIXct
dados1$Tempo <- as.POSIXct(dados1$s, origin = "1970-01-01", tz = "GMT")

# Verificar se a nova coluna "Tempo" foi adicionada corretamente
head(dados1) #ou head(dados1$Tempo)


#b)
# Package necessário para filtrar dados
library(dplyr)

# Filtrar os dados para o dia 4 de agosto de 2013
dados_filtrados_tempo <- dados1 %>%
  filter(Tempo >= as.POSIXct("2013-08-04 00:00:00", tz = "GMT") &
           Tempo <= as.POSIXct("2013-08-04 23:59:59", tz = "GMT"))

# Filtrar os dados para as temperaturas do motor de cada bomba
#K.1 -> Medições da temperatura do motor na bomba 1
#K.3 -> Medições da temperatura do motor na bomba 2
#K.5 -> Medições da temperatura do motor na bomba 3
dados_filtrados_temperatura <- dados_filtrados_tempo %>%
  select(Tempo, K.1, K.3, K.5)

# Package necessário para criar o gráfico
library(ggplot2)
ggplot(dados_filtrados_temperatura, aes(x = Tempo)) +
  geom_line(aes(y = K.1, color = "Bomba 1")) +
  geom_line(aes(y = K.3, color = "Bomba 2")) +
  geom_line(aes(y = K.5, color = "Bomba 3")) +
  labs(title = "Temperatura do Motor",
       x = "Tempo",
       y = "Temperatura",
       color = "Bomba") +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme_bw()


#c)
# Criar o boxplot com as três bombas
boxplot(dados_filtrados_temperatura[,2:4], # selecionar as colunas com as temperaturas
        names = c("Bomba 1", "Bomba 2", "Bomba 3"), # nomes das bombas
        xlab = "Bomba", ylab = "Temperatura", # rótulos dos eixos
        main = "Temperatura do motor de cada bomba")


#d)
#i.
# Package necessário para filtrar dados
library(dplyr)

# Filtrar os dados para o mês de março de 2014
dados_filtrados_tempo2 <- dados1 %>%
  filter(Tempo >= as.POSIXct("2014-03-01 00:00:00", tz = "GMT") &
           Tempo <= as.POSIXct("2014-03-31 23:59:59", tz = "GMT"))

# Filtrar os dados para as medições de oil rate de cada bomba
# bbl.d.2 -> Medições do oil rate da bomba 1
# bbl.d.5 -> Medições do oil rate da bomba 2
dados_filtrados_oil_rate <- dados_filtrados_tempo2 %>%
  select(Tempo, bbl.d.2, bbl.d.5)

colnames(dados_filtrados_oil_rate)[2:3]  <- c("Oil rate (Bomba 1)","Oil rate (Bomba 2)")

# Calcular a média diária para cada bomba (quantidade de barris produzida)
medias_diarias <- dados_filtrados_oil_rate %>%
  group_by(as.Date(Tempo)) %>%
  summarise(medias_diarias_bomba1 = mean(`Oil rate (Bomba 1)`),
            medias_diarias_bomba2 = mean(`Oil rate (Bomba 2)`))

colnames(medias_diarias)[1]  <- "Dia"

# Packages necessários para unir colunas de tabela (tidyr) e criar o gráfico de barras (ggplot2)
library(tidyr)
library(ggplot2)

# Unir as colunas "medias_diarias_bomba1" e "medias_diarias_bomba2"
medias_diarias <- medias_diarias %>% 
  gather(key = "bomba", value = "medias_diarias_bombas", -Dia)

# Criar o gráfico de barras
ggplot(medias_diarias, aes(x = Dia, y = medias_diarias_bombas, fill = bomba)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(title = "Barris de petróleo produzidos diariamente no mês de março de 2014",
       x = "Dia",
       y = "Nº de barris de petróleo produzidos (média diária)",
       fill = "Bomba") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d") +
  scale_fill_manual(values = c("coral1", "cadetblue"),
                    labels = c("Bomba 1", "Bomba 2")) +
  theme_bw()


#ii.
# Package necessário para filtrar dados
library(dplyr)

# Filtrar os dados para o período especificado
dados_filtrados_tempo3 <- dados1 %>%
  filter(Tempo >= as.POSIXct("2013-06-01 00:00:00", tz = "GMT") &
           Tempo <= as.POSIXct("2014-05-31 23:59:59", tz = "GMT"))

# Calcular a média mensal de extração de barris de petróleo da bomba 1
medias_mensais_bomba1 <- dados_filtrados_tempo3 %>%
  mutate(Mes = format(Tempo, "%m-%Y")) %>%
  group_by(Mes) %>%
  summarise(Media = mean(bbl.d.2))

# Encontrar o mês em que a extração foi a maior e armazenar na variável "mes_maior_valor"
mes_maior_valor <- medias_mensais_bomba1 %>%
  slice(which.max(medias_mensais_bomba1$Media)) %>%
  pull(Mes) ; mes_maior_valor

# O mês em que a bomba 1 extraiu mais barris de petróleo foi em agosto de 2013 (08-2013).


#iii.
# Package necessário para filtrar dados
library(dplyr)

# Filtrar os dados para o período especificado
dados_filtrados_tempo4 <- dados1 %>%
  filter(Tempo >= as.POSIXct("2013-06-01 00:00:00", tz = "GMT") &
           Tempo <= as.POSIXct("2014-05-31 23:59:59", tz = "GMT")) %>%
  select(Tempo, bbl.d.2, bbl.d.5)

colnames(dados_filtrados_tempo4)[2:3] <- c("Bomba 1", "Bomba 2")

# Converter os números de dias em datas
data_inicial <- as.Date("2013-06-01")
amostra_dias <- c(78, 362, 66, 156, 277, 41, 339, 169, 272, 268)
amostra_datas <- data_inicial + amostra_dias - 1 ; amostra_datas

# Datas correspondentes da amostra aleatória (amostra_datas)
# "2013-08-17" "2014-05-28" "2013-08-05" "2013-11-03" "2014-03-04" "2013-07-11" "2014-05-05"
# "2013-11-16" "2014-02-27" "2014-02-23"

producao_amostra <- dados_filtrados_tempo4 %>%
  filter(as.Date(Tempo) %in% amostra_datas) %>%
  group_by(as.Date(Tempo)) %>%
  summarize(`Bomba 1` = mean(`Bomba 1`), `Bomba 2` = mean(`Bomba 2`))

# A data da amostra "2014-02-23" não tem dados disponíveis


boxplot(producao_amostra[, 2:3], 
        col = c("coral", "lightblue"), 
        names = c("Bomba 1", "Bomba 2"), 
        xlab = "Bomba", 
        ylab = "Produção diária",
        main = "Produção diária por bomba")


#iv.
# H0: média da produção diária de petróleo da bomba 1 <= média da produção diária de petróleo da bomba 2
# H1: média da produção diária de petróleo da bomba 1 > média da produção diária de petróleo da bomba 2

# Teste t para 2 amostras emparelhadas
t.test(producao_amostra$`Bomba 1`, producao_amostra$`Bomba 2`, alternative = "greater", paired = TRUE)

# Como p = 2.17e-05 < alfa (0.05), rejeita-se H0.
# Há evidências estatísticas para afirmar que a média da produção diária de petróleo da bomba 1 é maior do que a da bomba 2 no período de 1-6-2013 e 31-5-2014, com 5% de significância.


#v.
# Calcular as médias amostrais
media_bomba1 <- mean(producao_amostra$`Bomba 1`)
media_bomba2 <- mean(producao_amostra$`Bomba 2`)

# Comparando as médias amostrais
diferenca <- media_bomba1 - media_bomba2
if (diferenca > 0) {
  cat("A média da produção diária de petróleo da bomba 1 é maior do que a da bomba 2.")
} else if (diferenca < 0) {
  cat("A média da produção diária de petróleo da bomba 2 é maior do que a da bomba 1.")
} else {
  cat("As médias são iguais.")
}

###################################################################################################################################################

#Exercício 2

# Primeiro efetua-se a definicao do caminho em que se encontra o nosso data set
setwd("C:/Users/MiguelJordão(1201487/Desktop/ANADI/anadi23") 

#Ler os dados do CSV.file
dadosCSV <- read.csv("Dados utilizados/DADOS2.csv")

# Selecionar as colunas correspondentes às precisões de cada algoritmo
precisoes <- dadosCSV[, 3:8]

# Calcular a matriz de correlações
correlacoes <- cor(precisoes)

#Imprimir os valores da matriz
print(correlacoes)

#          SVM        DT        KN        RF        ML        GB
#SVM 1.0000000 0.2619970 0.6374486 0.4659169 0.7111270 0.8629016
#DT  0.2619970 1.0000000 0.4339395 0.8802559 0.6247459 0.2127059
#KN  0.6374486 0.4339395 1.0000000 0.4834416 0.8539377 0.7502013
#RF  0.4659169 0.8802559 0.4834416 1.0000000 0.5719541 0.3240401
#ML  0.7111270 0.6247459 0.8539377 0.5719541 1.0000000 0.7211135
#GB  0.8629016 0.2127059 0.7502013 0.3240401 0.7211135 1.0000000

# 0.8802559 - maior relação entre DT e RF
# 0.2127059 - menor relação entre DT e GB
# restantes valores variam entre 0.2619970 e 0.8629016

#Utilizamos a função "cor" que é mais correta para estudar a comparação entre pares de variáveis
#Pois poderia ter sido utilizado o "rcorr" (que se baseava em spearman e pearson, mas ao analisarmos a população de dados deduzimos que não é possível utilizar nenhum dos dois)

#b)

# Para podermos efetuar afirmações sobre uma determinada população de dados, recorremos
# a testes paramétricos. Para tal, temos de efetuar os seguintes passos:

# Primeiro, formulamos a nossa hipótese de testes:
#H0: Existem diferenças significativas entre a precisão dos diferentes algoritmos
#H1: Não existem diferenças significativas entre a precisão dos diferentes algoritmos

# Em segundo, a variável dependente (precisão) é contínua?
# Sim.

# Em terceiro, a variável independente (algoritmos) tem dois ou mais grupos de algoritmos?
# Sim, existem 6 algoritmos.

# Em quarto, as observações são independentes dado que, de acordo com as precisões dos algoritmos estas não estão diretamente interligadas

# Em quinto, as observações tem outliers significativos?
# Para tal criamos um boxplot, no qual verificamos se existem outliers fora dos bigodes.

preciSVM <- precisoes$SVM 
preciDT <- precisoes$DT 
preciKN <- precisoes$KN 
preciRF <- precisoes$RF  
preciML <- precisoes$ML 
preciGB <- precisoes$GB 

numPrecisoes=c("SVM","DT","KN","RF","ML","GB")

dadosAgrupados <- list(preciSVM, preciDT, preciKN, preciRF, preciML, preciGB)

boxplot(dadosAgrupados,
        names= numPrecisoes,main = 'Precisão de cada algoritmo', xlab = 'Algoritmos', ylab = 'Precisão')

# Após analisar o boxplot, foi possível confirmar que, de facto, existem outliers significativos,
# dado que a aceleração é uma variável contínua proveniente de uma medição.
#
# Contudo, será feita também a verificação através do cálculo dos valores dos limites inferiores e superiores
# Para tal, será necessário averiguar se existem valores fora da amplitude dos bigodes.
# Consequentemente, estes valores terão de ser calculados:
#
iqrSVMcyl <- IQR(preciSVM)
iqrDTcyl <- IQR(preciDT)
iqrKNcyl <- IQR(preciKN)
iqrRFcyl <- IQR(preciRF)
iqrMLcyl <- IQR(preciML)
iqrGBcyl <- IQR(preciGB)

medianSVMcyl <- median(preciSVM)
medianDTcyl <- median(preciDT)
medianKNcyl <- median(preciKN)
medianRFcyl <- median(preciRF)
medianMLcyl <- median(preciML)
medianGBcyl <- median(preciGB)

liminfSVM <- quantile(preciSVM, 0.25) - 1.5 * iqrSVMcyl
limsupSVM <- quantile(preciSVM, 0.75) + 1.5 * iqrSVMcyl

liminfDT <- quantile(preciDT, 0.25) - 1.5 * iqrDTcyl
limsupDT <- quantile(preciDT, 0.75) + 1.5 * iqrDTcyl

liminfKN <- quantile(preciKN, 0.25) - 1.5 * iqrKNcyl
limsupKN  <- quantile(preciKN, 0.75) + 1.5 * iqrKNcyl

liminfRF <- quantile(preciRF, 0.25) - 1.5 * iqrRFcyl
limsupRF <- quantile(preciRF, 0.75) + 1.5 * iqrRFcyl

liminfML <- quantile(preciML, 0.25) - 1.5 * iqrMLcyl
limsupML <- quantile(preciML, 0.75) + 1.5 * iqrMLcyl

liminfGB <- quantile(preciGB, 0.25) - 1.5 * iqrGBcyl
limsupGB  <- quantile(preciGB, 0.75) + 1.5 * iqrGBcyl

outliersSVMcyl <- preciSVM[preciSVM > limsupSVM | preciSVM < liminfSVM]

outliersDTcyl <- preciDT[preciDT > limsupDT | preciDT < liminfDT]

outliersKNcyl <- preciKN[preciKN > limsupKN | preciKN < liminfKN]

outliersRFcyl <- preciRF[preciRF > limsupRF | preciRF < liminfRF]

outliersMLcyl <- preciML[preciML > limsupML | preciML < liminfML]

outliersGBcyl <- preciGB[preciGB > limsupGB | preciGB < liminfGB]
#
# Em sexto, após averiguarmos que, de facto, existem outliers, não será possível usar testes ONE-WAY ANOVA, 
# vamos também averiguar que nao existe homogenidade de variâncias.

# Para realizarmos essa afirmação, usamos testes de Shapiro.
#
shapiro.test(precisoes$SVM)
shapiro.test(precisoes$DT)
shapiro.test(precisoes$KN)
shapiro.test(precisoes$RF)
shapiro.test(precisoes$ML)
shapiro.test(precisoes$GB)

#
# Para os algoritmos SVM, DT,KN,RF e GB, a homogenidade é verificada, visto que
# o nível de significância é superior a 0,05.
#
# Contudo, para o algoritmo ML, o mesmo não se veifica, visto que é menor que 0,05.
# Podemos então constantar que não existe homogenidade dos dados.
#
# Assim, podemos concluir que, como uma ou mais hipóteses falham, a variável dependente não é normalmente distribuída.
# Recorremos então a testes não paramétricos, visto que o uso de ANOVA é impossibilitado.

# Outra maneira de verificar isto seria a partir de lillie tests ou mesmo histogramas

hist(precisoes$SVM)
hist(precisoes$DT)
hist(precisoes$KN)
hist(precisoes$RF)
hist(precisoes$ML)
hist(precisoes$GB)

# Para conseguirmos então verificar qual das hipóteses se encontra correta, será necessário
# usar um teste de Kruskal-Wallis.

#Após a visualização dos dados foi possível deduzir que não se trata de uma distribuição normal
#então não podemos aplicar um teste paramétrico, mas sim testes não paramétricos

# Assim, podemos concluir que, como uma ou mais hipóteses falham, a variável dependente não é normalmente distribuída.
# Recorremos então a testes não paramétricos, visto que o uso de ANOVA é impossibilitado.

# Para conseguirmos então verificar qual das hipóteses se encontra correta, será necessário
# usar um teste de Kruskal-Wallis.

#Vamos aplicar um estudo baseado num teste kruskal

kruskal.test(precisoes)
# Como p-value = 0.3335 > 0.05 = alfa, não se rejeita H0.

# Não há evidências estatísticas para afirmar que existem diferenças significativas
# entre a precisão dos diferentes algoritmos.


#c)
# Como foi inferido na alínea anterior não existem evidências estatísticas que afirmem que existam diferenças significativas
# logo não é possível realizar um estudo post-hoc ao teste efetuado previamente

# Caso fosse possível uma alternativa válida seria o uso to TuskeyHSD, mas, neste caso, não é!

###################################################################################################################################################

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
median4cyl <- median(dados4cyl)
median6cyl <- median(dados6cyl)
median8cyl <- median(dados8cyl)
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

dados_numericos <- c(dados4cyl,dados6cyl,dados8cyl)
grupos <- factor (c(rep("4 Cilindros",length(dados4cylComp$Cylinders)), 
                    rep("6 Cilindros",length(dados6cylComp$Cylinders)),
                    rep("8 Cilindros",length(dados8cylComp$Cylinders))))

kruskal.test(dados_numericos, grupos) 
#
# Observando que  p-value = 2.795e-11, inferior a 0.05, rejeitamos a hipótese h0 e 
# concluímos que existem diferenças significativas entre os grupos de cilindros.

# Com os valores obtidos, podemos atingir a conclusão de que, comparando os 3 grupos, as viaturas
# com 4 clindros têm a maior aceleração, com as de 6 cilindros no ponto intermédio e, por último, as
# de 8 cilindros com o menor valor de aceleração.


############################## 3Bi ################################

# Através do uso da regressão linear, averiguaremos se é possível relacionar influências nos valores da aceleração com os restantes dados.

# Fazemos a definição das variáveis que vamos usar.
weight <- dados$Weight
horsepower <- dados$Horsepower
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


